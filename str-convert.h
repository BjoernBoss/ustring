#pragma once

#include "str-common.h"

#include <string>
#include <concepts>
#include <utility>
#include <cwchar>
#include <type_traits>

/*
*	Decodes characters to codepoints (aka char32_t)
*	Encodes codepoints (aka char32_t) to any character
*
*	Performs decoding/encoding validations when operating between different encodings but only
*	perform character-length estimations/raw copies, when operating on the same encoding.
*
*	Valid Unicode-codepoints are all within the unicode range and not a surrogate-pair reserved value.
*
*	All encoded codepoints will be presented to the sink in a single call in order to prevent partial writings for half-full sinks.
*/
namespace str {
	/*
	*	Success: returned for all non-codepoint returning but succeeding operations (implicitly also valid)
	*	Empty: Source was empty and therefore nothing decoded
	*	Invalid: Encoding error was detected in source or encoded codepoint is invalid or did not use the proper encoding-form
	*	Incomplete: Codepoint seems to be encoded properly, but missing one or more characters for completion, no characters will be consumed
	*		=> if source is completed, return value will never be incomplete and at least one character will be consumed
	*	NotAscii: Read-ascii encountered a non-ascii character
	*	WriteFailed: Transcoding a character decoded it successfully from the source but could not encode it to the destination
	*/
	static constexpr char32_t CPSuccess = char32_t(0);
	static constexpr char32_t CPEmpty = char32_t(-1);
	static constexpr char32_t CPInvalid = char32_t(-2);
	static constexpr char32_t CPIncomplete = char32_t(-3);
	static constexpr char32_t CPNotAscii = char32_t(-4);
	static constexpr char32_t CPWriteFailed = char32_t(-5);

	/* check if the codepoint is CPSuccess, or none of the other defined errors (does not check if the codepoint itself lies in a valid unicode-range) */
	inline constexpr bool IsCPValid(char32_t cp) {
		/* char32_t is guaranteed to be unsigned */
		return (cp < std::min<char32_t>({ str::CPEmpty, str::CPInvalid, str::CPIncomplete, str::CPNotAscii, str::CPWriteFailed }));
	}
	struct CPOut {
		char32_t cp = str::CPEmpty;
		uint32_t consumed = 0;
	};

	/* maximum number of characters to potentially be consumed/produced per codepoint */
	static constexpr size_t MaxEncodeLength = 4;

	/* number of ascii characters with valid range: [0, 127] */
	static constexpr size_t AsciiRange = 128;

	/* number of unicode characters lie within range: [0, 0x10ffff] */
	static constexpr size_t UnicodeRange = 0x110000;

	/* default error character */
	static constexpr char CharOnError = '?';

	namespace detail {
		/* extract effective character (char/char8_t/char16_t/char32_t) */
		template <class Type>
		using EffChar = std::conditional_t<std::is_same_v<Type, char>, std::conditional_t<str::IsCharUtf8, char8_t, char>,
			std::conditional_t<std::is_same_v<Type, wchar_t>, std::conditional_t<str::IsWideUtf16, char16_t, char32_t>, Type>>;

		/* utf-8 help lookup maps (of the upper 5 bits) and boundary maps (for length) */
		static constexpr uint8_t Utf8InitCharLength[32] = {
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
			0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
		};
		static constexpr uint8_t Utf8InitCharCodeBits[32] = {
			0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1f, 0x1f, 0x1f, 0x1f, 0x0f, 0x0f, 0x07, 0x00
		};

		/* utf-16 help variables */
		static constexpr uint16_t Utf16LowerSurrogate = 0xd800;
		static constexpr uint16_t Utf16UpperSurrogate = 0xdc00;
		static constexpr uint16_t Utf16LastSurrogate = 0xdfff;

		inline constexpr str::CPOut ReadUtf8(const char8_t* begin, const char8_t* end) {
			uint8_t c8 = static_cast<uint8_t>(*begin);

			/* lookup the length of the character (with 0 invalid encoding) and extract the initial codepoint bits */
			uint32_t len = detail::Utf8InitCharLength[c8 >> 3];
			if (len == 0)
				return { str::CPInvalid, 1 };
			uint32_t cp = (c8 & detail::Utf8InitCharCodeBits[c8 >> 3]);

			/* check if the codepoint will be overlong */
			if (cp == 0 && len > 1)
				return { str::CPInvalid, len };

			/* validate the buffer has capacity enough */
			if (end - begin < len)
				return { str::CPIncomplete, 0 };

			/* validate the contiunation-bytes and construct the final codepoint */
			for (uint32_t j = 1; j < len; ++j) {
				uint8_t n8 = static_cast<uint8_t>(begin[j]);
				if ((n8 & 0xc0) != 0x80)
					return { str::CPInvalid, j };
				cp = ((cp << 6) | (n8 & 0x3f));
			}

			/* check if its an invalid codepoint (i.e. greater than UnicodeRange or surrogate-pair) */
			if (cp >= str::UnicodeRange || (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16LastSurrogate))
				return { str::CPInvalid, len };
			return { char32_t(cp), len };
		}
		inline constexpr str::CPOut ReadUtf16(const char16_t* begin, const char16_t* end) {
			uint32_t cp = static_cast<uint16_t>(*begin);

			/* check if its a valid single-token codepoint */
			if (cp < detail::Utf16LowerSurrogate || cp > detail::Utf16LastSurrogate)
				return { cp, 1 };

			/* ensure there are enough characters for a surrogate-pair and that the first value is valid */
			if (cp >= detail::Utf16UpperSurrogate)
				return { str::CPInvalid, 1 };
			if (end - begin < 2)
				return { str::CPIncomplete, 0 };

			/* extract and validate the second token */
			uint32_t next = static_cast<uint16_t>(begin[1]);
			if (next < detail::Utf16UpperSurrogate || next > detail::Utf16LastSurrogate)
				return { str::CPInvalid, 2 };

			/* decode the overall codepoint (cannot produce any invalid codepoints) */
			return { char32_t(0x10000 + ((cp & 0x03ff) << 10) | (next & 0x03ff)), 2 };
		}
		inline constexpr str::CPOut ReadUtf32(const char32_t* begin, const char32_t* end) {
			uint32_t cp = static_cast<uint32_t>(*begin);

			/* validate the codepoint */
			if (cp >= str::UnicodeRange || (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16UpperSurrogate))
				return { str::CPInvalid, 1 };
			return { char32_t(cp), 1 };
		}
		template <bool SizeOnly>
		inline str::CPOut ReadMultiByte(const char* begin, const char* end) {
			/* first decode multi-byte to wide-chars (as the standard libraries functions
			*	to utf32 only work for utf8 inputs, at least on some msvc versions) */
			std::mbstate_t state{ 0 };
			wchar_t wc = 0;

			/* read the next character and check if the character is incomplete (documentation suggests that a decoded codepoint should
			*	always fit into single wide-char, and fail if the max encoding length promise would be broken => should not be possible) */
			size_t res = std::mbrtowc((SizeOnly ? nullptr : &wc), begin, size_t(end - begin), &state);
			if (res == static_cast<size_t>(-2)) {
				if (end - begin >= str::MaxEncodeLength)
					return { str::CPInvalid, str::MaxEncodeLength };
				return { str::CPIncomplete, 0 };
			}
			uint32_t len = res;

			/* check if the decoded character is the null-character, which has a length of 1 */
			if (res == 0)
				len = 1;

			/* check if the decoded character is not a valid character, in which case the length is not
			*	returned and has to be figured out by creeping up to the previous response */
			else if (res == static_cast<size_t>(-1)) {
				len = 1;

				/* try to find the length by creeping up to the previous response (as we dont know the length of the error/null-byte) */
				while (true) {
					/* always use a clean state to ensure the previous try does not polute this result */
					state = { 0 };
					res = std::mbrtowc(0, begin, len, &state);

					/* check if the given result has been reached, or the character is still incomplete */
					if (res == static_cast<size_t>(-1))
						break;
					if (res == static_cast<size_t>(-2) && len < (end - begin)) {
						++len;
						continue;
					}

					/* a new error has been received/end of source has been reached, in which case an error in the encoding can
					*	just be returned as this state should really never be reached for a valid and deterministic std::mbrtowc */
					return { str::CPInvalid, std::min<size_t>(len, str::MaxEncodeLength) };
				}
			}

			/* check if the char is longer than the internal max character-length, which should really not happen in any encoding
			*	and would break the promise of str::MaxEncodeLength, or invalid for both of which an error can be returned */
			if (res == static_cast<size_t>(-1) || len > str::MaxEncodeLength)
				return { str::CPInvalid, std::min<size_t>(len, str::MaxEncodeLength) };

			/* check if only the size was required */
			if constexpr (SizeOnly)
				return { str::CPSuccess, len };

			/* check which kind of unicode-encoding needs to be used (consumed can be discarded as it will either be 1 or 0 but an error returned) */
			char32_t cp = 0;
			if constexpr (str::IsWideUtf16)
				cp = detail::ReadUtf16(reinterpret_cast<const char16_t*>(&wc), reinterpret_cast<const char16_t*>(&wc) + 1).cp;
			else
				cp = detail::ReadUtf32(reinterpret_cast<const char32_t*>(&wc), reinterpret_cast<const char32_t*>(&wc) + 1).cp;

			/* check if the decoding worked */
			if (!str::IsCPValid(cp))
				return { str::CPInvalid, len };
			return { cp, len };
		}

		/* expect s to contain at least one character (will only return consumed:0 on incomplete chars) */
		template <class ChType>
		constexpr str::CPOut ReadCodePoint(const ChType* begin, const ChType* end) {
			using EffType = detail::EffChar<ChType>;
			str::CPOut out{};

			/* check for the fast way out by the character being an immediate ascii character */
			if constexpr (str::IsAscii<ChType>) {
				if (std::make_unsigned_t<ChType>(*begin) < str::AsciiRange)
					return { char32_t(*begin), 1 };
			}

			/* decode the next codepoint */
			const EffType* effBegin = reinterpret_cast<const EffType*>(begin);
			const EffType* effEnd = reinterpret_cast<const EffType*>(end);
			if constexpr (std::is_same_v<EffType, char>)
				out = detail::ReadMultiByte<false>(effBegin, effEnd);
			else if constexpr (std::is_same_v<EffType, char8_t>)
				out = detail::ReadUtf8(effBegin, effEnd);
			else if constexpr (std::is_same_v<EffType, char16_t>)
				out = detail::ReadUtf16(effBegin, effEnd);
			else
				out = detail::ReadUtf32(effBegin, effEnd);
			return out;
		}

		template <class ChType>
		constexpr bool WriteUtf8(auto& sink, uint32_t cp) {
			/* check if its a single character, which can just be pushed */
			if (cp <= 0x7f) {
				str::WriteSink<ChType>(sink, static_cast<ChType>(cp));
				return true;
			}

			/* determine the length of the character */
			ChType out[4] = { 0 };
			size_t cont = 0;
			if (cp <= 0x07ff) {
				out[0] = static_cast<ChType>(0xc0 | (cp >> 6));
				cont = 1;
			}
			else if (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16LastSurrogate)
				return false;
			else if (cp <= 0xffff) {
				out[0] = static_cast<ChType>(0xe0 | (cp >> 12));
				cont = 2;
			}
			else if (cp < str::UnicodeRange) {
				out[0] = static_cast<ChType>(0xf0 | (cp >> 18));
				cont = 3;
			}
			else
				return false;
			size_t len = cont + 1;

			/* produce the continuation bytes */
			do {
				out[cont--] = static_cast<ChType>(0x80 | (cp & 0x3f));
				cp >>= 6;
			} while (cont > 0);

			/* write the data to the sink */
			str::WriteSink<ChType>(sink, out, len);
			return true;
		}
		template <class ChType>
		constexpr bool WriteUtf16(auto& sink, uint32_t cp) {
			/* check if its a single utf16-byte */
			if (cp < 0x10000) {
				if (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16UpperSurrogate)
					return false;
				str::WriteSink<ChType>(sink, static_cast<ChType>(cp));
				return true;
			}
			if (cp >= str::UnicodeRange)
				return false;
			cp -= 0x10000;

			/* produce the two utf16-bytes */
			ChType out[2] = { 0 };
			out[0] = static_cast<ChType>(detail::Utf16LowerSurrogate + (cp >> 10));
			out[1] = static_cast<ChType>(detail::Utf16UpperSurrogate + (cp & 0x03ff));

			/* write the data to the sink */
			str::WriteSink<ChType>(sink, out, 2);
			return true;
		}
		template <class ChType>
		constexpr bool WriteUtf32(auto& sink, uint32_t cp) {
			/* validate the codepoint and write it to the sink */
			if (cp >= str::UnicodeRange || (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16UpperSurrogate))
				return false;
			str::WriteSink<ChType>(sink, static_cast<ChType>(cp));
			return true;
		}
		constexpr bool WriteMultiByte(auto& sink, uint32_t cp) {
			str::Small<wchar_t, str::MaxEncodeLength> wc;

			/* convert the codepoint first to wide-characters and check if it only resulted in one, as a single
			*	multi-byte char must originate from exactly 1 wide char (also expected by read-multibyte) */
			if constexpr (str::IsWideUtf16) {
				if (!detail::WriteUtf16<wchar_t>(wc, cp))
					return false;
			}
			else if (!detail::WriteUtf32<wchar_t>(wc, cp))
				return false;
			if (wc.size() != 1)
				return false;

			/* [std::wcrtomb is unsafe, use std::wcrtomb_s instead] */
#pragma warning(push)
#pragma warning(disable : 4996)

			/* try to convert the character to the multi-byte presentation (no small-buffer, as MB_CUR_MAX is not necessarily constant...) */
			std::basic_string<char> buf(MB_CUR_MAX, '\0');
			std::mbstate_t state{ 0 };
			size_t res = std::wcrtomb(&buf[0], wc[0], &state);
#pragma warning(pop)

			/* check if the character could not be converted or if the converted character would break the promise of
			*	the string-conversion max encoding length in which case the codepoint is not considered encodable */
			if (res == static_cast<size_t>(-1) || res > str::MaxEncodeLength)
				return false;

			/* write the characters to the sink (res should never be zero) */
			str::WriteSink<char>(sink, buf.c_str(), res);
			return true;
		}

		template <class ChType>
		constexpr bool WriteCodePoint(auto& sink, char32_t cp) {
			using EffType = detail::EffChar<ChType>;

			/* check for the fast way out by the character being an immediate ascii character */
			if constexpr (str::IsAscii<ChType>) {
				if (static_cast<uint32_t>(cp) < str::AsciiRange) {
					str::WriteSink<ChType>(sink, static_cast<ChType>(cp));
					return true;
				}
			}

			/* encode the codepoint */
			if constexpr (std::is_same_v<EffType, char>)
				return detail::WriteMultiByte(sink, static_cast<uint32_t>(cp));
			else if constexpr (std::is_same_v<EffType, char8_t>)
				return detail::WriteUtf8<ChType>(sink, static_cast<uint32_t>(cp));
			else if constexpr (std::is_same_v<EffType, char16_t>)
				return detail::WriteUtf16<ChType>(sink, static_cast<uint32_t>(cp));
			else
				return detail::WriteUtf32<ChType>(sink, static_cast<uint32_t>(cp));
		}

		/* estimate the size of the next character by inspecting as few bytes as possible (expects s to contain at least one character) */
		template <class ChType>
		constexpr str::CPOut ReadNextSize(const ChType* begin, const ChType* end) {
			using EffType = detail::EffChar<ChType>;
			uint32_t len = 0;

			/* check for the fast way out by the character being an immediate ascii character */
			if constexpr (str::IsAscii<ChType>) {
				if (static_cast<uint32_t>(*begin) < str::AsciiRange)
					return { str::CPSuccess, 1 };
			}

			/* check what encoding it is and read the size (invalid encodings result in length 0) */
			if constexpr (std::is_same_v<EffType, char8_t>)
				len = detail::Utf8InitCharLength[static_cast<uint8_t>(*begin) >> 3];
			else if constexpr (std::is_same_v<EffType, char16_t>) {
				uint16_t c16 = static_cast<uint16_t>(*begin);
				if (c16 < detail::Utf16LowerSurrogate || c16 > detail::Utf16LastSurrogate)
					len = 1;
				else
					len = (c16 < detail::Utf16UpperSurrogate ? 2 : 0);
			}
			else if constexpr (std::is_same_v<EffType, char32_t>)
				len = 1;
			else
				return detail::ReadMultiByte<true>(begin, end);

			/* construct the intermediate result (invalid first-byte encodings are just considered size-1) */
			if (len == 0)
				return { str::CPInvalid, 1 };
			if (len <= uint32_t(end - begin))
				return { str::CPSuccess, len };
			return { str::CPIncomplete, 0 };
		}

		/* if different types, fully decode codepoint, otherwise only estimate the size to copy (expects s to contain at least one character) */
		template <class SourceType, class SinkType>
		constexpr str::CPOut TranscodeNext(auto& sink, const SourceType* begin, const SourceType* end) {
			static constexpr bool EffIdentical = std::is_same_v<detail::EffChar<SourceType>, detail::EffChar<SinkType>>;
			str::CPOut out{};

			/* check if the source and destination are the same type, in which case the length can be extracted
			*	and copied over to the source (codepoint can be set to null, as it will not be used) */
			if constexpr (EffIdentical)
				out = detail::ReadNextSize<SourceType>(begin, end);
			else
				out = detail::ReadCodePoint<SourceType>(begin, end);

			/* check if its incomplete or if an error occurred and return the result */
			if (!str::IsCPValid(out.cp))
				return out;

			/* check if the source and destination are of the same type, in which case the characters can just be copied */
			if constexpr (EffIdentical)
				str::WriteSink<SinkType>(sink, reinterpret_cast<const SinkType*>(begin), out.consumed);

			/* try to write the codepoint to the destination and otherwise return the consumed number of token */
			else if (!detail::WriteCodePoint<SinkType>(sink, out.cp))
				return { str::CPWriteFailed, out.consumed };
			return { str::CPSuccess, out.consumed };
		}
	}

	/* read a single codepoint from the beginning of the string */
	constexpr str::CPOut Decode(const str::AnyString auto& source, bool sourceCompleted) {
		using ChType = str::StringChar<decltype(source)>;

		auto [begin, end] = str::StringIterators<ChType>(source);
		if (begin == end)
			return { str::CPEmpty, 0 };

		str::CPOut out = detail::ReadCodePoint<ChType>(begin, end);
		if (!sourceCompleted || out.cp != str::CPIncomplete)
			return out;
		return { str::CPInvalid, uint32_t(end - begin) };
	}

	/* check if the next character is an ascii character and return it or return str::CPNotAscii with zero characters consumed */
	constexpr str::CPOut ReadAscii(const str::AnyString auto& source) {
		using ChType = str::StringChar<decltype(source)>;

		auto [begin, end] = str::StringIterators<ChType>(source);
		if (begin == end)
			return { str::CPNotAscii, 0 };

		/* check if the raw value itself can be checked for being an ascii character */
		if constexpr (str::IsAscii<ChType>) {
			if (std::make_unsigned_t<ChType>(*begin) < str::AsciiRange)
				return { char32_t(*begin), 1 };
			return { str::CPNotAscii, 0 };
		}

		/* decode the codepoint and check if its a valid ascii character */
		str::CPOut out = detail::ReadCodePoint<ChType>(begin, end);
		if (str::IsCPValid(out.cp) && uint32_t(out.cp) < str::AsciiRange)
			return out;
		return { str::CPNotAscii, 0 };
	}

	/* compute the size of the next character in the source while testing as few as characters as possible (returns str::CPSuccess on success) */
	constexpr str::CPOut LengthNext(const str::AnyString auto& source, bool sourceCompleted) {
		using ChType = str::StringChar<decltype(source)>;

		auto [begin, end] = str::StringIterators<ChType>(source);
		if (begin == end)
			return { str::CPEmpty, 0 };

		str::CPOut out = detail::ReadNextSize<ChType>(begin, end);
		if (!sourceCompleted || out.cp != str::CPIncomplete)
			return out;
		return { str::CPInvalid, uint32_t(end - begin) };
	}

	/* measure the entire length of the source while testing as few as characters as possible */
	struct MeasureOut {
		size_t codepoints = 0;
		size_t invalid = 0;
	};
	constexpr str::MeasureOut Measure(const str::AnyString auto& source) {
		using ChType = str::StringChar<decltype(source)>;
		str::MeasureOut out{};

		auto [begin, end] = str::StringIterators<ChType>(source);

		/* iterate over the string and measure all characters */
		while (begin != end) {
			str::CPOut res = detail::ReadNextSize<ChType>(begin, end);

			if (res.cp == str::CPSuccess)
				++out.codepoints;
			else {
				++out.invalid;
				if (res.cp == str::CPIncomplete)
					break;
			}
			begin += res.consumed;
		}
		return out;
	}

	/* write the single codepoint encoded in the corresponding type as a single call to the sink
	*	(returns false and does not modify the sink if the charset cannot hold the character) */
	constexpr bool EncodeInto(str::AnySink auto&& sink, char32_t cp) {
		using ChType = str::SinkChar<decltype(sink)>;
		return detail::WriteCodePoint<ChType>(sink, cp);
	}

	/* encodes a single codepoint into small string using str::EncodeInto and returns it */
	template <str::IsChar ChType>
	constexpr str::Small<ChType, str::MaxEncodeLength> Encode(char32_t cp) {
		str::Small<ChType, str::MaxEncodeLength> out{};
		str::EncodeInto(out, cp);
		return out;
	}

	/* read a single codepoint from the source and transcode it to the sink as a single call and leave the sink untouched if
	*	the codepoint is not considered valid (only estimate the size for the copy if the source and destination type are the
	*	same and therefore no encoding validation will be performed; returns str::CPSuccess on success) */
	constexpr str::CPOut TranscodeInto(str::AnySink auto&& sink, const str::AnyString auto& source, bool sourceCompleted) {
		using SourceType = str::StringChar<decltype(source)>;
		using SinkType = str::SinkChar<decltype(sink)>;

		auto [begin, end] = str::StringIterators<SourceType>(source);
		if (begin == end)
			return { str::CPEmpty, 0 };

		str::CPOut out = detail::TranscodeNext<SourceType, SinkType>(sink, begin, end);
		if (!sourceCompleted || out.cp != str::CPIncomplete)
			return out;
		return { str::CPInvalid, uint32_t(end - begin) };
	}

	/* transcodes a single codepoint into small string using str::TranscodeInto and returns it */
	template <str::IsChar ChType>
	struct TranscodeOut {
		str::Small<ChType, str::MaxEncodeLength> buffer;
		char32_t cp = str::CPEmpty;
		uint32_t consumed = 0;
	};
	template <str::IsChar ChType>
	constexpr str::TranscodeOut<ChType> Transcode(const str::AnyString auto& source, bool sourceCompleted) {
		str::TranscodeOut<ChType> out{};
		str::CPOut tmp = str::TranscodeInto(out.buffer, source, sourceCompleted);

		out.cp = tmp.cp;
		out.consumed = tmp.consumed;
		return out;
	}

	/* convert the source-string of any type and append it to the sink-string and only copy it without transcoding if the
	*	source and target type match (insert error char, if a character could not be transcoded and the error-char is not null) */
	constexpr auto& ConvertInto(str::AnySink auto&& sink, const str::AnyString auto& source, char charOnError = str::CharOnError) {
		using SourceType = str::StringChar<decltype(source)>;
		using SinkType = str::SinkChar<decltype(sink)>;

		auto [begin, end] = str::StringIterators<SourceType>(source);
		if (begin == end)
			return sink;

		/* check if a copy can be performed */
		if constexpr (std::is_same_v<detail::EffChar<SourceType>, detail::EffChar<SinkType>>) {
			str::WriteSink<SinkType>(sink, reinterpret_cast<const SinkType*>(begin), end - begin);
			return sink;
		}

		/* transcode all characters until the end of the string has been reached (and insert errors if error-char is not null) */
		while (begin != end) {
			auto [cp, consumed] = detail::TranscodeNext<SourceType, SinkType>(sink, begin, end);
			if (cp == str::CPIncomplete)
				begin = end;
			else
				begin += consumed;

			/* check if an error occurred and the error character should be inserted instead and write the error character
			*	to the sink (this should be rare, therefore it can be decoded and encoded properly; ignore any kind of failures) */
			if (cp != str::CPSuccess && charOnError != 0)
				detail::TranscodeNext<char, SinkType>(sink, &charOnError, &charOnError + 1);
		}
		return sink;
	}

	/* convert any object to the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType>
	constexpr std::basic_string<ChType> Convert(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		std::basic_string<ChType> out{};
		return str::ConvertInto(out, source, charOnError);
	}

	/* convert any object to the destination character-type (returning str::Small<Capacity>) */
	template <str::IsChar ChType, intptr_t Capacity>
	constexpr str::Small<ChType, Capacity> Convert(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		str::Small<ChType, Capacity> out{};
		return str::ConvertInto(out, source, charOnError);
	}

	/* convenience for fast conversion to a std::basic_string */
	constexpr std::string ToChar(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char>(source, charOnError);
	}
	constexpr std::wstring ToWide(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<wchar_t>(source, charOnError);
	}
	constexpr std::u8string ToUtf8(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char8_t>(source, charOnError);
	}
	constexpr std::u16string ToUtf16(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char16_t>(source, charOnError);
	}
	constexpr std::u32string ToUtf32(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char32_t>(source, charOnError);
	}

	/* convenience for fast conversion to a str::Small<Capacity> */
	template <intptr_t Capacity>
	constexpr str::ChSmall<Capacity> ToChar(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char, Capacity>(source, charOnError);
	}
	template <intptr_t Capacity>
	constexpr str::WdSmall<Capacity> ToWide(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<wchar_t, Capacity>(source, charOnError);
	}
	template <intptr_t Capacity>
	constexpr str::U8Small<Capacity> ToUtf8(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char8_t, Capacity>(source, charOnError);
	}
	template <intptr_t Capacity>
	constexpr str::U16Small<Capacity> ToUtf16(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char16_t, Capacity>(source, charOnError);
	}
	template <intptr_t Capacity>
	constexpr str::U32Small<Capacity> ToUtf32(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Convert<char32_t, Capacity>(source, charOnError);
	}




	/* return a string containing the byte-order-mark encoded in the corresponding type (empty if not utf8/utf16/utf32) */
	template <str::IsChar ChType>
	constexpr str::Small<ChType, str::MaxEncodeLength> BOM() {
		str::Small<ChType, str::MaxEncodeLength> out{};
		if constexpr (str::IsUnicode<ChType>)
			detail::WriteCodePoint<ChType>(out, 0xfeff);
		return out;
	}
}
