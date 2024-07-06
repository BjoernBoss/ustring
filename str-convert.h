#pragma once

#include "str-common.h"
#include "str-codepoint.h"
#include "unicode/cp-property.h"

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
	struct CPOut {
		char32_t cp = cp::Empty;
		uint32_t consumed = 0;
	};

	namespace detail {
		template <class ChType> struct MaxEncodingSize;
		template <> struct MaxEncodingSize<char> { static constexpr size_t value = 4; };
		template <> struct MaxEncodingSize<char8_t> { static constexpr size_t value = 4; };
		template <> struct MaxEncodingSize<char16_t> { static constexpr size_t value = 2; };
		template <> struct MaxEncodingSize<char32_t> { static constexpr size_t value = 1; };
		template <> struct MaxEncodingSize<wchar_t> { static constexpr size_t value = (str::IsWideUtf16 ? 2 : 1); };

		/* utf-8 help lookup maps (of the upper 5 bits) and boundary maps (for length) */
		static constexpr uint8_t Utf8InitCharLength[32] = {
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
			0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
		};
		static constexpr uint8_t Utf8InitCharCodeBits[32] = {
			0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1f, 0x1f, 0x1f, 0x1f, 0x0f, 0x0f, 0x07, 0x00
		};

		inline constexpr str::CPOut ReadUtf8(const char8_t* begin, const char8_t* end) {
			uint8_t c8 = static_cast<uint8_t>(*begin);

			/* lookup the length of the character (with 0 invalid encoding) and extract the initial codepoint bits */
			uint32_t len = detail::Utf8InitCharLength[c8 >> 3];
			if (len == 0)
				return { cp::Invalid, 1 };
			uint32_t cp = (c8 & detail::Utf8InitCharCodeBits[c8 >> 3]);

			/* check if the codepoint will be overlong */
			if (cp == 0 && len > 1)
				return { cp::Invalid, len };

			/* validate the buffer has capacity enough */
			if (end - begin < len)
				return { cp::Incomplete, 0 };

			/* validate the contiunation-bytes and construct the final codepoint */
			for (uint32_t j = 1; j < len; ++j) {
				uint8_t n8 = static_cast<uint8_t>(begin[j]);
				if ((n8 & 0xc0) != 0x80)
					return { cp::Invalid, j };
				cp = ((cp << 6) | (n8 & 0x3f));
			}

			/* check if its an invalid codepoint */
			if (!cp::IsUnicode(char32_t(cp)))
				return { cp::Invalid, len };
			return { char32_t(cp), len };
		}
		inline constexpr str::CPOut ReadUtf16(const char16_t* begin, const char16_t* end) {
			uint32_t cp = static_cast<uint16_t>(*begin);

			/* check if its a valid single-token codepoint */
			if (cp < detail::SurrogateFirst || cp > detail::SurrogateLast)
				return { cp, 1 };

			/* ensure there are enough characters for a surrogate-pair and that the first value is valid */
			if (cp >= detail::SurrogateUpper)
				return { cp::Invalid, 1 };
			if (end - begin < 2)
				return { cp::Incomplete, 0 };

			/* extract and validate the second token */
			uint32_t next = static_cast<uint16_t>(begin[1]);
			if (next < detail::SurrogateUpper || next > detail::SurrogateLast)
				return { cp::Invalid, 2 };

			/* decode the overall codepoint (cannot produce any invalid codepoints) */
			return { char32_t(0x10000 + ((cp & 0x03ff) << 10) | (next & 0x03ff)), 2 };
		}
		inline constexpr str::CPOut ReadUtf32(const char32_t* begin, const char32_t* end) {
			uint32_t cp = static_cast<uint32_t>(*begin);

			/* validate the codepoint */
			if (!cp::IsUnicode(char32_t(cp)))
				return { cp::Invalid, 1 };
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
			uint32_t len = 1;

			/* check if the character is considered incompleted */
			if (res == static_cast<size_t>(-2))
				len = uint32_t(end - begin);

			/* check if the decoded character is not a valid character, in which case the length is not
			*	returned and has to be figured out by creeping up to the previous response */
			else if (res == static_cast<size_t>(-1)) while (true) {
				/* always use a clean state to ensure the previous try does not polute this result */
				state = { 0 };
				res = std::mbrtowc(0, begin, len, &state);

				/* check if the encoding is not considered incomplete anymore (should std::mbrtowc
				*	not behave deterministically, just return the current length as invalid) */
				if (res != static_cast<size_t>(-2) || len >= (end - begin)) {
					res = -1;
					break;
				}
				++len;
			}

			/* check if the result is not the null-byte in which case it reflects the length */
			else if (res != 0)
				len = uint32_t(res);

			/* check if the char is longer than the internal max character-length, which should really not happen
			*	in any encoding and would break the promise of detail::MaxEncodingSize, or if an error occurred */
			if (len > detail::MaxEncodingSize<char>::value)
				return { cp::Invalid, detail::MaxEncodingSize<char>::value };
			if (res == static_cast<size_t>(-1))
				return { cp::Invalid, len };

			/* check if the result is incomplete */
			if (res == static_cast<size_t>(-2))
				return { cp::Incomplete, 0 };

			/* check if only the size was required */
			if constexpr (SizeOnly)
				return { cp::Success, len };

			/* check which kind of unicode-encoding needs to be used (consumed can be discarded as it will either be 1 or 0 but an error returned) */
			char32_t cp = 0;
			if constexpr (str::IsWideUtf16)
				cp = detail::ReadUtf16(reinterpret_cast<const char16_t*>(&wc), reinterpret_cast<const char16_t*>(&wc) + 1).cp;
			else
				cp = detail::ReadUtf32(reinterpret_cast<const char32_t*>(&wc), reinterpret_cast<const char32_t*>(&wc) + 1).cp;

			/* check if the decoding worked */
			if (!cp::Valid(cp))
				return { cp::Invalid, len };
			return { cp, len };
		}

		/* expect s to contain at least one character (will only return consumed:0 on incomplete chars) */
		template <class ChType>
		constexpr str::CPOut ReadCodePoint(const ChType* begin, const ChType* end) {
			using EffType = str::EffChar<ChType>;
			str::CPOut out{};

			/* check for the fast way out by the character being an immediate ascii character */
			if (str::IsAscii<ChType> && cp::prop::IsAscii(*begin))
				return { char32_t(*begin), 1 };

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
				str::SinkChars<ChType>(sink, static_cast<ChType>(cp), 1);
				return true;
			}

			/* determine the length of the character */
			ChType out[4] = { 0 };
			size_t cont = 0;
			if (cp <= 0x07ff) {
				out[0] = static_cast<ChType>(0xc0 | (cp >> 6));
				cont = 1;
			}
			else if (cp >= detail::SurrogateFirst && cp <= detail::SurrogateLast)
				return false;
			else if (cp <= 0xffff) {
				out[0] = static_cast<ChType>(0xe0 | (cp >> 12));
				cont = 2;
			}
			else if (cp < cp::UnicodeRange) {
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
			str::SinkString<ChType>(sink, out, len);
			return true;
		}
		template <class ChType>
		constexpr bool WriteUtf16(auto& sink, uint32_t cp) {
			/* check if its a single utf16-byte */
			if (cp < 0x10000) {
				if (cp >= detail::SurrogateFirst && cp <= detail::SurrogateUpper)
					return false;
				str::SinkChars<ChType>(sink, static_cast<ChType>(cp), 1);
				return true;
			}
			if (cp >= cp::UnicodeRange)
				return false;
			cp -= 0x10000;

			/* produce the two utf16-bytes */
			ChType out[2] = { 0 };
			out[0] = static_cast<ChType>(detail::SurrogateFirst + (cp >> 10));
			out[1] = static_cast<ChType>(detail::SurrogateUpper + (cp & 0x03ff));

			/* write the data to the sink */
			str::SinkString<ChType>(sink, out, 2);
			return true;
		}
		template <class ChType>
		constexpr bool WriteUtf32(auto& sink, uint32_t cp) {
			/* validate the codepoint and write it to the sink */
			if (!cp::IsUnicode(char32_t(cp)))
				return false;
			str::SinkChars<ChType>(sink, static_cast<ChType>(cp), 1);
			return true;
		}
		constexpr bool WriteMultiByte(auto& sink, uint32_t cp) {
			str::Small<wchar_t, detail::MaxEncodingSize<wchar_t>::value> wc;

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
			if (res == static_cast<size_t>(-1) || res > detail::MaxEncodingSize<char>::value)
				return false;

			/* write the characters to the sink (res should never be zero) */
			str::SinkString<char>(sink, buf.c_str(), res);
			return true;
		}

		template <class ChType>
		constexpr bool WriteCodePoint(auto& sink, char32_t cp) {
			using EffType = str::EffChar<ChType>;

			/* check for the fast way out by the character being an immediate ascii character */
			if (str::IsAscii<ChType> && cp::prop::IsAscii(cp)) {
				str::SinkChars<ChType>(sink, static_cast<ChType>(cp), 1);
				return true;
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
			using EffType = str::EffChar<ChType>;
			uint32_t len = 0;

			/* check for the fast way out by the character being an immediate ascii character */
			if (str::IsAscii<ChType> && cp::prop::IsAscii(*begin))
				return { cp::Success, 1 };

			/* check what encoding it is and read the size (invalid encodings result in length 0) */
			if constexpr (std::is_same_v<EffType, char8_t>)
				len = detail::Utf8InitCharLength[static_cast<uint8_t>(*begin) >> 3];
			else if constexpr (std::is_same_v<EffType, char16_t>) {
				uint16_t c16 = static_cast<uint16_t>(*begin);
				if (c16 < detail::SurrogateFirst || c16 > detail::SurrogateLast)
					len = 1;
				else
					len = (c16 < detail::SurrogateUpper ? 2 : 0);
			}
			else if constexpr (std::is_same_v<EffType, char32_t>)
				len = 1;
			else
				return detail::ReadMultiByte<true>(begin, end);

			/* construct the intermediate result (invalid first-byte encodings are just considered size-1) */
			if (len == 0)
				return { cp::Invalid, 1 };
			if (len <= uint32_t(end - begin))
				return { cp::Success, len };
			return { cp::Incomplete, 0 };
		}

		/* if different types, fully decode codepoint, otherwise only estimate the size to copy (expects s to contain at least one character) */
		template <class SourceType, class SinkType>
		constexpr str::CPOut TranscodeNext(auto& sink, const SourceType* begin, const SourceType* end) {
			str::CPOut out{};

			/* check if the source and destination are the same type, in which case the length can be extracted
			*	and copied over to the source (codepoint can be set to null, as it will not be used) */
			if constexpr (str::_EffSame<SourceType, SinkType>)
				out = detail::ReadNextSize<SourceType>(begin, end);
			else
				out = detail::ReadCodePoint<SourceType>(begin, end);

			/* check if its incomplete or if an error occurred and return the result */
			if (!cp::Valid(out.cp))
				return out;

			/* check if the source and destination are of the same type, in which case the characters can just be copied */
			if constexpr (str::_EffSame<SourceType, SinkType>)
				str::SinkString<SinkType>(sink, reinterpret_cast<const SinkType*>(begin), out.consumed);

			/* try to write the codepoint to the destination and otherwise return the consumed number of token */
			else if (!detail::WriteCodePoint<SinkType>(sink, out.cp))
				return { cp::WriteFailed, out.consumed };
			return { cp::Success, out.consumed };
		}
	}

	/* maximum number of characters for the given char-type required to encode a single codepoint */
	template <str::IsChar ChType>
	constexpr size_t _MaxEncSize = detail::MaxEncodingSize<ChType>::value;

	/* maximum number of bytes for the given char-type required to encode a single codepoint */
	template <str::IsChar ChType>
	constexpr size_t _MaxEncBytes = detail::MaxEncodingSize<ChType>::value * sizeof(ChType);

	/* small-string large enough to hold at least one complete codepoint when encoded into it */
	template <str::IsChar ChType>
	using CPSmall = str::Small<ChType, str::_MaxEncSize<ChType>>;

	/* read a single codepoint from the beginning of the string (will not return invalid unicode-codepoints, if result is valid) */
	constexpr str::CPOut Decode(const str::_AnyString auto& source, bool sourceCompleted) {
		using ChType = str::_StringCharType<decltype(source)>;

		auto [begin, end] = str::StringIterators<ChType>(source);
		if (begin == end)
			return { cp::Empty, 0 };

		str::CPOut out = detail::ReadCodePoint<ChType>(begin, end);
		if (!sourceCompleted || out.cp != cp::Incomplete)
			return out;
		return { cp::Invalid, uint32_t(end - begin) };
	}

	/* check if the next character is an ascii character and return it or return cp::NotAscii with zero characters consumed */
	constexpr str::CPOut ReadAscii(const str::_AnyString auto& source) {
		using ChType = str::_StringCharType<decltype(source)>;

		auto [begin, end] = str::StringIterators<ChType>(source);
		if (begin == end)
			return { cp::NotAscii, 0 };

		/* check if the raw value itself can be checked for being an ascii character */
		if constexpr (str::IsAscii<ChType>) {
			if (cp::prop::IsAscii(*begin))
				return { char32_t(*begin), 1 };
			return { cp::NotAscii, 0 };
		}

		/* decode the codepoint and check if its a valid ascii character */
		str::CPOut out = detail::ReadCodePoint<ChType>(begin, end);
		if (cp::Valid(out.cp) && cp::prop::IsAscii(out.cp))
			return out;
		return { cp::NotAscii, 0 };
	}

	/* compute the size of the next character in the source while testing as few as characters as possible (returns cp::Success on success) */
	constexpr str::CPOut LengthNext(const str::_AnyString auto& source, bool sourceCompleted) {
		using ChType = str::_StringCharType<decltype(source)>;

		auto [begin, end] = str::StringIterators<ChType>(source);
		if (begin == end)
			return { cp::Empty, 0 };

		str::CPOut out = detail::ReadNextSize<ChType>(begin, end);
		if (!sourceCompleted || out.cp != cp::Incomplete)
			return out;
		return { cp::Invalid, uint32_t(end - begin) };
	}

	/* measure the entire length of the source while testing as few as characters as possible */
	struct MeasureOut {
		size_t codepoints = 0;
		size_t invalid = 0;
	};
	constexpr str::MeasureOut Measure(const str::_AnyString auto& source) {
		using ChType = str::_StringCharType<decltype(source)>;
		str::MeasureOut out{};

		auto [begin, end] = str::StringIterators<ChType>(source);

		/* iterate over the string and measure all characters */
		while (begin != end) {
			str::CPOut res = detail::ReadNextSize<ChType>(begin, end);

			if (res.cp == cp::Success)
				++out.codepoints;
			else {
				++out.invalid;
				if (res.cp == cp::Incomplete)
					break;
			}
			begin += res.consumed;
		}
		return out;
	}

	/* write the single codepoint encoded in the corresponding type as a single call to the sink
	*	(returns false and does not modify the sink if the charset cannot hold the character) */
	constexpr bool EncodeInto(str::_AnySink auto&& sink, char32_t cp) {
		using ChType = str::_SinkCharType<decltype(sink)>;
		return detail::WriteCodePoint<ChType>(sink, cp);
	}

	/* encodes a single codepoint into small string using str::EncodeInto and returns it
	*	(return an empty string if the target charset cannot hold the codepoint) */
	template <str::IsChar ChType>
	constexpr str::CPSmall<ChType> _Encode(char32_t cp) {
		str::CPSmall<ChType> out{};
		str::EncodeInto(out, cp);
		return out;
	}

	/* read a single codepoint from the source and transcode it to the sink as a single call and leave the sink untouched if
	*	the codepoint is not considered valid (only estimate the size for the copy if the source and destination type are the
	*	same and therefore no encoding validation will be performed; returns cp::Success on success) */
	constexpr str::CPOut TranscodeInto(str::_AnySink auto&& sink, const str::_AnyString auto& source, bool sourceCompleted) {
		using SourceType = str::_StringCharType<decltype(source)>;
		using SinkType = str::_SinkCharType<decltype(sink)>;

		auto [begin, end] = str::StringIterators<SourceType>(source);
		if (begin == end)
			return { cp::Empty, 0 };

		str::CPOut out = detail::TranscodeNext<SourceType, SinkType>(sink, begin, end);
		if (!sourceCompleted || out.cp != cp::Incomplete)
			return out;
		return { cp::Invalid, uint32_t(end - begin) };
	}

	/* transcodes a single codepoint into small string using str::TranscodeInto and returns it */
	template <str::IsChar ChType>
	struct TranscodeOut {
		str::CPSmall<ChType> buffer;
		char32_t cp = cp::Empty;
		uint32_t consumed = 0;
	};
	template <str::IsChar ChType>
	constexpr str::TranscodeOut<ChType> Transcode(const str::_AnyString auto& source, bool sourceCompleted) {
		str::TranscodeOut<ChType> out{};
		str::CPOut tmp = str::TranscodeInto(out.buffer, source, sourceCompleted);

		out.cp = tmp.cp;
		out.consumed = tmp.consumed;
		return out;
	}

	/* convert the source-character of any type and append it to the sink-string and copy it without transcoding if the
	*	source and target type match (insert error char, if the character could not be transcoded and error-char is not null) */
	constexpr auto& AppChars(str::_AnySink auto&& sink, str::IsChar auto chr, size_t count = 1, char32_t cpOnError = cp::DefErrorChar) {
		using SourceType = decltype(chr);
		using SinkType = str::_SinkCharType<decltype(sink)>;

		/* check if a simple write can be performed */
		if constexpr (str::_EffSame<SourceType, SinkType>) {
			str::SinkChars<SinkType>(sink, static_cast<SinkType>(chr), count);
			return sink;
		}

		/* check if the character can just be transcoded */
		if (count == 1) {
			if constexpr (str::_EffSame<SourceType, char32_t>)
				detail::WriteCodePoint<SinkType>(sink, static_cast<char32_t>(chr));

			/* write the character and write the error char if an error occurred */
			else if (detail::TranscodeNext<SourceType, SinkType>(sink, &chr, &chr + 1).cp != cp::Success && cpOnError != 0)
				detail::WriteCodePoint<SinkType>(sink, cpOnError);
			return sink;
		}
		else if (count == 0)
			return sink;

		/* transcode the character to a temporary buffer and write the error char if an error occurred and otherwise return as nothing can be done */
		str::CPSmall<SinkType> temp;
		if (detail::TranscodeNext<SourceType, SinkType>(temp, &chr, &chr + 1).cp != cp::Success) {
			if (cpOnError == 0 || !detail::WriteCodePoint<SinkType>(temp, cpOnError))
				return sink;
		}

		/* write the temporary buffer to the sink */
		if (temp.size() == 1)
			str::SinkChars<SinkType>(sink, temp[0], count);
		else for (size_t i = 0; i < count; ++i)
			str::SinkString<SinkType>(sink, temp.c_str(), temp.size());
		return sink;
	}

	/* convert the source-string of any type and append it to the sink-string and copy it without transcoding if the
	*	source and target type match (insert error char, if a character could not be transcoded and error-char is not null) */
	constexpr auto& Append(str::_AnySink auto&& sink, const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		using SourceType = str::_StringCharType<decltype(source)>;
		using SinkType = str::_SinkCharType<decltype(sink)>;

		auto [begin, end] = str::StringIterators<SourceType>(source);
		if (begin == end)
			return sink;

		/* check if a simple write can be performed */
		if constexpr (str::_EffSame<SourceType, SinkType>) {
			str::SinkString<SinkType>(sink, reinterpret_cast<const SinkType*>(begin), end - begin);
			return sink;
		}

		/* transcode all characters until the end of the string has been reached (and insert errors if error-char is not null) */
		while (begin != end) {
			auto [cp, consumed] = detail::TranscodeNext<SourceType, SinkType>(sink, begin, end);
			if (cp == cp::Incomplete)
				begin = end;
			else
				begin += consumed;

			/* check if an error occurred and the error character should be inserted instead and write the error character to the sink */
			if (cp != cp::Success && cpOnError != 0)
				detail::WriteCodePoint<SinkType>(sink, cpOnError);
		}
		return sink;
	}

	/* wraps str::Append to keep style of ConvertTo/ConvertInto */
	constexpr auto& ConvertInto(str::_AnySink auto&& sink, const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		str::Append(sink, source, cpOnError);
		return sink;
	}

	/* convert any object to the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType>
	constexpr std::basic_string<ChType> ConvertTo(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		std::basic_string<ChType> out{};
		return str::ConvertInto(out, source, cpOnError);
	}

	/* convert any object to the destination character-type (returning str::Small<Capacity>) */
	template <str::IsChar ChType, intptr_t Capacity>
	constexpr str::Small<ChType, Capacity> ConvertTo(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		str::Small<ChType, Capacity> out{};
		return str::ConvertInto(out, source, cpOnError);
	}

	/* convenience for fast conversion to a std::basic_string */
	constexpr std::string ToChar(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char>(source, cpOnError);
	}
	constexpr std::wstring ToWide(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<wchar_t>(source, cpOnError);
	}
	constexpr std::u8string ToUtf8(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char8_t>(source, cpOnError);
	}
	constexpr std::u16string ToUtf16(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char16_t>(source, cpOnError);
	}
	constexpr std::u32string ToUtf32(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char32_t>(source, cpOnError);
	}

	/* convenience for fast conversion to a str::Small<Capacity> */
	template <intptr_t Capacity>
	constexpr str::ChSmall<Capacity> ToChar(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char, Capacity>(source, cpOnError);
	}
	template <intptr_t Capacity>
	constexpr str::WdSmall<Capacity> ToWide(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<wchar_t, Capacity>(source, cpOnError);
	}
	template <intptr_t Capacity>
	constexpr str::U8Small<Capacity> ToUtf8(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char8_t, Capacity>(source, cpOnError);
	}
	template <intptr_t Capacity>
	constexpr str::U16Small<Capacity> ToUtf16(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char16_t, Capacity>(source, cpOnError);
	}
	template <intptr_t Capacity>
	constexpr str::U32Small<Capacity> ToUtf32(const str::_AnyString auto& source, char32_t cpOnError = cp::DefErrorChar) {
		return str::ConvertTo<char32_t, Capacity>(source, cpOnError);
	}
}
