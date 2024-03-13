#pragma once

#include "str-common.h"

#include <string>
#include <concepts>
#include <utility>
#include <cwchar>
#include <type_traits>
#include <tuple>

/*
*	char8_t/char16_t/char32_t must be their respective unicode-encodings
*	wchar_t must be a unicode-encoding
*	char can be multi-byte using the current locale or any unicode-encoding
*
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
	static constexpr char CharOnError = '?';
	static constexpr char32_t NotAscii = char32_t(-1);

	/* maximum number of characters to potentially be consumed/produced per codepoint */
	static constexpr size_t MaxEncodeLength = 4;

	/* number of ascii characters with valid range: [0, 127] */
	static constexpr size_t AsciiRange = 128;

	/* number of unicode characters lie within range: [0, 0x10ffff] */
	static constexpr size_t UnicodeRange = 0x110000;

	/*
	*	valid: valid decoded codepoint
	*	empty: source was empty and therefore nothing decoded
	*	invalid: encoding error was detected in source or encoded codepoint is invalid or did not use the proper encoding-form
	*	incomplete: codepoint seems to be encoded properly, but missing one or more characters for completion (if source is completed,
	*		return value will never be incomplete and at least one character will be consumed)
	*/
	enum class DecResult : uint8_t {
		valid,
		empty,
		invalid,
		incomplete
	};

	struct DecodeOut {
		size_t consumed = 0;
		char32_t cp = 0;
		str::DecResult result = str::DecResult::empty;
	};
	struct AsciiOut {
		size_t consumed = 0;
		char32_t cp = 0;
	};
	struct LengthOut {
		size_t consumed = 0;
		str::DecResult result = str::DecResult::empty;
	};
	struct TranscodeOut {
		size_t consumed = 0;
		str::DecResult result = str::DecResult::empty;
		bool valid = false;
	};
	template <str::IsChar ChType>
	struct TranscodeBufOut {
		str::Small<ChType, str::MaxEncodeLength> buffer;
		size_t consumed = 0;
		str::DecResult result = str::DecResult::empty;
		bool valid = false;
	};
	struct MeasureOut {
		size_t consumed = 0;
		size_t codepoints = 0;
		size_t invalid = 0;
	};

	namespace detail {
		template <class ExpType, size_t ExpSize, class ActType, size_t ActSize>
		constexpr bool IsBufferSame(const ExpType(&expected)[ExpSize], const ActType(&actual)[ActSize]) {
			if (sizeof(ExpType) != sizeof(ActType) || ExpSize != ActSize)
				return false;

			size_t i = 0;
			while (i < ExpSize && expected[i] == static_cast<ExpType>(actual[i]))
				++i;

			return (i == ExpSize);
		}
		template <class ExpType, size_t ExpSize, class ActType, size_t ActSize>
		constexpr bool HoldSameValues(const ExpType(&expected)[ExpSize], const ActType(&actual)[ActSize]) {
			if (ExpSize != ActSize)
				return false;
			using LargeType = std::conditional_t<sizeof(ExpType) >= sizeof(ActType), ExpType, ActType>;

			size_t i = 0;
			while (i < ExpSize && static_cast<LargeType>(expected[i]) == static_cast<LargeType>(actual[i]))
				++i;

			return (i == ExpSize);
		}

		/* utf8 test-string: \U0000007f\U0000ff00\U00010000 */
		template <class Type, size_t Size>
		constexpr bool IsUtf8(const Type(&test)[Size]) {
			constexpr uint8_t expected[] = { 0x7f, 0xef, 0xbc, 0x80, 0xf0, 0x90, 0x80, 0x80, 0x00 };
			return detail::IsBufferSame(expected, test);
		};

		/* utf16 test-string: \U00010000\U0000ff00 */
		template <class Type, size_t Size>
		constexpr bool IsUtf16(const Type(&test)[Size]) {
			constexpr uint16_t expected[] = { 0xd800, 0xdc00, 0xff00, 0x0000 };
			return detail::IsBufferSame(expected, test);
		};

		/* utf32 test-string: \U00010000\U0000ff00 */
		template <class Type, size_t Size>
		constexpr bool IsUtf32(const Type(&test)[Size]) {
			constexpr uint32_t expected[] = { 0x10000, 0xff00, 0x0000 };
			return detail::IsBufferSame(expected, test);
		};

		/* check assumptions hold */
		static_assert(detail::IsUtf8(u8"\U0000007f\U0000ff00\U00010000"), "char8_t is expected to be utf-8 encoded");
		static_assert(detail::IsUtf16(u"\U00010000\U0000ff00"), "char16_t is expected to be utf-16 encoded");
		static_assert(detail::IsUtf32(U"\U00010000\U0000ff00"), "char32_t is expected to be utf-32 encoded");

		/* [character cannot be represented in current code page] */
#pragma warning(push)
#pragma warning(disable : 4566)

		/* type equals char or char8_t/char16_t/char32_t, depending on encoding */
		using MBEquivalence = std::conditional_t<detail::IsUtf8("\U0000007f\U0000ff00\U00010000"), char8_t,
			std::conditional_t<detail::IsUtf16("\U00010000\U0000ff00"), char16_t,
			std::conditional_t<detail::IsUtf32("\U00010000\U0000ff00"), char32_t, char>>>;

		/* type equals wchar_t or char8_t/char16_t/char32_t, depending on encoding */
		using WideEquivalence = std::conditional_t<detail::IsUtf8(L"\U0000007f\U0000ff00\U00010000"), char8_t,
			std::conditional_t<detail::IsUtf16(L"\U00010000\U0000ff00"), char16_t,
			std::conditional_t<detail::IsUtf32(L"\U00010000\U0000ff00"), char32_t, wchar_t>>>;
		static_assert(!std::is_same_v<detail::WideEquivalence, wchar_t>, "wchar_t is expected to be a unicode encoding");
#pragma warning(pop)

		static constexpr bool CharHoldsAscii = detail::HoldSameValues(
			U"\0\001\002\003\004\005\006\a\b\t\n\v\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
			U" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177",
			"\0\001\002\003\004\005\006\a\b\t\n\v\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
			" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177");

		/* extract effective character (char/char8_t/char16_t/char32_t) */
		template <class Type>
		using EffectiveChar = std::conditional_t<std::is_same_v<Type, char>, detail::MBEquivalence,
			std::conditional_t<std::is_same_v<Type, wchar_t>, detail::WideEquivalence, Type>>;

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

		constexpr std::tuple<size_t, uint32_t, str::DecResult> ReadUtf8(const auto& source) {
			uint8_t c8 = static_cast<uint8_t>(source[0]);

			/* lookup the length of the character (with 0 invalid encoding) and extract the initial codepoint bits */
			size_t len = detail::Utf8InitCharLength[c8 >> 3];
			if (len == 0)
				return { 1, 0, str::DecResult::invalid };
			uint32_t cp = (c8 & detail::Utf8InitCharCodeBits[c8 >> 3]);

			/* check if the codepoint will be overlong */
			if (cp == 0 && len > 1)
				return { len, 0, str::DecResult::invalid };

			/* validate the buffer has capacity enough */
			if (source.size() < len)
				return { 0, 0, str::DecResult::incomplete };

			/* validate the contiunation-bytes and construct the final codepoint */
			for (size_t j = 1; j < len; ++j) {
				uint8_t n8 = static_cast<uint8_t>(source[j]);
				if ((n8 & 0xc0) != 0x80)
					return { j, 0, str::DecResult::invalid };
				cp = ((cp << 6) | (n8 & 0x3f));
			}

			/* check if its an invalid codepoint (i.e. greater than UnicodeRange or surrogate-pair) */
			if (cp >= str::UnicodeRange || (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16LastSurrogate))
				return { len, 0, str::DecResult::invalid };
			return { len, cp, str::DecResult::valid };
		}
		constexpr std::tuple<size_t, uint32_t, str::DecResult> ReadUtf16(const auto& source) {
			uint32_t cp = static_cast<uint16_t>(source[0]);

			/* check if its a valid single-token codepoint */
			if (cp < detail::Utf16LowerSurrogate || cp > detail::Utf16LastSurrogate)
				return { 1, cp, str::DecResult::valid };

			/* ensure there are enough characters for a surrogate-pair and that the first value is valid */
			if (cp >= detail::Utf16UpperSurrogate)
				return { 1, 0, str::DecResult::invalid };
			if (source.size() < 2)
				return { 0, 0, str::DecResult::incomplete };

			/* extract and validate the second token */
			uint32_t next = static_cast<uint16_t>(source[1]);
			if (next < detail::Utf16UpperSurrogate || next > detail::Utf16LastSurrogate)
				return { 2, 0, str::DecResult::invalid };

			/* decode the overall codepoint (cannot produce any invalid codepoints) */
			return { 2, 0x10000 + ((cp & 0x03ff) << 10) | (next & 0x03ff), str::DecResult::valid };
		}
		constexpr std::tuple<size_t, uint32_t, str::DecResult> ReadUtf32(const auto& source) {
			uint32_t cp = static_cast<uint32_t>(source[0]);

			/* validate the codepoint */
			if (cp >= str::UnicodeRange || (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16UpperSurrogate))
				return { 1, 0, str::DecResult::invalid };
			return { 1, cp, str::DecResult::valid };
		}
		template <bool SizeOnly>
		std::tuple<size_t, wchar_t, str::DecResult> ReadMultiByte(const std::basic_string_view<char>& source) {
			std::mbstate_t state{ 0 };
			wchar_t wc = 0;

			/* read the next character and check if the character is incomplete (documentation
			*	suggests that a decoded codepoint should always fit into single wide-char) */
			size_t res = std::mbrtowc((SizeOnly ? nullptr : &wc), source.data(), source.size(), &state);
			if (res == static_cast<size_t>(-2)) {
				/* check if the max encoding length promise would be broken by this character (should not be possible for any encoding) */
				if (source.size() >= str::MaxEncodeLength)
					return { str::MaxEncodeLength, 0, str::DecResult::invalid };
				return { 0, 0, str::DecResult::incomplete };
			}
			size_t len = res;

			/* check if the decoded character is not a valid character or the null-character, in which case
			*	the length is not returned and has to be figured out by creeping up to the previous response */
			if (res == static_cast<size_t>(-1) || res == 0) {
				size_t len = 1;

				/* try to find the length by creeping up to the previous response (as we dont know the length of the error/null-byte) */
				while (true) {
					/* always use a clean state to ensure the previous try does not polute this result */
					state = { 0 };
					size_t temp = std::mbrtowc(0, source.data(), len, &state);

					/* check if the given result has been reached, or the character is still incomplete */
					if (temp == res)
						break;
					if (temp == static_cast<size_t>(-2) && len < source.size()) {
						++len;
						continue;
					}

					/* a new error has been received/end of source has been reached, in which case an error in the encoding can
					*	just be returned as this state should really never be reached for a valid and deterministic std::mbrtowc */
					return { std::min<size_t>(len, str::MaxEncodeLength), 0, str::DecResult::invalid };
				}
			}

			/* check if the char is longer than the internal max character-length, which should really not happen in any encoding
			*	and would break the promise of str::MaxEncodeLength, or invalid for both of which an error can be returned */
			if (res == static_cast<size_t>(-1) || len > str::MaxEncodeLength)
				return { std::min<size_t>(len, str::MaxEncodeLength), 0, str::DecResult::invalid };
			return { len, wc, str::DecResult::valid };
		}

		template <class ChType>
		constexpr bool WriteUtf8(auto& sink, uint32_t cp) {
			/* check if its a single character, which can just be pushed */
			if (cp <= 0x7f) {
				sink.push_back(static_cast<ChType>(cp));
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
			sink.append(std::basic_string_view<ChType>{ out, len });
			return true;
		}
		template <class ChType>
		constexpr bool WriteUtf16(auto& sink, uint32_t cp) {
			/* check if its a single utf16-byte */
			if (cp < 0x10000) {
				if (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16UpperSurrogate)
					return false;
				sink.push_back(static_cast<ChType>(cp));
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
			sink.append(std::basic_string_view<ChType>{ out, 2 });
			return true;
		}
		template <class ChType>
		constexpr bool WriteUtf32(auto& sink, uint32_t cp) {
			/* validate the codepoint and write it to the sink */
			if (cp >= str::UnicodeRange || (cp >= detail::Utf16LowerSurrogate && cp <= detail::Utf16UpperSurrogate))
				return false;
			sink.push_back(static_cast<ChType>(cp));
			return true;
		}
		constexpr bool WriteMultiByte(auto& sink, uint32_t cp) {
			str::Small<wchar_t, str::MaxEncodeLength> wc;

			/* convert the codepoint first to wide-characters and check if it only resulted in one, as a single
			*	multi-byte char must originate from exactly 1 wide char (also expected by read-multibyte) */
			if constexpr (std::is_same_v<detail::WideEquivalence, char8_t>) {
				if (!detail::WriteUtf8<wchar_t>(wc, cp))
					return false;
			}
			else if constexpr (std::is_same_v<detail::WideEquivalence, char16_t>) {
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

			/* try to convert the character to the multi-byte presentation
			*	(no small-buffer, as MB_CUR_MAX is not necessarily constant...) */
			std::basic_string<char> buf(MB_CUR_MAX, '\0');
			std::mbstate_t state{ 0 };
			size_t res = std::wcrtomb(&buf[0], wc[0], &state);
#pragma warning(pop)

			/* check if the character could not be converted or if the converted character would break the promise of
			*	the string-conversion max encoding length in which case the codepoint is not considered encodable */
			if (res == static_cast<size_t>(-1) || res > str::MaxEncodeLength)
				return false;

			/* write the characters to the sink */
			sink.append(std::string_view{ buf.c_str(), res });
			return true;
		}

		/* expect s to contain at least one character (will only return consumed:0 on incomplete chars) */
		template <class ChType>
		inline constexpr str::DecodeOut ReadCodePoint(const std::basic_string_view<ChType>& source, bool sourceCompleted) {
			using EffType = detail::EffectiveChar<ChType>;
			std::tuple<size_t, uint32_t, str::DecResult> res{};

			/* check for the fast way out by the character being an immediate ascii character */
			if constexpr (!std::is_same_v<EffType, char> || detail::CharHoldsAscii) {
				if (std::make_unsigned_t<ChType>(source[0]) < str::AsciiRange)
					return { 1, char32_t(source[0]), str::DecResult::valid };
			}

			/* decode the next codepoint */
			if constexpr (std::is_same_v<EffType, char>) {
				/* for multibytes, it will first be decoded to wide-chars (as the standard libraries
				*	functions to utf32 only work for utf8 inputs, at least on some msvc versions) */
				auto [len, wc, result] = detail::ReadMultiByte<false>(source);
				res = { len, 0, result };

				/* decode the wide-character */
				if (result == str::DecResult::valid) {
					std::tuple<size_t, uint32_t, str::DecResult> wres{};

					/* check which kind of unicode-encoding needs to be used */
					if constexpr (std::is_same_v<detail::WideEquivalence, char8_t>)
						wres = detail::ReadUtf8(std::basic_string_view{ &wc, 1 });
					else if constexpr (std::is_same_v<detail::WideEquivalence, char16_t>)
						wres = detail::ReadUtf16(std::basic_string_view{ &wc, 1 });
					else
						wres = detail::ReadUtf32(std::basic_string_view{ &wc, 1 });

					/* parse wchar_t-result to the primary result */
					if (std::get<2>(wres) != str::DecResult::valid || std::get<0>(wres) != 1)
						std::get<2>(res) = str::DecResult::invalid;
					else
						std::get<1>(res) = std::get<1>(wres);
				}
			}
			else if constexpr (std::is_same_v<EffType, char8_t>)
				res = detail::ReadUtf8(source);
			else if constexpr (std::is_same_v<EffType, char16_t>)
				res = detail::ReadUtf16(source);
			else
				res = detail::ReadUtf32(source);

			/* check if the end of the source has been reached, in which case a last incomplete encoding should be considered invalid */
			if (sourceCompleted && std::get<2>(res) == str::DecResult::incomplete)
				return { source.size(), 0, str::DecResult::invalid };
			return { std::get<0>(res), static_cast<char32_t>(std::get<1>(res)), std::get<2>(res) };
		}

		template <class ChType>
		constexpr bool WriteCodePoint(auto& sink, char32_t cp) {
			using EffType = detail::EffectiveChar<ChType>;

			/* check for the fast way out by the character being an immediate ascii character */
			if constexpr (!std::is_same_v<EffType, char> || detail::CharHoldsAscii) {
				if (static_cast<uint32_t>(cp) < str::AsciiRange) {
					sink.push_back(static_cast<ChType>(cp));
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

		/* estimate the size of the next character based on reading the first character (expects s to contain at least one character) */
		template <class ChType>
		constexpr str::LengthOut ReadNextSize(const std::basic_string_view<ChType>& source, bool sourceCompleted) {
			using EffType = detail::EffectiveChar<ChType>;
			size_t len = 0;

			/* check for the fast way out by the character being an immediate ascii character */
			if constexpr (!std::is_same_v<EffType, char> || detail::CharHoldsAscii) {
				if (static_cast<uint32_t>(source[0]) < str::AsciiRange)
					return { 1, str::DecResult::valid };
			}

			/* check what encoding it is and read the size (invalid encodings result in length 0) */
			if constexpr (std::is_same_v<EffType, char8_t>) {
				len = detail::Utf8InitCharLength[static_cast<uint8_t>(source[0]) >> 3];

				/* validate all other continuation bytes */
				if (len <= source.size()) {
					for (size_t i = 1; i < len; ++i) {
						if ((source[i] & 0xc0) != 0x80)
							len = 0;
					}
				}
			}
			else if constexpr (std::is_same_v<EffType, char16_t>) {
				uint16_t c16 = static_cast<uint16_t>(source[0]);
				if (c16 < detail::Utf16LowerSurrogate || c16 > detail::Utf16LastSurrogate)
					len = 1;
				else
					len = (c16 < detail::Utf16UpperSurrogate ? 2 : 0);

				/* validate the second byte of the encoding */
				if (len <= source.size() && len == 2) {
					uint16_t n16 = static_cast<uint16_t>(source[1]);
					if (n16 < detail::Utf16UpperSurrogate || n16 > detail::Utf16LastSurrogate)
						len = 0;
				}
			}
			else if constexpr (std::is_same_v<EffType, char32_t>)
				len = 1;
			else {
				/* estimate the length of the multibyte-character by decoding it to a wchar_t */
				auto [len, _, res] = detail::ReadMultiByte<true>(source);
				if (res == str::DecResult::incomplete && sourceCompleted)
					return { source.size(), str::DecResult::invalid };
				return { len, res };
			}

			/* construct the intermediate result (invalid first-byte encodings are just considered size-1) */
			if (len == 0)
				return { 1, str::DecResult::invalid };
			if (len <= source.size())
				return { len, str::DecResult::valid };
			if (sourceCompleted)
				return { source.size(), str::DecResult::invalid };
			return { 0, str::DecResult::incomplete };
		}

		/* expects s to contain at least one character */
		template <class SourceType, class SinkType>
		constexpr str::TranscodeOut TranscodeNext(auto& sink, const std::basic_string_view<SourceType>& source, bool sourceCompleted) {
			static constexpr bool EffIdentical = std::is_same_v<detail::EffectiveChar<SourceType>, detail::EffectiveChar<SinkType>>;
			str::DecodeOut out{};

			/* check if the source and destination are the same type, in which case the length can be extracted
			*	and copied over to the source (codepoint can be set to null, as it will not be used) */
			if constexpr (EffIdentical) {
				auto [len, res] = detail::ReadNextSize<SourceType>(source, sourceCompleted);
				out.consumed = len;
				out.result = res;
			}
			else
				out = detail::ReadCodePoint<SourceType>(source, sourceCompleted);

			/* check if its incomplete or if an error occurred and return the result */
			if (out.result != str::DecResult::valid)
				return { out.consumed, out.result, false };

			/* check if the source and destination are of the same type, in which case the characters can just be copied */
			if constexpr (EffIdentical)
				sink.append(std::basic_string_view<SinkType>{ reinterpret_cast<const SinkType*>(source.data()), out.consumed });

			/* try to write the codepoint to the destination and otherwise return the consumed number of token */
			else if (!detail::WriteCodePoint<SinkType>(sink, out.cp))
				return { out.consumed, str::DecResult::valid, false };
			return { out.consumed, str::DecResult::valid, true };
		}
	}

	/* check if the normal character string uses a given utf-encoding (might not use any utf-encoding, otherwise size is also considered to match) */
	static constexpr bool IsCharUtf8 = std::is_same_v<detail::MBEquivalence, char8_t>;
	static constexpr bool IsCharUtf16 = std::is_same_v<detail::MBEquivalence, char16_t>;
	static constexpr bool IsCharUtf32 = std::is_same_v<detail::MBEquivalence, char32_t>;
	static constexpr bool IsCharAscii = detail::CharHoldsAscii;

	/* check if the wide character string uses a given utf-encoding (will use exactly one utf-encoding, size will also match) */
	static constexpr bool IsWideUtf8 = std::is_same_v<detail::WideEquivalence, char8_t>;
	static constexpr bool IsWideUtf16 = std::is_same_v<detail::WideEquivalence, char16_t>;
	static constexpr bool IsWideUtf32 = std::is_same_v<detail::WideEquivalence, char32_t>;
	static constexpr bool IsWideAscii = true;

	/* read a single codepoint from the beginning of the string */
	constexpr str::DecodeOut Decode(const str::AnyString auto& source, bool sourceCompleted) {
		using ChType = str::StringChar<decltype(source)>;

		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return { 0, 0, str::DecResult::empty };

		return detail::ReadCodePoint<ChType>(view, sourceCompleted);
	}

	/* check if the next character is an ascii character and return it or return str::NotAscii with zero characters consumed */
	constexpr str::AsciiOut ReadAscii(const str::AnyString auto& source) {
		using ChType = str::StringChar<decltype(source)>;
		using EffType = detail::EffectiveChar<ChType>;

		/* check if the string itself is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return { 0, str::NotAscii };

		/* check if the raw value itself can be checked for being an ascii character */
		if constexpr (!std::is_same_v<EffType, char> || detail::CharHoldsAscii) {
			if (std::make_unsigned_t<ChType>(view[0]) < str::AsciiRange)
				return { 1, char32_t(view[0]) };
			return { 0, str::NotAscii };
		}

		/* decode the codepoint and check if its a valid ascii character */
		auto [consumed, cp, result] = detail::ReadCodePoint<ChType>(view, true);
		if (result != str::DecResult::valid || cp < 0 || cp >= str::AsciiRange)
			return { 0, str::NotAscii };
		return { consumed, cp };
	}

	/* compute the size of the next character in the source without decoding and validating it fully */
	constexpr str::LengthOut LengthNext(const str::AnyString auto& source, bool sourceCompleted) {
		using ChType = str::StringChar<decltype(source)>;

		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return { 0, str::DecResult::empty };

		return detail::ReadNextSize<ChType>(view, sourceCompleted);
	}

	/* measure the entire length of the source without fully decoding the code-points */
	constexpr str::MeasureOut Measure(const str::AnyString auto& source) {
		using ChType = str::StringChar<decltype(source)>;
		str::MeasureOut out{};
		std::basic_string_view<ChType> view{ source };

		/* iterate over the string and measure all characters */
		while (!view.empty()) {
			auto res = detail::ReadNextSize<ChType>(view, true);
			view = view.substr(res.consumed);

			out.consumed += res.consumed;
			++(res.result == str::DecResult::valid ? out.codepoints : out.invalid);
		}
		return out;
	}

	/* write the single codepoint encoded in the corresponding type as a single call to the sink (returns false
	*	and does not modify the sink if the charset cannot hold the character or the codepoint is invalid) */
	constexpr bool EncodeInto(str::AnySink auto& sink, char32_t cp) {
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
	*	same and therefore no encoding validation will be performed; if the decoding succeeded but the encoding to the sink
	*	failed, [result] will be valid but [valid] will be false and nothing will have been written to the sink) */
	constexpr str::TranscodeOut TranscodeInto(str::AnySink auto& sink, const str::AnyString auto& source, bool sourceCompleted) {
		using SourceType = str::StringChar<decltype(source)>;
		using SinkType = str::SinkChar<decltype(sink)>;

		std::basic_string_view<SourceType> view{ source };
		if (view.empty())
			return { 0, str::DecResult::empty, true };

		return detail::TranscodeNext<SourceType, SinkType>(sink, view, sourceCompleted);
	}

	/* transcodes a single codepoint into small string using str::TranscodeInto and returns it */
	template <str::IsChar ChType>
	constexpr str::TranscodeBufOut<ChType> Transcode(const str::AnyString auto& source, bool sourceCompleted) {
		str::Small<ChType, str::MaxEncodeLength> out{};
		str::TranscodeOut res = str::TranscodeInto(out, source, sourceCompleted);
		return { out, res.consumed, res.result, res.valid };
	}

	/* convert the source-string of any type and append it to the sink-string and only copy it without transcoding if the
	*	source and target type match (insert error char, if a character could not be transcoded and the error-char is not null) */
	constexpr auto& ConvertInto(str::AnySink auto& sink, const str::AnyString auto& source, char charOnError = str::CharOnError) {
		using SourceType = str::StringChar<decltype(source)>;
		using SinkType = str::SinkChar<decltype(sink)>;
		std::basic_string_view<SourceType> view{ source };

		/* check if a copy can be performed */
		if constexpr (std::is_same_v<detail::EffectiveChar<SourceType>, detail::EffectiveChar<SinkType>>) {
			sink.append(std::basic_string_view<SinkType>{ reinterpret_cast<const SinkType*>(view.data()), view.size() });
			return sink;
		}

		/* transcode all characters until the end of the string has been reached (and insert errors if error-char is not null) */
		while (!view.empty()) {
			auto [consumed, _, valid] = detail::TranscodeNext<SourceType, SinkType>(sink, view, true);
			view = view.substr(consumed);

			/* check if an error occurred and the error character should be inserted instead and write the error character
			*	to the sink (this should be rare, therefore it can be decoded and encoded properly; ignore any kind of failures) */
			if (!valid && charOnError != 0)
				detail::TranscodeNext<char, SinkType>(sink, std::basic_string_view{ &charOnError, 1 }, true);
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
