#pragma once

#include "../str-common-v2.h"

namespace str {
	/* check expected assumptions */
	static_assert(sizeof(char16_t) == sizeof(uint16_t), "char16_t is expected to be 16bit");
	static_assert(detail::IsUtf16(u"\U00010000\U0000ff00"), "char16_t is expected to be utf-16 encoded");

	namespace detail {
		static constexpr size_t Utf16Len = 2;

		/* expect: begin != end; consumed always greater than zero */
		template <bool AllowIncomplete>
		inline constexpr str::Decoded NextUtf16(const char16_t* cur, const char16_t* end) {
			uint32_t first = static_cast<uint16_t>(*cur);

			/* check if its a valid single-token codepoint (valid at all times) */
			if (first < detail::SurrogateFirst || first > detail::SurrogateLast)
				return { first, 1 };

			/* ensure that the first value is valid and there are enough characters for a surrogate-pair */
			if (first >= detail::SurrogateUpper)
				return { str::Invalid, 1 };
			if (end - cur < 2) {
				if constexpr (AllowIncomplete)
					return { str::Invalid, 0 };
				return { str::Invalid, 1 };
			}

			/* extract and validate the second token */
			uint32_t second = static_cast<uint16_t>(cur[1]);
			if (second < detail::SurrogateUpper || second > detail::SurrogateLast)
				return { str::Invalid, 1 };

			/* decode the overall codepoint (cannot produce any invalid codepoints) */
			return { char32_t(0x10000 + ((first & 0x03ff) << 10) | (second & 0x03ff)), 2 };
		}
		inline constexpr str::Decoded PrevUtf16(const char16_t* begin, const char16_t* cur) {
			/* check if its a single codepoint */
			uint32_t second = static_cast<uint16_t>(cur[-1]);
			if (second < detail::SurrogateFirst || second > detail::SurrogateLast)
				return { second, 1 };

			/* ensure there are enough characters for a surrogate-pair and that the second value is valid */
			if (second < detail::SurrogateUpper || cur - begin < 2)
				return { str::Invalid, 1 };

			/* extract and validate the first token */
			uint32_t first = static_cast<uint16_t>(cur[-2]);
			if (first < detail::SurrogateFirst || first >= detail::SurrogateUpper)
				return { str::Invalid, 1 };

			/* decode the overall codepoint (cannot produce any invalid codepoints) */
			return { char32_t(0x10000 + ((first & 0x03ff) << 10) | (second & 0x03ff)), 2 };
		}
		template <class ChType = char16_t>
		inline constexpr bool MakeUtf16(auto&& sink, char32_t cp) {
			/* check if its a single utf16-token */
			if (cp < 0x10000) {
				if (cp >= detail::SurrogateFirst && cp <= detail::SurrogateUpper)
					return false;
				str::CallSink<ChType>(sink, ChType(cp), 1);
				return true;
			}
			if (cp >= detail::UnicodeRange)
				return false;
			cp -= 0x10000;

			/* produce the two utf16-token and write them to the sink */
			ChType out[detail::Utf16Len] = { 0 };
			out[0] = static_cast<ChType>(detail::SurrogateFirst + (cp >> 10));
			out[1] = static_cast<ChType>(detail::SurrogateUpper + (cp & 0x03ff));
			str::CallSink<ChType>(sink, std::basic_string_view<ChType>{ out, out + 2 });
			return true;
		}
	}
}
