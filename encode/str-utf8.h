#pragma once

#include "../str-common-v2.h"

namespace str {
	/* check expected assumptions */
	static_assert(sizeof(char8_t) == sizeof(uint8_t), "char8_t is expected to be 8bit");
	static_assert(detail::IsUtf8(u8"\U0000007f\U0000ff00\U00010000"), "char8_t is expected to be utf-8 encoded");

	namespace detail {
		static constexpr size_t Utf8Len = 4;

		/* utf-8 help lookup maps (of the upper 5 bits) and boundary maps (for length) */
		static constexpr uint8_t InitCharLength[32] = {
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
			0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0
		};
		static constexpr uint8_t InitCharCodeBits[32] = {
			0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
			0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1f, 0x1f, 0x1f, 0x1f, 0x0f, 0x0f, 0x07, 0x00
		};

		/* expect: begin != end; consumed always greater than zero */
		inline constexpr detail::Decoded NextUtf8(const char8_t* cur, const char8_t* end) {
			uint8_t c8 = static_cast<uint8_t>(*cur);

			/* lookup the length of the character (with 0 invalid encoding) and extract the initial codepoint bits */
			uint32_t len = detail::InitCharLength[c8 >> 3];
			if (len == 0)
				return { str::Invalid, 1 };
			uint32_t cp = (c8 & detail::InitCharCodeBits[c8 >> 3]);

			/* validate the buffer has capacity enough */
			if (end - cur < len)
				return { str::Invalid, 1 };

			/* validate the contiunation-bytes and construct the final codepoint */
			for (uint32_t j = 1; j < len; ++j) {
				uint8_t n8 = static_cast<uint8_t>(cur[j]);
				if ((n8 & 0xc0) != 0x80)
					return { str::Invalid, 1 };
				cp = ((cp << 6) | (n8 & 0x3f));
			}

			/* check if its an invalid codepoint */
			if ((cp >= detail::SurrogateFirst && cp <= detail::SurrogateLast) || cp >= detail::UnicodeRange)
				return { str::Invalid, 1 };
			return { char32_t(cp), len };
		}
		inline constexpr detail::Decoded PrevUtf8(const char8_t* begin, const char8_t* cur) {
			uint32_t cp = 0, len = 1, shift = 0;

			/* skip all continuation bytes and accumulate their codepoint-value */
			const char8_t* c = 0;
			uint8_t c8 = 0;
			while (true) {
				c = cur - int32_t(len);
				c8 = static_cast<uint8_t>(*c);

				/* check if this is not a continuation-byte (i.e. the start of a codepoint) */
				if ((c8 & 0xc0) != 0x80)
					break;
				if (c == begin || len >= 4)
					return { str::Invalid, 1 };
				++len;
				cp |= (uint32_t(c8 & 0x3f) << shift);
				shift += 6;
			}

			/* validate the starting-byte and update the final codepoint-value (byte must exist, as the loop would
			*	otherwise have returned an invalid codepoint; will also catch act of 0, as len will never be 0) */
			uint32_t act = detail::InitCharLength[c8 >> 3];
			if (act != len)
				return { str::Invalid, 1 };
			cp |= (uint32_t(c8 & detail::InitCharCodeBits[c8 >> 3]) << shift);

			/* check if its an invalid codepoint */
			if ((cp >= detail::SurrogateFirst && cp <= detail::SurrogateLast) || cp >= detail::UnicodeRange)
				return { str::Invalid, 1 };
			return { char32_t(cp), len };
		}
		template <class ChType = char8_t, size_t Len = detail::Utf8Len>
		inline constexpr str::Local<ChType, Len> MakeUtf8(char32_t cp) {
			/* check if its a single character, which can just be pushed */
			if (cp <= 0x7f)
				return { ChType(cp) };

			/* determine the length of the character */
			str::Local<ChType, Len> out;
			uint32_t shift = 0;
			if (cp <= 0x07ff) {
				out.push_back(static_cast<ChType>(0xc0 | (cp >> 6)));
				shift = 6;
			}
			else if (cp >= detail::SurrogateFirst && cp <= detail::SurrogateLast)
				return out;
			else if (cp <= 0xffff) {
				out.push_back(static_cast<ChType>(0xe0 | (cp >> 12)));
				shift = 12;
			}
			else if (cp < detail::UnicodeRange) {
				out.push_back(static_cast<ChType>(0xf0 | (cp >> 18)));
				shift = 18;
			}
			else
				return out;

			/* produce the continuation bytes */
			while (shift > 0) {
				shift -= 6;
				out.push_back(static_cast<ChType>(0x80 | ((cp >> shift) & 0x3f)));
			}
			return out;
		}
	}
}
