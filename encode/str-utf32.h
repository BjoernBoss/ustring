#pragma once

#include "../str-common.h"

namespace str {
	/* check expected assumptions */
	static_assert(sizeof(char32_t) == sizeof(uint32_t), "char32_t is expected to be 32bit");
	static_assert(detail::IsUtf32(U"\U00010000\U0000ff00"), "char32_t is expected to be utf-32 encoded");

	namespace detail {
		static constexpr size_t Utf32Len = 1;

		/* expect: begin != end; consumed always greater than zero */
		inline constexpr str::Decoded NextUtf32(const char32_t* cur, const char32_t* end) {
			uint32_t val = static_cast<uint32_t>(cur[0]);
			if ((val >= detail::SurrogateFirst && val <= detail::SurrogateLast) || val >= detail::UnicodeRange)
				return { str::Invalid, 1 };
			return { char32_t(val), 1 };
		}
		inline constexpr str::Decoded PrevUtf32(const char32_t* begin, const char32_t* cur) {
			uint32_t val = static_cast<uint32_t>(cur[-1]);
			if ((val >= detail::SurrogateFirst && val <= detail::SurrogateLast) || val >= detail::UnicodeRange)
				return { str::Invalid, 1 };
			return { char32_t(val), 1 };
		}
		template <class ChType = char32_t>
		constexpr bool MakeUtf32(auto&& sink, char32_t cp) {
			if (cp >= detail::SurrogateFirst && cp <= detail::SurrogateUpper)
				return false;
			if (cp >= detail::UnicodeRange)
				return false;
			str::CallSink<ChType>(sink, ChType(cp), 1);
			return true;
		}
		inline constexpr uint32_t EstimateUtf32(const char32_t* cur, const char32_t* end) {
			return 1;
		}
	}
}
