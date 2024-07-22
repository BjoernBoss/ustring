#pragma once

#include "../str-common.h"

#include "str-utf16.h"
#include "str-utf32.h"

namespace str {
	/* [character cannot be represented in current code page; expression is never evaluated and might have side effects] */
#pragma warning(push)
#pragma warning(disable : 4566 6237)
	static constexpr bool WideIsUtf16 = (sizeof(wchar_t) == sizeof(char16_t) && detail::IsUtf16(L"\U00010000\U0000ff00"));
	static constexpr bool WideIsUtf32 = (sizeof(wchar_t) == sizeof(char32_t) && detail::IsUtf32(L"\U00010000\U0000ff00"));
#pragma warning(pop)

	/* check expected assumptions */
	static_assert(sizeof(wchar_t) == sizeof(uint16_t) || sizeof(wchar_t) == sizeof(uint32_t), "wchar_t is expected to be at least 16bit and at most 32bit");
	static_assert(str::WideIsUtf16 || str::WideIsUtf32, "wchar_t is expected to be utf-16 or utf-32 encoded");

	namespace detail {
		static constexpr size_t WideLen = (str::WideIsUtf16 ? detail::Utf16Len : detail::Utf32Len);

		/* expect: begin != end; consumed always greater than zero */
		template <bool AllowIncomplete>
		inline constexpr str::Decoded NextWide(const wchar_t* cur, const wchar_t* end) {
			if constexpr (str::WideIsUtf16)
				return detail::NextUtf16<wchar_t, AllowIncomplete>(cur, end);
			else
				return detail::NextUtf32<wchar_t>(cur, end);
		}
		inline constexpr str::Decoded PrevWide(const wchar_t* begin, const wchar_t* cur) {
			if constexpr (str::WideIsUtf16)
				return detail::PrevUtf16<wchar_t>(begin, cur);
			else
				return detail::PrevUtf32<wchar_t>(begin, cur);
		}
		inline constexpr bool MakeWide(auto&& sink, char32_t cp) {
			if constexpr (str::WideIsUtf16)
				return detail::MakeUtf16<wchar_t>(sink, cp);
			else
				return detail::MakeUtf32<wchar_t>(sink, cp);
		}
		inline constexpr uint32_t EstimateWide(const wchar_t* cur, const wchar_t* end) {
			if constexpr (str::WideIsUtf16)
				return detail::EstimateUtf16<wchar_t>(cur, end);
			else
				return detail::EstimateUtf32<wchar_t>(cur, end);
		}
	}
}
