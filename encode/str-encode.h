#pragma once

#include "../str-common-v2.h"

#include "str-utf8.h"
#include "str-utf16.h"
#include "str-utf32.h"
#include "str-wide.h"
#include "str-multibyte.h"

/*
*	All codepoints are represented as char32_t
*
*	Expected:
*		char8_t/char16_t/char32_t encodes the corresponding utf-standard
*		wchar_t encodes utf-16 or utf-32
*		char is either utf-8 or arbitrary multi-byte solution
*/
namespace str {
	namespace detail {
		template <class ChType>
		inline constexpr detail::Decoded DecodeNext(const ChType* cur, const ChType* end) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::NextUtf8(cur, end);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::NextUtf16(cur, end);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::NextUtf32(cur, end);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::NextWide(cur, end);
			else
				return detail::NextChar(cur, end);
		}
		template <class ChType>
		inline constexpr detail::Decoded DecodePrev(const ChType* begin, const ChType* cur) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::PrevUtf8(begin, cur);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::PrevUtf16(begin, cur);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::PrevUtf32(begin, cur);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::PrevWide(begin, cur);
			else
				return detail::PrevChar(begin, cur);
		}
	}

	/* [str::CPIterator] Initialized to str::Invalid; call prev/next to initialize the state
	*	Will only produce valid unicode codepoints or str::Invalid */
	template <str::IsChar ChType>
	struct Iterator {
	private:
		const ChType* pBegin = 0;
		const ChType* pCurrent = 0;
		const ChType* pEnd = 0;
		detail::Decoded pOut{};

	public:
		constexpr Iterator(const std::basic_string_view<ChType>& s = {}, size_t off = 0) {
			pBegin = s.data();
			pEnd = pBegin + s.size();
			pOut = { str::Invalid, 0 };
			if (off < s.size())
				pCurrent = pBegin + off;
			else
				pCurrent = pEnd;
		}

	public:
		constexpr bool prev() {
			if (pCurrent == pBegin)
				return false;
			pOut = detail::DecodePrev<ChType>(pBegin, pCurrent);
			pCurrent -= pOut.consumed;
			return true;
		}
		constexpr bool next() {
			const ChType* cur = pCurrent + pOut.consumed;
			if (cur >= pEnd)
				return false;
			pCurrent = cur;
			pOut = detail::DecodeNext<ChType>(pCurrent = cur, pEnd);
			return true;
		}
		constexpr char32_t get() const {
			return pOut.cp;
		}
		constexpr size_t base() const {
			return pCurrent - pBegin;
		}
		constexpr std::basic_string_view<ChType> string() const {
			return std::basic_string_view<ChType>{ pBegin, pEnd };
		}
	};

	namespace detail {
		template <class ChType>
		struct EncSize;
		template <> struct EncSize<char8_t> { static constexpr size_t value = detail::Utf8Len; };
		template <> struct EncSize<char16_t> { static constexpr size_t value = detail::Utf16Len; };
		template <> struct EncSize<char32_t> { static constexpr size_t value = detail::Utf32Len; };
		template <> struct EncSize<wchar_t> { static constexpr size_t value = detail::WideLen; };
		template <> struct EncSize<char> { static constexpr size_t value = detail::CharLen; };
	}

	/* local-string to hold the single encoded codepoint for the corresponding type */
	template <str::IsChar ChType>
	using Single = str::Local<ChType, detail::EncSize<ChType>::value>;

	/* encode single codepoint to corresponding type and return the empty-string for invalid codepoints */
	template <str::IsChar ChType>
	constexpr str::Single<ChType> Encode(char32_t cp) {
		if constexpr (std::is_same_v<ChType, char8_t>)
			return detail::MakeUtf8(cp);
		else if constexpr (std::is_same_v<ChType, char16_t>)
			return detail::MakeUtf16(cp);
		else if constexpr (std::is_same_v<ChType, char32_t>)
			return detail::MakeUtf32(cp);
		else if constexpr (std::is_same_v<ChType, wchar_t>)
			return detail::MakeWide(cp);
		else
			return detail::MakeChar(cp);
	}
}
