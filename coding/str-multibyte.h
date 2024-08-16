/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "../str-common.h"
#include "../str-local.h"

#include "str-utf8.h"
#include "str-wide.h"

namespace str {
	/* [character cannot be represented in current code page] */
#pragma warning(push)
#pragma warning(disable : 4566)
	static constexpr bool CharIsUtf8 = detail::IsUtf8("\U0000007f\U0000ff00\U00010000");
#pragma warning(pop)

	/* [expression is never evaluated and might have side effects] */
#pragma warning(push)
#pragma warning(disable : 6286)
	static constexpr bool CharHoldsAscii = (str::CharIsUtf8 || detail::HoldSameValues(
		U"\0\001\002\003\004\005\006\a\b\t\n\v\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
		U" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177",
		"\0\001\002\003\004\005\006\a\b\t\n\v\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
		" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177"));
#pragma warning(pop)

	/* check expected assumptions */
	static_assert(sizeof(char) == sizeof(uint8_t), "char is expected to be 8bit");

	namespace detail {
		/* also limited to utf-8-length to give it a hard upper-limit, despite it not being specified */
		static constexpr size_t CharLen = detail::Utf8Len;

		/* expect: begin != end; consumed always greater than zero */
		template <bool AllowIncomplete>
		inline constexpr str::Decoded NextChar(const char* cur, const char* end) {
			/* utf-8 path */
			if constexpr (str::CharIsUtf8)
				return detail::NextUtf8<char, AllowIncomplete>(cur, end);
			else {
				/* fast-path for ascii-characters */
				if (str::CharHoldsAscii && uint32_t(*cur) <= detail::AsciiRange)
					return { char32_t(*cur), 1 };

				/* first decode multi-byte to wide-chars (as the standard libraries functions
				*	to utf32 only work for utf8 inputs, at least on some msvc versions) */
				std::mbstate_t state{ 0 };
				wchar_t wc = 0;

				/* read the next character and check if the character is incomplete (documentation suggests that a decoded codepoint should
				*	always fit into single wide-char, and fail if the max encoding length promise would be broken => should not be possible) */
				size_t res = std::mbrtowc(&wc, cur, size_t(end - cur), &state);

				/* check if the character is invalid or considered incomplete and otherwise
				*	fetch the length (1 for null-byte, otherwise directly equals to the result) */
				if (res == static_cast<size_t>(-1))
					return { str::Invalid, 1 };
				if (res == static_cast<size_t>(-2)) {
					/* ensure the max character-length promise is not broken */
					if (AllowIncomplete && size_t(end - cur) < detail::CharLen)
						return { str::Invalid, 0 };
					return { str::Invalid, 1 };
				}
				uint32_t len = (res == 0 ? 1 : uint32_t(res));

				/* check if the char is longer than the internal max character-length, which should
				*	really not happen in any encoding and would break the promise of str::Encoded */
				if (len > detail::CharLen)
					return { str::Invalid, 1 };

				/* convert the wide-character to the final codepoint (consumed can
				*	be discarded as it will either be 1 or 0 and str::Invalid returned) */
				char32_t cp = detail::NextWide<false>(&wc, &wc + 1).cp;
				if (cp == str::Invalid)
					return { str::Invalid, 1 };
				return { cp, len };
			}
		}
		inline constexpr str::Decoded PrevChar(const char* begin, const char* cur) {
			/* utf-8 path */
			if constexpr (str::CharIsUtf8)
				return detail::PrevUtf8<char>(begin, cur);
			else {
				/* fast-path for ascii-characters */
				if (str::CharHoldsAscii && uint32_t(cur[-1]) <= detail::AsciiRange)
					return { char32_t(cur[-1]), 1 };

				/* first decode multi-byte to wide-chars (as the standard libraries functions
				*	to utf32 only work for utf8 inputs, at least on some msvc versions) */
				wchar_t wc = 0;

				/* iteratively increase the length until a valid codepoint is encountered */
				size_t len = 1, max = std::min<size_t>(detail::CharLen, cur - begin);
				while (true) {
					std::mbstate_t state{ 0 };
					size_t res = std::mbrtowc(&wc, cur - len, len, &state);

					/* check if the character is still considered invalid (probably not a valid start) */
					if (res == static_cast<size_t>(-1)) {
						if (len >= max)
							return { str::Invalid, 1 };
						++len;
						continue;
					}

					/* check if the character is considered incomplete, in which case the next character must be considered invalid */
					if (res == static_cast<size_t>(-2))
						return { str::Invalid, 1 };

					/* ensure that the length matches the actual length */
					if (len != (res == 0 ? 1 : res))
						return { str::Invalid, 1 };
					break;
				}

				/* convert the wide-character to the final codepoint (consumed can
				*	be discarded as it will either be 1 or 0 and str::Invalid returned) */
				char32_t cp = detail::NextWide<false>(&wc, &wc + 1).cp;
				if (cp == str::Invalid)
					return { str::Invalid, 1 };
				return { cp, uint32_t(len) };
			}
		}
		inline constexpr bool MakeChar(auto&& sink, char32_t cp) {
			/* utf-8 path */
			if constexpr (str::CharIsUtf8)
				return detail::MakeUtf8<char>(sink, cp);
			else {
				/* fast-path for ascii-characters */
				if (str::CharHoldsAscii && uint32_t(cp) <= detail::AsciiRange) {
					str::CallSink(sink, char(cp), 1);
					return true;
				}

				/* convert the codepoint first to wide-characters and check if it only resulted in one (will also catch encoding-errors),
				*	as a single multi-byte char must originate from exactly 1 wide char (also expected by read-multibyte) */
				str::Local<wchar_t, detail::WideLen> wc;
				detail::MakeWide(wc, cp);
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
				if (res == static_cast<size_t>(-1) || res > detail::CharLen)
					return false;

				/* write the characters to the sink (res should never be zero) */
				if (res == 1)
					str::CallSink(sink, char(buf[0]), 1);
				else
					str::CallSink(sink, buf);
				return true;
			}
		}
		inline constexpr uint32_t EstimateChar(const char* cur, const char* end) {
			/* utf-8 path */
			if constexpr (str::CharIsUtf8)
				return detail::EstimateUtf8<char>(cur, end);
			else {
				/* fast-path for ascii-characters */
				if (str::CharHoldsAscii && uint32_t(*cur) <= detail::AsciiRange)
					return 1;

				/* read the next character and check if the character is incomplete (documentation suggests that a decoded codepoint should
				*	always fit into single wide-char, and fail if the max encoding length promise would be broken => should not be possible) */
				std::mbstate_t state{ 0 };
				size_t res = std::mbrtowc(0, cur, size_t(end - cur), &state);

				/* check if the character is invalid or considered incomplete and otherwise
				*	fetch the length (1 for null-byte, otherwise directly equals to the result) */
				if (res == static_cast<size_t>(-1) || res == static_cast<size_t>(-2))
					return 0;
				uint32_t len = (res == 0 ? 1 : uint32_t(res));

				/* check if the char is longer than the internal max character-length, which should
				*	really not happen in any encoding and would break the promise of str::Encoded */
				if (len > detail::CharLen)
					return 0;
				return len;
			}
		}
	}
}
