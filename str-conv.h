#pragma once

#include <string>
#include <concepts>
#include <tuple>

/*
*	string-convert namespace
*
*	Parses char/wchar_t to be unicode-encoded
*	Does not validate code-points to be valid (i.e. out-of-bounds/surrogate-pairs/overlong utf8-encoding...)
*/
namespace stc {
	using CodePoint = uint32_t;

	namespace detail {
		template <class Type, class CType>
		concept IsCharString = std::convertible_to<Type, std::basic_string_view<CType>>;

		template <class Type>
		struct CharType { using type = void; };

		template <detail::IsCharString<char> Type>
		struct CharType<Type> { using type = char; };

		template <detail::IsCharString<wchar_t> Type>
		struct CharType<Type> { using type = wchar_t; };

		template <detail::IsCharString<char8_t> Type>
		struct CharType<Type> { using type = char8_t; };

		template <detail::IsCharString<char16_t> Type>
		struct CharType<Type> { using type = char16_t; };

		template <detail::IsCharString<char32_t> Type>
		struct CharType<Type> { using type = char32_t; };

		/* expects s to contain at least one character */
		template <class Type>
		std::tuple<stc::CodePoint, size_t, bool> ReadCodePoint(const std::basic_string_view<Type>& s) {
			/* utf-8 */
			if constexpr (sizeof(Type) == 1) {
				uint8_t c8 = static_cast<uint8_t>(s[0]);
				stc::CodePoint cp = 0;
				size_t len = 1;

				if ((c8 & 0x80) == 0x00)
					return { c8, 1, true };
				if ((c8 & 0xe0) == 0xc0) {
					len = 2;
					cp = (c8 & 0x1f);
				}
				else if ((c8 & 0xf0) == 0xe0) {
					len = 3;
					cp = (c8 & 0x0f);
				}
				else if ((c8 & 0xf8) == 0xf0) {
					len = 4;
					cp = (c8 & 0x07);
				}
				else
					return { 0, 1, false };

				if (s.size() < len)
					return { c8, s.size(), false };

				for (size_t j = 1; j < len; ++j) {
					uint8_t n8 = static_cast<uint8_t>(s[j]);
					cp = (cp << 6);

					if ((n8 & 0xc0) != 0x80)
						return { c8, (j + 1), false };
					cp |= (n8 & 0x3f);
				}
				return { cp, len, true };
			}

			/* utf-16 */
			else if constexpr (sizeof(Type) == 2) {
				stc::CodePoint c32 = static_cast<uint16_t>(s[0]);

				if (c32 < 0xd800 || c32 > 0xdfff)
					return { c32, 1, true };

				if (c32 > 0xdbff || s.size() < 2)
					return { c32, 1, false };

				stc::CodePoint n32 = static_cast<uint16_t>(s[1]);
				if (n32 < 0xdc00 || n32 > 0xdfff)
					return { c32, 2, false };
				return { 0x10000 + ((c32 & 0x03ff) << 10) | (n32 & 0x03ff), 2, true };
			}

			/* utf-32 */
			else
				return { static_cast<stc::CodePoint>(s[0]), 1, true };
		}

		template <class Type>
		void WriteCodePoint(std::basic_string<Type>& s, stc::CodePoint cp) {
			/* utf-8 */
			if constexpr (sizeof(Type) == 1) {
				if (cp <= 0x7f) {
					s.push_back(static_cast<Type>(cp));
					return;
				}

				if (cp <= 0x07ff)
					s.push_back(static_cast<Type>(0xc0 | ((cp >> 6) & 0x1f)));
				else {
					if (cp <= 0xffff)
						s.push_back(static_cast<Type>(0xe0 | ((cp >> 12) & 0x0f)));
					else {
						s.push_back(static_cast<Type>(0xf0 | ((cp >> 18) & 0x07)));
						s.push_back(static_cast<Type>(0x80 | ((cp >> 12) & 0x3f)));
					}

					/* push the second to last 6 bits of the codepoint */
					s.push_back(static_cast<Type>(0x80 | ((cp >> 6) & 0x3f)));
				}

				/* push the last 6 bits of the codepoint */
				s.push_back(static_cast<Type>(0x80 | (cp & 0x3f)));
			}

			/* utf-16 */
			else if constexpr (sizeof(Type) == 2) {
				if (cp >= 0x10000) {
					cp -= 0x10000;
					s.push_back(static_cast<Type>(0xd800 + ((cp >> 10) & 0x03ff)));
					cp = 0xdc00 + (cp & 0x03ff);
				}
				s.push_back(static_cast<Type>(cp));
			}

			/* utf-32 */
			else
				s.push_back(static_cast<Type>(cp));
		}
	}

	/* is type a supported character */
	template <class Type>
	concept IsChar = std::same_as<Type, char> || std::same_as<Type, wchar_t> || std::same_as<Type, char8_t> || std::same_as<Type, char16_t> || std::same_as<Type, char32_t>;

	/* is type anything that can be converted to string-view */
	template <class Type>
	concept IsString = !std::same_as<typename detail::CharType<Type>::type, void>;

	/* get char-base of string-view this type is convertible to */
	template <stc::IsString Type>
	using CharType = typename detail::CharType<Type>::type;

	/* string-view the type is convertible to */
	template <stc::IsString Type>
	using CharView = std::basic_string_view<stc::CharType<Type>>;

	/* string the type is convertible to */
	template <stc::IsString Type>
	using CharString = std::basic_string<stc::CharType<Type>>;

	/* append the source-string of any type to the destination-type string */
	template <stc::IsChar DType, stc::IsString SType>
	std::basic_string<DType>& Append(std::basic_string<DType>& dest, SType&& source, stc::CodePoint charOnError = '?') {
		using SChar = stc::CharType<SType>;

		if constexpr (std::same_as<SType, DType>)
			dest.append(stc::CharView<SType>{ source });
		else {
			stc::CharView<SType> view{ source };

			/* fetch the code-points and append them back to the destination */
			while (!view.empty()) {
				auto [cp, len, valid] = detail::ReadCodePoint<SChar>(view);
				view = view.substr(len);

				/* check if the code-point is valid and otherwise check if the error-replacement should be inserted */
				if (valid)
					detail::WriteCodePoint<DType>(dest, cp);
				else if (charOnError != 0)
					detail::WriteCodePoint<DType>(dest, charOnError);
			}
		}
		return dest;
	}

	/* append the source-character of any type to the destination-type string */
	template <stc::IsChar DType, stc::IsChar SType>
	std::basic_string<DType>& Append(std::basic_string<DType>& dest, SType&& source, stc::CodePoint charOnError = '?') {
		return stc::Append(dest, std::basic_string_view<SType>{ &source, 1 });
	}

	/* convert any object of s-type to a string using d-type characters */
	template <stc::IsChar DType, stc::IsString SType>
	std::basic_string<DType> Conf(SType&& s) {
		std::basic_string<DType> out;
		return stc::Append(out, s);
	}

	/* convenience for fast conversion */
	std::string ToChar(stc::IsString auto&& s) {
		return stc::Conf<char>(s);
	}
	std::wstring ToWide(stc::IsString auto&& s) {
		return stc::Conf<wchar_t>(s);
	}
	std::u8string ToUtf8(stc::IsString auto&& s) {
		return stc::Conf<char8_t>(s);
	}
	std::u16string ToUtf16(stc::IsString auto&& s) {
		return stc::Conf<char16_t>(s);
	}
	std::u32string ToUtf32(stc::IsString auto&& s) {
		return stc::Conf<char32_t>(s);
	}
}
