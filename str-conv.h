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
	/* maximum number of characters to potentially be consumed/produced per codepoint */
	static constexpr size_t MaxEncodeLength = 4;

	using CodePoint = uint32_t;

	namespace detail {
		template <class Type, class CType>
		concept StringViewFromChar = std::convertible_to<Type, std::basic_string_view<CType>>;

		template <class Type>
		struct CharType { using type = void; };

		template <detail::StringViewFromChar<char> Type>
		struct CharType<Type> { using type = char; };

		template <detail::StringViewFromChar<wchar_t> Type>
		struct CharType<Type> { using type = wchar_t; };

		template <detail::StringViewFromChar<char8_t> Type>
		struct CharType<Type> { using type = char8_t; };

		template <detail::StringViewFromChar<char16_t> Type>
		struct CharType<Type> { using type = char16_t; };

		template <detail::StringViewFromChar<char32_t> Type>
		struct CharType<Type> { using type = char32_t; };
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

	/* string-view for the char-type */
	template <stc::IsChar Type>
	using View = std::basic_string_view<Type>;

	/* string-view the type can be converted to */
	template <stc::IsString Type>
	using ViewFromStr = std::basic_string_view<stc::CharType<Type>>;

	/* string for the char-type */
	template <stc::IsChar Type>
	using String = std::basic_string<Type>;

	/* string the type can be converted to */
	template <stc::IsString Type>
	using StringFromStr = std::basic_string<stc::CharType<Type>>;

	/* type supports writing single characters or sequences to */
	template <class Type>
	concept IsWritable = requires(Type t) {
		{ t } -> stc::IsString;
		{ t.push_back(stc::CharType<Type>()) };
		{ t.append(stc::ViewFromStr<Type>()) };
	};

	namespace detail {
		/* expects s to contain at least one character */
		template <class Type>
		std::tuple<stc::CodePoint, size_t, bool> ReadCodePoint(const stc::View<Type>& s) {
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

		template <stc::IsWritable Type>
		void WriteCodePoint(Type& s, stc::CodePoint cp) {
			using CType = stc::CharType<Type>;

			/* utf-8 */
			if constexpr (sizeof(CType) == 1) {
				if (cp <= 0x7f) {
					s.push_back(static_cast<CType>(cp));
					return;
				}

				if (cp <= 0x07ff)
					s.push_back(static_cast<CType>(0xc0 | ((cp >> 6) & 0x1f)));
				else {
					if (cp <= 0xffff)
						s.push_back(static_cast<CType>(0xe0 | ((cp >> 12) & 0x0f)));
					else {
						s.push_back(static_cast<CType>(0xf0 | ((cp >> 18) & 0x07)));
						s.push_back(static_cast<CType>(0x80 | ((cp >> 12) & 0x3f)));
					}

					/* push the second to last 6 bits of the codepoint */
					s.push_back(static_cast<CType>(0x80 | ((cp >> 6) & 0x3f)));
				}

				/* push the last 6 bits of the codepoint */
				s.push_back(static_cast<CType>(0x80 | (cp & 0x3f)));
			}

			/* utf-16 */
			else if constexpr (sizeof(CType) == 2) {
				if (cp >= 0x10000) {
					cp -= 0x10000;
					s.push_back(static_cast<CType>(0xd800 + ((cp >> 10) & 0x03ff)));
					cp = 0xdc00 + (cp & 0x03ff);
				}
				s.push_back(static_cast<CType>(cp));
			}

			/* utf-32 */
			else
				s.push_back(static_cast<CType>(cp));
		}

		template <stc::IsWritable Type>
		void TryWriteErrorChar(Type& s, char c) {
			/* will not be written if its a null-byte */
			if (c == 0)
				return;

			/* this should be rare, therefore it can be decoded and encoded properly (ignore any kind of failures) */
			auto [cp, _, valid] = detail::ReadCodePoint<char>(std::string_view{ &c, 1 });
			if (valid)
				detail::WriteCodePoint(s, cp);
		}
	}

	/* append the source-string of any type to the destination-type string */
	template <stc::IsWritable DType, stc::IsString SType>
	DType& Append(DType& dest, SType&& source, char charOnError = '?') {
		using SChar = stc::CharType<SType>;

		if constexpr (std::same_as<SType, DType>)
			dest.append(stc::View<SChar>{ source });
		else {
			stc::View<SChar> view{ source };

			/* fetch the code-points and append them back to the destination */
			while (!view.empty()) {
				auto [cp, len, valid] = detail::ReadCodePoint<SChar>(view);
				view = view.substr(len);

				/* check if the code-point is valid and otherwise try to write the error-char */
				if (valid)
					detail::WriteCodePoint<DType>(dest, cp);
				else
					detail::TryWriteErrorChar(dest, charOnError);
			}
		}
		return dest;
	}

	/* append the source-character of any type to the destination-type string */
	template <stc::IsWritable DType, stc::IsChar SType>
	DType& Append(DType& dest, SType&& source, char charOnError = '?') {
		return stc::Append(dest, stc::View<SType>{ &source, 1 }, charOnError);
	}

	/* read a single codepoint from the beginning of the string (return: code-point, num-consumed, is-valid) */
	template <stc::IsString Type>
	std::tuple<stc::CodePoint, size_t, bool> Decode(Type&& source) {
		stc::ViewFromStr<Type> view{ source };
		if (view.empty())
			return { 0, 0, true };
		return detail::ReadCodePoint(view);
	}

	/* return a string containing the single codepoint encoded in the corresponding type
	*	(size is small enough such that basic_string should not allocate; max: stc::MaxEncodeLength) */
	template <stc::IsChar Type>
	stc::String<Type> Encode(stc::CodePoint cp) {
		stc::String<Type> out{};
		detail::WriteCodePoint(out, cp);
		return out;
	}

	/* return a string containing the next character from the source encoded in the corresponding type
	*	(empty-string returned on error; size is small enough such that basic_string should not allocate; max: stc::MaxEncodeLength) */
	template <stc::IsChar DType, stc::IsString SType>
	std::tuple<stc::String<DType>, size_t> Transcode(SType&& source, char charOnError = '?') {
		stc::String<DType> out{};

		/* cannot just copy first value on identical char-types, as it might require multiple
		*	chars (like utf8 made from 3 chars) and it has to be validated */
		stc::ViewFromStr<SType> view{ source };
		if (view.empty())
			return { out, 0 };

		/* decode the next codepoint (and check if an error occurred) */
		auto [cp, len, valid] = detail::ReadCodePoint(view);
		if (!valid) {
			detail::TryWriteErrorChar(out, charOnError);
			return { out, len };
		}

		/* check if the types are the same, in which case the cp does not need to be transcoded, but the length can just be copied */
		if constexpr (std::same_as<DType, stc::CharType<SType>>)
			out.append(view.substr(0, len));
		else
			detail::WriteCodePoint(out, cp);
		return { out, len };
	}

	/* return a string containing the byte-order-mark encoded in the corresponding type (empty if
	*	not utf8/utf16/utf32; size is small enough such that basic_string should not allocate) */
	template <stc::IsChar Type>
	stc::String<Type> BOM() {
		stc::String<Type> out{};
		if constexpr (std::same_as<Type, char8_t> || std::same_as<Type, char16_t> || std::same_as<Type, char32_t>)
			detail::WriteCodePoint<Type>(out, 0xfeff);
		return out;
	}

	/* convert any object of s-type to a string using d-type characters */
	template <stc::IsChar DType, stc::IsString SType>
	stc::String<DType> Conf(SType&& s) {
		stc::String<DType> out{};
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
