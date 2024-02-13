#pragma once

#include "str-common.h"

#include <string>
#include <concepts>
#include <utility>

/*
*	Parses char/wchar_t to be unicode-encoded
*/
namespace str {
	static constexpr char CharOnError = '?';

	/* maximum number of characters to potentially be consumed/produced per codepoint */
	static constexpr size_t MaxEncodeLength = 4;

	using CodePoint = uint32_t;

	/* check if this is a codepoint defined as valid by the unicode standard (does not check internally reserved codes, just overall bounds) */
	inline constexpr bool ValidCodePoint(str::CodePoint cp) {
		if (cp > 0x10ffff)
			return false;
		return (cp < 0xd800 || cp > 0xdfff);
	}

	enum class DecodeResult : uint8_t {
		valid,
		empty,
		encoding,
		strictness,
		incomplete
	};
	struct DecodeOut {
		str::CodePoint cp = 0;
		uint8_t consumed = 0;
		str::DecodeResult result = str::DecodeResult::empty;
	};

	namespace detail {
		/* default-implementation for utf-8/utf-16/utf-32, which can write any valid codepoint */
		template <class ChType>
		struct CanWriteChar {
			constexpr bool operator()(ChType c) const {
				return true;
			}
		};
		template <>
		struct CanWriteChar<char> {
			constexpr bool operator()(char c) const {
				return true;
			}
		};
		template <>
		struct CanWriteChar<wchar_t> {
			constexpr bool operator()(wchar_t c) const {
				return true;
			}
		};

		template <class ChType, bool Strict>
		str::DecodeOut ReadUtf8(const auto& source, size_t off, size_t size) {
			static constexpr str::CodePoint OverlongBound[4] = { 0x00'007f, 0x00'07ff, 0x00'ffff, 0x1f'ffff };
			uint8_t c8 = static_cast<uint8_t>(source[off]);

			/* check if its a single byte and validate the overlong boundary (although only out of principle) */
			if ((c8 & 0x80) == 0x00) {
				if (Strict && c8 > OverlongBound[0])
					return { 0, 1, str::DecodeResult::strictness };
				return { c8, 1, str::DecodeResult::valid };
			}

			/* extract the length of the encoding */
			str::CodePoint cp = 0;
			uint8_t len = 1;
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
				return { 0, 1, str::DecodeResult::encoding };

			/* validate the buffer has capacity enough */
			if (size - off < len) {
				return { 0, 0, str::DecodeResult::incomplete };
			}

			/* validate the contiunation-bytes and construct the final code-point */
			for (uint8_t j = 1; j < len; ++j) {
				uint8_t n8 = static_cast<uint8_t>(source[off + j]);
				if ((n8 & 0xc0) != 0x80)
					return { 0, j, str::DecodeResult::encoding };
				cp = ((cp << 6) | (n8 & 0x3f));
			}

			/* check if this is a strict decoding and check the overlong bondaries */
			if (Strict && cp > OverlongBound[len - 1])
				return { 0, len, str::DecodeResult::strictness };
			return { cp, len, str::DecodeResult::valid };
		}
		template <class ChType>
		str::DecodeOut ReadUtf16(const auto& source, size_t off, size_t size) {
			str::CodePoint cp = static_cast<uint16_t>(source[off]);

			/* check if its a valid single-token codepoint */
			if (cp < 0xd800 || cp > 0xdfff)
				return { cp, 1, str::DecodeResult::valid };

			/* ensure there are enough characters for a surrogate-pair and that the first value is valid */
			if (cp > 0xdbff)
				return { cp, 1, str::DecodeResult::encoding };
			if (size - off < 2)
				return { cp, 0, str::DecodeResult::incomplete };

			/* extract and validate the second token */
			str::CodePoint next = static_cast<uint16_t>(source[off + 1]);
			if (next < 0xdc00 || next > 0xdfff)
				return { cp, 2, str::DecodeResult::encoding };

			/* decode the overall code-point */
			return { 0x10000 + ((cp & 0x03ff) << 10) | (next & 0x03ff), 2, str::DecodeResult::valid };
		}
		template <class ChType>
		str::DecodeOut ReadUtf32(const auto& source, size_t off, size_t size) {
			return { static_cast<str::CodePoint>(source[off]), 1, str::DecodeResult::valid };
		}

		/* expect s to contain at least one character (will only return consumed:0 on incomplete chars) */
		template <class ChType, bool Strict>
		str::DecodeOut ReadCodePoint(const auto& source, size_t off, size_t size) {
			str::DecodeOut res{};

			/* decode the next codepoint */
			if constexpr (std::same_as<ChType, char> || std::same_as<ChType, char8_t> || (std::same_as<ChType, wchar_t> && sizeof(wchar_t) == 1))
				res = str::detail::ReadUtf8<ChType, Strict>(source, off, size);
			else if constexpr (std::same_as<ChType, char16_t> || (std::same_as<ChType, wchar_t> && sizeof(wchar_t) == 2))
				res = str::detail::ReadUtf16<ChType>(source, off, size);
			else
				res = str::detail::ReadUtf32<ChType>(source, off, size);

			/* validate the codepoint itself */
			if (Strict && res.result == str::DecodeResult::valid && !str::ValidCodePoint(res.cp))
				return { 0, res.consumed, str::DecodeResult::strictness };
			return res;
		}

		template <class ChType>
		void WriteUtf8(auto& sink, str::CodePoint cp) {
			/* check if a single character fits */
			if (cp <= 0x7f) {
				sink.push_back(static_cast<ChType>(cp));
				return;
			}

			/* write the first 1-3 characters out */
			if (cp <= 0x07ff)
				sink.push_back(static_cast<ChType>(0xc0 | ((cp >> 6) & 0x1f)));
			else {
				if (cp <= 0xffff)
					sink.push_back(static_cast<ChType>(0xe0 | ((cp >> 12) & 0x0f)));
				else {
					sink.push_back(static_cast<ChType>(0xf0 | ((cp >> 18) & 0x07)));
					sink.push_back(static_cast<ChType>(0x80 | ((cp >> 12) & 0x3f)));
				}

				/* push the second to last 6 bits of the codepoint */
				sink.push_back(static_cast<ChType>(0x80 | ((cp >> 6) & 0x3f)));
			}

			/* push the last 6 bits of the codepoint */
			sink.push_back(static_cast<ChType>(0x80 | (cp & 0x3f)));
		}
		template <class ChType>
		void WriteUtf16(auto& sink, str::CodePoint cp) {
			if (cp >= 0x10000) {
				cp -= 0x10000;
				sink.push_back(static_cast<ChType>(0xd800 + ((cp >> 10) & 0x03ff)));
				cp = 0xdc00 + (cp & 0x03ff);
			}
			sink.push_back(static_cast<ChType>(cp));
		}
		template <class ChType>
		void WriteUtf32(auto& sink, str::CodePoint cp) {
			sink.push_back(static_cast<ChType>(cp));
		}

		template <class ChType, bool Strict>
		bool WriteCodePoint(auto& sink, str::CodePoint cp) {
			/* check if the codepoint is invalid or cannot be encoded */
			if (Strict && !str::ValidCodePoint(cp))
				return false;
			if (!detail::CanWriteChar<ChType>{}(cp))
				return false;

			/* encode the codepoint */
			if constexpr (std::same_as<ChType, char> || std::same_as<ChType, char8_t> || (std::same_as<ChType, wchar_t> && sizeof(wchar_t) == 1))
				str::detail::WriteUtf8<ChType>(sink, cp);
			else if constexpr (std::same_as<ChType, char16_t> || (std::same_as<ChType, wchar_t> && sizeof(wchar_t) == 2))
				str::detail::WriteUtf16<ChType>(sink, cp);
			else
				str::detail::WriteUtf32<ChType>(sink, cp);
			return true;
		}

		template <class ChType>
		void TryWriteErrorChar(auto& s, char c) {
			/* will not be written if its a null-byte */
			if (c == 0)
				return;

			/* this should be rare, therefore it can be decoded and encoded properly (ignore any kind of failures) */
			auto [cp, _, res] = str::detail::ReadCodePoint<ChType, true>(&c, 0, 1);
			if (res == str::DecodeResult::valid)
				str::detail::WriteCodePoint<ChType, true>(s, cp);
		}
	}

	/* check if the character can be encoded by the corresponding character */
	template <str::IsChar ChType>
	constexpr bool CanWrite(ChType c) {
		return detail::CanWriteChar<ChType>{}(c);
	}

	/* read a single codepoint from the beginning of the string (if strictness is required,
	*	any overlong utf8-chars or invalid codepoints will result in strictness-errors) */
	template <bool Strict = true>
	str::DecodeOut Decode(const str::AnyString auto& source) {
		using ChType = str::StringChar<decltype(source)>;

		size_t size = detail::GetSize<ChType>(source);
		if (size == 0)
			return { 0, 0, str::DecodeResult::empty };
		return detail::ReadCodePoint<ChType, Strict>(source, 0, size);
	}

	/* write the single codepoint encoded in the corresponding type to the sink (returns the false and does not modify
	*	the sink if strictness is enabled and the codepoint is invalid, or the charset cannot hold the character) */
	template <bool Strict = true>
	bool Encode(str::AnySink auto& sink, str::CodePoint cp) {
		using ChType = str::SinkChar<decltype(sink)>;

		/* write the codepoint to the output (will automatically check for errors) */
		return detail::WriteCodePoint<ChType>(sink, cp);
	}

	/* return a string containing the single codepoint encoded in the corresponding type (returns the empty string
	*	if strictness is enabled and the codepoint is invalid, or the charset cannot hold the character) */
	template <str::IsChar ChType, bool Strict = true>
	str::Small<ChType, str::MaxEncodeLength> Encode(str::CodePoint cp) {
		str::Small<ChType, str::MaxEncodeLength> out{};

		/* return-value can be ignored, as string will remain empty */
		str::Encode<Strict>(out, cp);
		return out;
	}

	/* read a single codepoint from the source and transcode it to the sink (returns number of characters consumed from the source,
	*	0 on empty source or incomplete character, and write the error char, if any error occurred and error-char is not null) */
	template <bool Strict = true>
	size_t Transcode(str::AnySink auto& sink, const str::AnyString auto& source, char charOnError = str::CharOnError) {
		/* cannot just copy first value on identical char-types, as it might require multiple
		*	chars (like utf8 made from 3 chars) and it has to be validated */
		using SChType = str::StringChar<decltype(source)>;
		using DChType = str::SinkChar<decltype(sink)>;

		/* check if the source is empty, in which case nothing can be transcoded */
		size_t size = detail::GetSize<SChType>(source);
		if (size == 0)
			return 0;

		/* decode the next character and check if its incomplete, in which case nothing will be done */
		str::DecodeOut out = detail::ReadCodePoint<SChType, Strict>(source, 0, size);
		if (out.result == str::DecodeResult::incomplete)
			return 0;

		/* check if an error occurred, in which case the char-error will be written */
		if (out.result != str::DecodeResult::valid) {
			detail::TryWriteErrorChar<DChType>(sink, charOnError);
			return out.consumed;
		}

		/* check if the source and destination are of the same type, in which case the characters can just be copied */
		if constexpr (std::same_as<SChType, DChType>) {
			for (size_t i = 0; i < out.consumed; ++i)
				sink.push_back(source[i]);
			return out.consumed;
		}

		/* try to write the codepoint to the destination and otherwise write the error-token and return the consumed number of token */
		else if (!detail::WriteCodePoint<DChType, Strict>(sink, out.cp))
			detail::TryWriteErrorChar<DChType>(sink, charOnError);
		return out.consumed;
	}

	/* return a string containing the transcoded next codepoint from the source (returns number of characters consumed from the source,
	*	0 on empty source or incomplete character, and write the error char, if any error occurred and error-char is not null) */
	template <str::IsChar ChType, bool Strict = true>
	std::pair<str::Small<ChType, str::MaxEncodeLength>, size_t> Transcode(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		str::Small<ChType, str::MaxEncodeLength> out{};
		size_t consumed = str::Transcode<Strict>(out, source, charOnError);
		return { out, consumed };
	}

	/* append the source-string of any type to the sink-string (append error char, if last is incomplete and error-char is not null) */
	template <bool Strict = true>
	auto& Append(str::AnySink auto& sink, const str::AnyString auto& source, char charOnError = str::CharOnError) {
		using SChType = str::StringChar<decltype(source)>;
		using DChType = str::SinkChar<decltype(sink)>;
		std::basic_string_view<SChType> view{ source };

		/* transcode all characters until a zero is returned (which implies last incomplete) */
		while (!view.empty()) {
			size_t consumed = str::Transcode<Strict>(sink, view, charOnError);
			if (consumed == 0)
				break;
			view = view.substr(consumed);
		}

		/* check if the view is not yet empty, in which case the last character was incomplete (and therefore add an error-character) */
		if (!view.empty())
			detail::TryWriteErrorChar<DChType>(sink, charOnError);
		return sink;
	}

	/* append the source-character of any type to the destination-type string */
	template <bool Strict = true>
	auto& Append(str::AnySink auto& sink, str::IsChar auto val, char charOnError = str::CharOnError) {
		return str::Append<Strict>(sink, std::basic_string_view<decltype(val)>{ &val, 1 }, charOnError);
	}

	/* return a string containing the byte-order-mark encoded in the corresponding type (empty if not utf8/utf16/utf32) */
	template <str::IsChar ChType>
	str::Small<ChType, str::MaxEncodeLength> BOM() {
		str::Small<ChType, str::MaxEncodeLength> out{};
		if constexpr (std::same_as<ChType, char8_t> || std::same_as<ChType, char16_t> || std::same_as<ChType, char32_t>)
			detail::WriteCodePoint<ChType, true>(out, 0xfeff);
		return out;
	}

	/* convert any object of s-type to a string using d-type characters */
	template <str::IsChar ChType, bool Strict = true>
	std::basic_string<ChType> Conf(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		std::basic_string<ChType> out{};
		return str::Append<Strict>(out, source, charOnError);
	}

	/* convenience for fast conversion */
	template <bool Strict = true>
	std::string ToChar(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conf<char, Strict>(source, charOnError);
	}
	template <bool Strict = true>
	std::wstring ToWide(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conf<wchar_t, Strict>(source, charOnError);
	}
	template <bool Strict = true>
	std::u8string ToUtf8(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conf<char8_t, Strict>(source, charOnError);
	}
	template <bool Strict = true>
	std::u16string ToUtf16(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conf<char16_t, Strict>(source, charOnError);
	}
	template <bool Strict = true>
	std::u32string ToUtf32(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conf<char32_t, Strict>(source, charOnError);
	}
}
