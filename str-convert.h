#pragma once

#include "str-common.h"

#include <string>
#include <concepts>
#include <utility>
#include <cwchar>
#include <type_traits>

/*
*	char8_t/char16_t/char32_t must be their respective unicode-encodings
*	wchar_t must be a unicode-encoding
*	char can be multi-byte using the current locale or any unicode-encoding
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

	/*
	*	valid: valid decoded codepoint
	*	empty: source was empty and therefore nothing decoded
	*	encoding: encoding error was detected in source
	*	strictness: encoded codepoint is invalid or did not use the proper encoding-form
	*	incomplete: codepoint seems to be encoded properly, but missing one or more characters for completion
	*/
	enum class DecResult : uint8_t {
		valid,
		empty,
		encoding,
		strictness,
		incomplete
	};

	struct DecodeOut {
		size_t consumed = 0;
		str::CodePoint cp = 0;
		str::DecResult result = str::DecResult::empty;
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

		/* extract effective character (char/char8_t/char16_t/char32_t) */
		template <class Type>
		using EffectiveChar = std::conditional_t<std::is_same_v<Type, char>, detail::MBEquivalence,
			std::conditional_t<std::is_same_v<Type, wchar_t>, detail::WideEquivalence, Type>>;

		template <bool Strict>
		str::DecodeOut ReadUtf8(const auto& source) {
			static constexpr str::CodePoint OverlongBound[4] = { 0x00'007f, 0x00'07ff, 0x00'ffff, 0x1f'ffff };
			uint8_t c8 = static_cast<uint8_t>(source[0]);

			/* check if its a single byte and validate the overlong boundary (although only out of principle) */
			if ((c8 & 0x80) == 0x00) {
				if (Strict && c8 > OverlongBound[0])
					return { 1, 0, str::DecResult::strictness };
				return { 1, c8, str::DecResult::valid };
			}

			/* extract the length of the encoding */
			str::CodePoint cp = 0;
			size_t len = 1;
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
				return { 1, 0, str::DecResult::encoding };

			/* validate the buffer has capacity enough */
			if (source.size() < len) {
				return { 0, 0, str::DecResult::incomplete };
			}

			/* validate the contiunation-bytes and construct the final codepoint */
			for (size_t j = 1; j < len; ++j) {
				uint8_t n8 = static_cast<uint8_t>(source[j]);
				if ((n8 & 0xc0) != 0x80)
					return { j, 0, str::DecResult::encoding };
				cp = ((cp << 6) | (n8 & 0x3f));
			}

			/* check if this is a strict decoding and check the overlong bondaries */
			if (Strict && cp > OverlongBound[len - 1])
				return { len, 0, str::DecResult::strictness };
			return { len, cp, str::DecResult::valid };
		}
		str::DecodeOut ReadUtf16(const auto& source) {
			str::CodePoint cp = static_cast<uint16_t>(source[0]);

			/* check if its a valid single-token codepoint */
			if (cp < 0xd800 || cp > 0xdfff)
				return { 1, cp, str::DecResult::valid };

			/* ensure there are enough characters for a surrogate-pair and that the first value is valid */
			if (cp > 0xdbff)
				return { 1, 0, str::DecResult::encoding };
			if (source.size() < 2)
				return { 0, 0, str::DecResult::incomplete };

			/* extract and validate the second token */
			str::CodePoint next = static_cast<uint16_t>(source[1]);
			if (next < 0xdc00 || next > 0xdfff)
				return { 2, 0, str::DecResult::encoding };

			/* decode the overall codepoint */
			return { 2, 0x10000 + ((cp & 0x03ff) << 10) | (next & 0x03ff), str::DecResult::valid };
		}
		str::DecodeOut ReadUtf32(const auto& source) {
			return { 1, static_cast<str::CodePoint>(source[0]), str::DecResult::valid };
		}
		template <bool Strict>
		str::DecodeOut ReadMultiByte(const auto& source) {
			std::mbstate_t state{ 0 };
			wchar_t wc = 0;

			/* read the next character and check if the character is incomplete (documentation
			*	suggests that decoded codepoint should always fit into single wide-char) */
			size_t res = std::mbrtowc(&wc, source.data(), source.size(), &state);
			if (res == static_cast<size_t>(-2)) {
				/* check if the max encoding length promise would be broken by this character (should not be possible for any encoding) */
				if (source.size() >= str::MaxEncodeLength)
					return { 1, 0, str::DecResult::encoding };
				return { 0, 0, str::DecResult::incomplete };
			}
			size_t len = res;

			/* check if there is a decoding-error or its a null-character, in which
			*	case the length has to be figured out by creeping up to the previous response */
			if (res == static_cast<size_t>(-1) || res == 0) {
				len = 1;
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
					return { std::min<size_t>(len, str::MaxEncodeLength), 0, str::DecResult::encoding };
				}
			}

			/* check if the char is longer than the internal max character-length, which should really not happen
			*	in any encoding and would break the promise of str::MaxEncodeLength, and can therefore be dropped */
			if (len > str::MaxEncodeLength)
				return { str::MaxEncodeLength, 0, str::DecResult::encoding };

			/* try to decode the wide-character to the codepoint (cannot use std::mbrtoc8 or std::mbrtoc32 as they
			*	are missing/do not respect the codepage in MSVC) and fail with encoding if it cannot be decoded */
			str::DecodeOut out{};
			if constexpr (std::is_same_v<detail::WideEquivalence, char8_t>)
				out = detail::ReadUtf8<Strict>(std::basic_string_view<wchar_t>{ &wc, 1 });
			else if constexpr (std::is_same_v<detail::WideEquivalence, char16_t>)
				out = detail::ReadUtf16(std::basic_string_view<wchar_t>{ &wc, 1 });
			else
				out = detail::ReadUtf32(std::basic_string_view<wchar_t>{ &wc, 1 });

			/* check if the wide character could be decoded properly (no need to check for valid codepoints,
			*	as this function is only called by ReadCodePoint, which will perform the check) */
			if (out.result != str::DecResult::valid)
				return { len, 0, str::DecResult::encoding };
			return { len, out.cp, str::DecResult::valid };
		}

		void WriteUtf8(auto& sink, str::CodePoint cp) {
			/* check if a single character fits */
			if (cp <= 0x7f) {
				sink.push_back(static_cast<char8_t>(cp));
				return;
			}

			/* write the first 1-3 characters out */
			if (cp <= 0x07ff)
				sink.push_back(static_cast<char8_t>(0xc0 | ((cp >> 6) & 0x1f)));
			else {
				if (cp <= 0xffff)
					sink.push_back(static_cast<char8_t>(0xe0 | ((cp >> 12) & 0x0f)));
				else {
					sink.push_back(static_cast<char8_t>(0xf0 | ((cp >> 18) & 0x07)));
					sink.push_back(static_cast<char8_t>(0x80 | ((cp >> 12) & 0x3f)));
				}

				/* push the second to last 6 bits of the codepoint */
				sink.push_back(static_cast<char8_t>(0x80 | ((cp >> 6) & 0x3f)));
			}

			/* push the last 6 bits of the codepoint */
			sink.push_back(static_cast<char8_t>(0x80 | (cp & 0x3f)));
		}
		void WriteUtf16(auto& sink, str::CodePoint cp) {
			if (cp >= 0x10000) {
				cp -= 0x10000;
				sink.push_back(static_cast<char16_t>(0xd800 + ((cp >> 10) & 0x03ff)));
				cp = 0xdc00 + (cp & 0x03ff);
			}
			sink.push_back(static_cast<char16_t>(cp));
		}
		void WriteUtf32(auto& sink, str::CodePoint cp) {
			sink.push_back(static_cast<char32_t>(cp));
		}
		bool WriteMultiByte(auto& sink, str::CodePoint cp) {
			str::Small<wchar_t, str::MaxEncodeLength> wc;

			/* convert the codepoint first to wide-characters (will succeed at all times)  and check if it only resulted in
			*	one, as a single multi-byte char must originate from exactly 1 wide char (also expected by read-multibyte) */
			if constexpr (std::is_same_v<detail::WideEquivalence, char8_t>)
				detail::WriteUtf8(wc, cp);
			else if constexpr (std::is_same_v<detail::WideEquivalence, char16_t>)
				detail::WriteUtf16(wc, cp);
			else
				detail::WriteUtf32(wc, cp);
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
			for (size_t i = 0; i < res; ++i)
				sink.push_back(buf[i]);
			return true;
		}

		/* expect s to contain at least one character (will only return consumed:0 on incomplete chars) */
		template <class ChType, bool Strict>
		str::DecodeOut ReadCodePoint(const std::basic_string_view<ChType>& source) {
			using EffType = detail::EffectiveChar<ChType>;
			str::DecodeOut res{};

			/* decode the next codepoint */
			if constexpr (std::is_same_v<EffType, char>)
				res = detail::ReadMultiByte<Strict>(source);
			else if constexpr (std::is_same_v<EffType, char8_t>)
				res = detail::ReadUtf8<Strict>(source);
			else if constexpr (std::is_same_v<EffType, char16_t>)
				res = detail::ReadUtf16(source);
			else
				res = detail::ReadUtf32(source);

			/* validate the codepoint itself */
			if (Strict && res.result == str::DecResult::valid && !str::ValidCodePoint(res.cp))
				return { res.consumed, 0, str::DecResult::strictness };
			return res;
		}

		template <class ChType, bool Strict>
		bool WriteCodePoint(auto& sink, str::CodePoint cp) {
			using EffType = detail::EffectiveChar<ChType>;

			/* check if the codepoint is invalid */
			if (Strict && !str::ValidCodePoint(cp))
				return false;

			/* encode the codepoint (only multi-byte can potentially fail, depending on code-page) */
			if constexpr (std::is_same_v<EffType, char>)
				return detail::WriteMultiByte(sink, cp);
			else if constexpr (std::is_same_v<EffType, char8_t>)
				detail::WriteUtf8(sink, cp);
			else if constexpr (std::is_same_v<EffType, char16_t>)
				detail::WriteUtf16(sink, cp);
			else
				detail::WriteUtf32(sink, cp);
			return true;
		}

		template <class ChType>
		void TryWriteErrorChar(auto& s, char c) {
			/* will not be written if its a null-byte */
			if (c == 0)
				return;

			/* this should be rare, therefore it can be decoded and encoded properly (ignore any kind of failures) */
			auto [_, cp, res] = detail::ReadCodePoint<char, true>(std::basic_string_view{ &c, 1 });
			if (res == str::DecResult::valid)
				detail::WriteCodePoint<ChType, true>(s, cp);
		}
	}

	/* check if the normal character string uses a given utf-encoding */
	static constexpr bool IsCharUtf8 = std::is_same_v<detail::MBEquivalence, char8_t>;
	static constexpr bool IsCharUtf16 = std::is_same_v<detail::MBEquivalence, char16_t>;
	static constexpr bool IsCharUtf32 = std::is_same_v<detail::MBEquivalence, char32_t>;

	/* check if the wide character string uses a given utf-encoding */
	static constexpr bool IsWideUtf8 = std::is_same_v<detail::WideEquivalence, char8_t>;
	static constexpr bool IsWideUtf16 = std::is_same_v<detail::WideEquivalence, char16_t>;
	static constexpr bool IsWideUtf32 = std::is_same_v<detail::WideEquivalence, char32_t>;

	/* read a single codepoint from the beginning of the string (if strictness is required,
	*	any overlong utf8-chars or invalid codepoints will result in strictness-errors) */
	template <bool Strict = true>
	str::DecodeOut Decode(const str::AnyString auto& source) {
		using ChType = str::StringChar<decltype(source)>;

		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return { 0, 0, str::DecResult::empty };
		return detail::ReadCodePoint<ChType, Strict>(view);
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

	/*
	*	Transcode-Mode
	*		fast: if source and destination are of the same type, just copy char-by-char without decoding, else equal to [relaxed]
	*		relaxed: only check encoding, but do not check for invalid codepoints or overlong utf8-encodings
	*		strict: ensure every unicode-codepoint is decoded properly and a valid codepoint
	*/
	enum class TrMode : uint8_t {
		fast,
		relaxed,
		strict
	};

	/* read a single codepoint from the source and transcode it to the sink (returns number of characters consumed from the source,
	*	0 on empty source or incomplete character, and write the error char, if any error occurred and error-char is not null) */
	template <str::TrMode Mode = str::TrMode::fast>
	size_t Transcode(str::AnySink auto& sink, const str::AnyString auto& source, char charOnError = str::CharOnError) {
		using SChType = str::StringChar<decltype(source)>;
		using DChType = str::SinkChar<decltype(sink)>;

		/* check if the source is empty, in which case nothing can be transcoded */
		std::basic_string_view<SChType> view{ source };
		if (view.empty())
			return 0;

		/* check if fast transcoding has been selected, and the source and destination are the same type, in which case
		*	just one character will be copied, no matter the validity of the encoding/the length of the actual encoding */
		if constexpr (Mode == str::TrMode::fast && std::is_same_v<SChType, DChType>) {
			sink.push_back(view[0]);
			return 1;
		}

		/* decode the next character and check if its incomplete, in which case nothing will be done */
		str::DecodeOut out = detail::ReadCodePoint<SChType, Mode == str::TrMode::strict>(view);
		if (out.result == str::DecResult::incomplete)
			return 0;

		/* check if an error occurred, in which case the char-error will be written */
		if (out.result != str::DecResult::valid) {
			detail::TryWriteErrorChar<DChType>(sink, charOnError);
			return out.consumed;
		}

		/* check if the source and destination are of the same type, in which case the characters can just be copied */
		if constexpr (std::is_same_v<SChType, DChType>) {
			for (size_t i = 0; i < out.consumed; ++i)
				sink.push_back(view[i]);
			return out.consumed;
		}

		/* try to write the codepoint to the destination and otherwise write the error-token and return the consumed number of token */
		else if (!detail::WriteCodePoint<DChType, Mode == str::TrMode::strict>(sink, out.cp))
			detail::TryWriteErrorChar<DChType>(sink, charOnError);
		return out.consumed;
	}

	/* return a string containing the transcoded next codepoint from the source (returns number of characters consumed from the source,
	*	0 on empty source or incomplete character, and write the error char, if any error occurred and error-char is not null) */
	template <str::IsChar ChType, str::TrMode Mode = str::TrMode::fast>
	std::pair<str::Small<ChType, str::MaxEncodeLength>, size_t> Transcode(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		str::Small<ChType, str::MaxEncodeLength> out{};
		size_t consumed = str::Transcode<Mode>(out, source, charOnError);
		return { out, consumed };
	}

	/* append the source-string of any type to the sink-string (append error char, if last is incomplete and error-char is not null) */
	template <str::TrMode Mode = str::TrMode::fast>
	auto& Append(str::AnySink auto& sink, const str::AnyString auto& source, char charOnError = str::CharOnError) {
		using SChType = str::StringChar<decltype(source)>;
		using DChType = str::SinkChar<decltype(sink)>;
		std::basic_string_view<SChType> view{ source };

		/* check if the source and destination are the same and can just be appended, due to fast transcoding-mode */
		if constexpr (Mode == str::TrMode::fast && std::is_same_v<SChType, DChType>) {
			sink.append(view);
			return sink;
		}

		/* transcode all characters until a zero is returned (which implies last incomplete) */
		while (!view.empty()) {
			size_t consumed = str::Transcode<Mode>(sink, view, charOnError);
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
	template <str::TrMode Mode = str::TrMode::fast>
	auto& Append(str::AnySink auto& sink, str::IsChar auto val, char charOnError = str::CharOnError) {
		return str::Append<Mode>(sink, std::basic_string_view<decltype(val)>{ &val, 1 }, charOnError);
	}

	/* return a string containing the byte-order-mark encoded in the corresponding type (empty if not utf8/utf16/utf32) */
	template <str::IsChar ChType>
	str::Small<ChType, str::MaxEncodeLength> BOM() {
		str::Small<ChType, str::MaxEncodeLength> out{};
		if constexpr (str::IsUnicode<ChType>)
			detail::WriteCodePoint<ChType, true>(out, 0xfeff);
		return out;
	}

	/* convert any object of s-type to a string using d-type characters */
	template <str::IsChar ChType, str::TrMode Mode = str::TrMode::fast>
	std::basic_string<ChType> Conv(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		std::basic_string<ChType> out{};
		return str::Append<Mode>(out, source, charOnError);
	}

	/* convenience for fast conversion */
	template <str::TrMode Mode = str::TrMode::fast>
	std::string ToChar(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conv<char, Mode>(source, charOnError);
	}
	template <str::TrMode Mode = str::TrMode::fast>
	std::wstring ToWide(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conv<wchar_t, Mode>(source, charOnError);
	}
	template <str::TrMode Mode = str::TrMode::fast>
	std::u8string ToUtf8(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conv<char8_t, Mode>(source, charOnError);
	}
	template <str::TrMode Mode = str::TrMode::fast>
	std::u16string ToUtf16(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conv<char16_t, Mode>(source, charOnError);
	}
	template <str::TrMode Mode = str::TrMode::fast>
	std::u32string ToUtf32(const str::AnyString auto& source, char charOnError = str::CharOnError) {
		return str::Conv<char32_t, Mode>(source, charOnError);
	}
}
