/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "coding/str-utf8.h"
#include "coding/str-utf16.h"
#include "coding/str-utf32.h"
#include "coding/str-wide.h"
#include "coding/str-multibyte.h"
#include "str-common.h"

/*
*	All codepoints are represented as char32_t
*
*	Expected:
*		char8_t/char16_t/char32_t encodes the corresponding utf-standard
*		wchar_t encodes utf-16 or utf-32
*		char is either utf-8 or arbitrary multi-byte solution
*
*	Decoding returns std::Invalid if:
*		=> source is empty => len = 0
*		=> partial call and codepoint incomplete => len = 0
*		=> invalid encoding => len = 1
*
*	All decoding functions will only produce str::Invalid or valid Codepoints as per cp::IsUnicode
*		=> Unless CodeError resolves to invalid codepoint itself
*
*	All codepoints will be written as a single char/string-call to the sink
*		=> Prevent partially encoded codepoints from being added
*/
namespace str {
	namespace detail {
		template <class ChType>
		struct EncSize;
		template <> struct EncSize<char8_t> { static constexpr size_t value = detail::Utf8Len; };
		template <> struct EncSize<char16_t> { static constexpr size_t value = detail::Utf16Len; };
		template <> struct EncSize<char32_t> { static constexpr size_t value = detail::Utf32Len; };
		template <> struct EncSize<wchar_t> { static constexpr size_t value = detail::WideLen; };
		template <> struct EncSize<char> { static constexpr size_t value = detail::CharLen; };
	}

	/* maximum encoding-size for any codepoint for the corresponding type */
	template <str::IsChar ChType>
	constexpr size_t MaxEncSize = detail::EncSize<ChType>::value;

	/* maximum byte-size to encode any codepoint of the corresponding type */
	template <str::IsChar ChType>
	constexpr size_t MaxEncBytes = detail::EncSize<ChType>::value * sizeof(ChType);

	/* local-string to hold the single encoded codepoint for the corresponding type */
	template <str::IsChar ChType>
	using Encoded = str::Local<ChType, detail::EncSize<ChType>::value>;

	/* local-string and number of consumed characters to transcode the single codepoint to the corresponding type */
	template <str::IsChar ChType>
	struct Transcoded {
		str::Encoded<ChType> cp;
		uint32_t consumed = 0;
	};

	/* return the effective character type equivalent to the encoding (i.e. if wchar_t uses
	*	utf-16, will result in char16_t; will only result in char, char8_t, char16_t, char32_t) */
	template <str::IsChar Type>
	using EffChar = std::conditional_t<std::is_same_v<Type, char>, std::conditional_t<str::CharIsUtf8, char8_t, char>,
		std::conditional_t<std::is_same_v<Type, wchar_t>, std::conditional_t<str::WideIsUtf16, char16_t, char32_t>, Type>>;

	/* check if the two character-types are effectively using the same encoding */
	template <class ChTypeA, class ChTypeB>
	concept EffSame = std::is_same_v<str::EffChar<ChTypeA>, str::EffChar<ChTypeB>>;

	/*
	*	define behavior when encountering invalid codepoints while decoding/encoding
	*		=> If no predefined value, CodeError is interpreted as codepoint to be used instead as value
	*/
	namespace err {
		/* default error character to be used instead of str::Invalid (guaranteed to be an ascii-character)
		*	=> decoding: return CodeError instead of str::Invalid [no error-verification of CodeError]
		*	=> encoding: try to encode CodeError or throw str::CodingException if it fails as well
		*	=> ascii: ensure the CodeError is a valid ascii character or throw strd::CodingException
		*	=> iterator: return CodeError instead of str::Invalid [no error-verification of CodeError] */
		static constexpr char32_t DefChar = U'?';

		/* throw str::CodingException if str::Invalid is encountered */
		static constexpr char32_t Throw = char32_t(-1);

		/* return str::Invalid without performing any modifications
		*	=> encoding: write nothing to the sink
		*	=> decoding: return str::Invalid
		*	=> ascii: return str::Invalid
		*	=> iterator: return str::Invalid */
		static constexpr char32_t Nothing = char32_t(-2);

		/* skip the codepoint and continue to the next valid codepoint
		*	=> encoding: write nothing to the sink
		*	=> decoding: return str::Invalid
		*	=> ascii: return str::Invalid
		*	=> iterator: advance to next valid codepoint in given direction */
		static constexpr char32_t Skip = char32_t(-3);
	}

	/* invalid codepoint decoding/encoding exception */
	struct CodingException : public std::runtime_error {
		CodingException(const std::string& s) : runtime_error(s) {}
	};

	namespace detail {
		template <class ChType, bool AllowIncomplete>
		inline constexpr str::Decoded DecodeNext(const ChType* cur, const ChType* end) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::NextUtf8<char8_t, AllowIncomplete>(cur, end);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::NextUtf16<char16_t, AllowIncomplete>(cur, end);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::NextUtf32<char32_t>(cur, end);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::NextWide<AllowIncomplete>(cur, end);
			else
				return detail::NextChar<AllowIncomplete>(cur, end);
		}
		template <class ChType>
		inline constexpr str::Decoded DecodePrev(const ChType* begin, const ChType* cur) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::PrevUtf8<char8_t>(begin, cur);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::PrevUtf16<char16_t>(begin, cur);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::PrevUtf32<char32_t>(begin, cur);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::PrevWide(begin, cur);
			else
				return detail::PrevChar(begin, cur);
		}
		template <class ChType>
		inline constexpr bool EncodeCodepoint(auto&& sink, char32_t cp) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::MakeUtf8<char8_t>(sink, cp);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::MakeUtf16<char16_t>(sink, cp);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::MakeUtf32<char32_t>(sink, cp);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::MakeWide(sink, cp);
			else
				return detail::MakeChar(sink, cp);
		}
		template <class ChType>
		inline constexpr uint32_t EstimateLength(const ChType* cur, const ChType* end) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::EstimateUtf8<char8_t>(cur, end);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::EstimateUtf16<char16_t>(cur, end);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::EstimateUtf32<char32_t>(cur, end);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::EstimateWide(cur, end);
			else
				return detail::EstimateChar(cur, end);
		}

		template <class ChType, char32_t CodeError>
		constexpr inline bool EncodeToSingle(auto&& sink, char32_t cp) {
			if (detail::EncodeCodepoint<ChType>(sink, cp))
				return true;

			/* check if the codepoint could not be encoded and handle the error accordingly */
			if constexpr (CodeError == err::Skip || CodeError == err::Nothing)
				return false;
			else if constexpr (CodeError == err::Throw)
				throw str::CodingException("Invalid codepoint encountered in str::Codepoint/str::Transcode");
			if (!detail::EncodeCodepoint<ChType>(sink, CodeError))
				throw str::CodingException("Alternative coding-error codepoint could not be encoded in str::Codepoint/str::Transcode");
			return true;
		}
		template <char32_t CodeError>
		constexpr inline void EncodeTo(auto&& sink, char32_t cp, size_t count) {
			using ChType = str::SinkChar<decltype(sink)>;

			/* check if the codepoint can directly be written to the sink */
			if (count == 1)
				detail::EncodeToSingle<ChType, CodeError>(sink, cp);
			else if (count > 0) {
				/* encode the string to a temporary buffer for faster duplication */
				str::Encoded<ChType> c;
				if (!detail::EncodeToSingle<ChType, CodeError>(c, cp))
					return;

				/* write the codepoint out */
				if (c.size() == 1)
					str::CallSink(sink, c[0], count);
				else for (size_t i = 0; i < count; ++i)
					str::CallSink(sink, c);
			}
		}
	}

	/* [str::IsIterator] Initialized to str::Invalid; call prev()/next() to initialize the state
	*	Will only produce valid unicode codepoints or str::Invalid */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	struct Iterator {
	private:
		const ChType* pBegin = 0;
		const ChType* pCurrent = 0;
		const ChType* pEnd = 0;
		str::Decoded pOut{};

	public:
		constexpr Iterator(const std::basic_string_view<ChType>& s = {}, size_t off = 0) {
			pBegin = s.data();
			pEnd = pBegin + s.size();
			pCurrent = std::min(pBegin + off, pEnd);
			pOut = { str::Invalid, 0 };
		}

	public:
		constexpr bool prev() {
			if (pCurrent <= pBegin)
				return false;

			/* skip all codepoints until the first valid codepoint is encountered */
			if constexpr (CodeError == err::Skip) {
				const ChType* cur = pCurrent;
				size_t acc = 0;
				str::Decoded out{};

				while (true) {
					out = detail::DecodePrev<ChType>(pBegin, cur);
					if (out.cp != str::Invalid)
						break;

					/* accumulate the current invalid codepoint */
					if (cur <= pBegin)
						return false;
					acc += out.consumed;
					cur -= out.consumed;
				}
				pOut = { out.cp, out.consumed + acc };
			}

			else {
				/* decode the previous codepoint and check if it needs to be corrected */
				pOut = detail::DecodePrev<ChType>(pBegin, pCurrent);
				if constexpr (CodeError != err::Nothing) {
					if (pOut.cp == str::Invalid) {
						if constexpr (CodeError == err::Throw)
							throw str::CodingException("Invalid codepoint encountered in str::Iterator");
						pOut.cp = CodeError;
					}
				}
			}
			pCurrent -= pOut.consumed;
			return true;
		}
		constexpr bool next() {
			const ChType* cur = pCurrent + pOut.consumed;
			if (cur >= pEnd)
				return false;

			/* skip all codepoints until the first valid codepoint is encountered */
			if constexpr (CodeError == err::Skip) {
				str::Decoded out;
				while (true) {
					out = detail::DecodeNext<ChType, false>(cur, pEnd);
					if (out.cp != str::Invalid)
						break;
					cur += out.consumed;
					if (cur >= pEnd)
						return false;
				}
				pCurrent = cur;
				pOut = out;
			}

			else {
				/* decode the next codepoint and check if it needs to be corrected */
				pCurrent = cur;
				pOut = detail::DecodeNext<ChType, false>(pCurrent, pEnd);
				if constexpr (CodeError != err::Nothing) {
					if (pOut.cp == str::Invalid) {
						if constexpr (CodeError == err::Throw)
							throw str::CodingException("Invalid codepoint encountered in str::Iterator");
						pOut.cp = CodeError;
					}
				}
			}
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

	/* decode single next codepoint from corresponding type, if its an ascii character, and return str::Invalid if the source is empty,
	*	otherwise at least consume one character at all times and return str::Invalid on decoding-errors or if the codepoint is not ascii */
	template <char32_t CodeError = err::DefChar>
	constexpr str::Decoded GetAscii(const str::IsStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};

		/* check if its a char and char does not hold ascii, in which case it has to be fully
		*	decoded (all other types are guaranteed to use unicode and therefore encode ascii) */
		uint32_t len = 0;
		if constexpr (std::is_same_v<ChType, char> && !str::CharHoldsAscii) {
			str::Decoded dec = detail::NextChar(view.data(), view.data() + view.size());
			if (dec.cp < detail::AsciiRange)
				return dec;
			len = dec.consumed;
		}

		/* check if the next value is not an ascii-character, in which case it has to be decoded normally to ensure to get the normal length */
		else if (std::make_unsigned_t<ChType>(view[0]) >= detail::AsciiRange)
			len = detail::DecodeNext<ChType, false>(view.data(), view.data() + view.size()).consumed;

		/* cast the single next value as codepoint */
		else
			return { char32_t(std::make_unsigned_t<ChType>(view[0])), 1 };

		/* handle the invalid codepoint by either raising the exception or replacing the codepoint */
		if constexpr (CodeError == err::Nothing || CodeError == err::Skip)
			return { str::Invalid, len };
		if constexpr (CodeError == err::Throw)
			throw str::CodingException("Invalid codepoint encountered in str::GetAscii");
		if constexpr (CodeError >= detail::AsciiRange)
			throw str::CodingException("Alternative coding-error codepoint is not a valid ascii character in str::GetAscii");
		return { CodeError, len };
	}

	/* encode single codepoint to corresponding type and write it to the sink and return it */
	template <char32_t CodeError = err::DefChar>
	constexpr auto& CodepointTo(str::IsSink auto&& sink, char32_t cp, size_t count = 1) {
		detail::EncodeTo<CodeError>(sink, cp, count);
		return sink;
	}

	/* encode single codepoint to corresponding type and write it to an object of the given sink-type using str::CodepointTo */
	template <str::IsSink SinkType, char32_t CodeError = err::DefChar>
	constexpr SinkType Codepoint(char32_t cp, size_t count = 1) {
		SinkType out{};
		detail::EncodeTo<CodeError>(out, cp, count);
		return out;
	}

	/* decode a single codepoint from the source and apply the CodeError handling if necssary (return
	*	str::Invalid if the source is empty and otherwise at least consume one character at all times) */
	template <char32_t CodeError = err::DefChar>
	constexpr str::Decoded GetCodepoint(const str::IsStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};
		str::Decoded dec = detail::DecodeNext<ChType, false>(view.data(), view.data() + view.size());

		/* check if an error occurred and the codepoint should either be replaced or an exception raised */
		if constexpr (CodeError != err::Nothing && CodeError != err::Skip) {
			if (dec.cp == str::Invalid) {
				if constexpr (CodeError == err::Throw)
					throw str::CodingException("Invalid codepoint encountered in str::GetCodepoint");
				dec.cp = CodeError;
			}
		}
		return dec;
	}

	/* decode a single codepoint from the source and apply the CodeError handling if necssary (return str::Invalid if the
	*	source is empty or the next codepoint is incomplete and otherwise at least consume one character at all times) */
	template <char32_t CodeError = err::DefChar>
	constexpr str::Decoded PartialCodepoint(const str::IsStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};
		str::Decoded dec = detail::DecodeNext<ChType, true>(view.data(), view.data() + view.size());

		/* check if an error occurred and the codepoint should either be replaced or an exception raised */
		if constexpr (CodeError != err::Nothing && CodeError != err::Skip) {
			if (dec.cp == str::Invalid && dec.consumed > 0) {
				if constexpr (CodeError == err::Throw)
					throw str::CodingException("Invalid codepoint encountered in str::PartialCodepoint");
				dec.cp = CodeError;
			}
		}
		return dec;
	}

	/* transcode the next codepoint as efficient as possible and return it (does not guarantee valid encoding, if both source and
	*	destination are of the same type, returns either empty consumed or empty string depending if error on decoding or encoding) */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	constexpr str::Transcoded<ChType> Transcode(const str::IsStr auto& source) {
		using SChType = str::StrChar<decltype(source)>;
		std::basic_string_view<SChType> view{ source };
		str::Transcoded<ChType> out{};

		/* check if the length can just be copied */
		if constexpr (str::EffSame<SChType, ChType>) {
			if (view.empty())
				return out;
			uint32_t len = detail::EstimateLength<SChType>(view.data(), view.data() + view.size());
			if (len == 0)
				len = 1;
			if (len <= view.size()) {
				out.cp.append(reinterpret_cast<const ChType*>(view.data()), len);
				out.consumed = len;
			}
			return out;
		}

		/* decode the next codepoint as efficient as possible */
		char32_t cp = 0;
		if constexpr (std::is_same_v<str::EffChar<SChType>, char32_t>) {
			if (view.empty())
				return out;
			out.consumed = 1;
			cp = char32_t(view[0]);
		}
		else {
			auto [_cp, len] = str::GetCodepoint<CodeError>(view);
			out.consumed = len;
			if (_cp == str::Invalid)
				return out;
			cp = _cp;
		}

		/* encode the codepoint as efficient as possible */
		if constexpr (std::is_same_v<str::EffChar<ChType>, char32_t>)
			out.cp.push_back(cp);
		else
			str::CodepointTo<CodeError>(out.cp, cp, 1);
		return out;
	}

	/* transcode the entire source-string as efficient as possible to the sink and return it (does
	*	not guarantee valid encoding, if both source and destination are of the same type) */
	template <char32_t CodeError = err::DefChar>
	constexpr auto& TranscodeAllTo(str::IsSink auto&& sink, const str::IsStr auto& source) {
		using SChType = str::StrChar<decltype(source)>;
		using DChType = str::SinkChar<decltype(sink)>;
		std::basic_string_view<SChType> view{ source };

		/* check if the string can just be appended */
		if constexpr (str::EffSame<SChType, DChType>) {
			str::CallSink(sink, std::basic_string_view<DChType>{ reinterpret_cast<const DChType*>(view.data()), view.size() });
			return sink;
		}

		/* check if the source does not need to be decoded */
		if constexpr (std::is_same_v<str::EffChar<SChType>, char32_t>) {
			for (auto c : view)
				str::CodepointTo<CodeError>(sink, char32_t(c), 1);
			return sink;
		}

		/* check if the destination does not need to be encoded */
		if constexpr (std::is_same_v<str::EffChar<DChType>, char32_t>) {
			str::Iterator<SChType, CodeError> it{ view };
			while (it.next())
				str::CallSink(sink, DChType(it.get()), 1);
			return sink;
		}

		/* iterate over the source-codepoints and transcode them */
		str::Iterator<SChType, CodeError> it{ view };
		while (it.next())
			str::CodepointTo<CodeError>(sink, it.get(), 1);
		return sink;
	}

	/* transcode the entire source-string as efficient as possible to an object of the given sink-type using str::TranscodeAllTo
	*	and return it (does not guarantee valid encoding, if both source and destination are of the same type) */
	template <str::IsSink SinkType, char32_t CodeError = err::DefChar>
	constexpr SinkType TranscodeAll(const str::IsStr auto& source) {
		SinkType out{};
		str::TranscodeAllTo<CodeError>(out, source);
		return out;
	}
}
