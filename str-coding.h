/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "coding/str-utf8.h"
#include "coding/str-utf16.h"
#include "coding/str-utf32.h"
#include "coding/str-wide.h"
#include "coding/str-multibyte.h"
#include "str-common.h"
#include "str-chars.h"

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
	struct CodingException : public str::RuntimeException {
		CodingException(const std::wstring& s) : str::RuntimeException{ s } {}
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
		inline constexpr bool EncodeSingle(auto&& sink, char32_t cp) {
			if (detail::EncodeCodepoint<ChType>(sink, cp))
				return true;

			/* check if the codepoint could not be encoded and handle the error accordingly */
			if constexpr (CodeError == err::Skip || CodeError == err::Nothing)
				return false;
			else if constexpr (CodeError == err::Throw)
				throw str::CodingException(L"Codepoint could not be encoded");
			if (!detail::EncodeCodepoint<ChType>(sink, CodeError))
				throw str::CodingException(L"Alternative coding-error codepoint could not be encoded");
			return true;
		}
		template <class ChType, char32_t CodeError, bool AllowIncomplete>
		inline constexpr str::Decoded DecodeSingle(const std::basic_string_view<ChType>& view) {
			if (view.empty())
				return {};
			str::Decoded dec = detail::DecodeNext<ChType, AllowIncomplete>(view.data(), view.data() + view.size());

			/* check if an error occurred and the codepoint should either be replaced or an exception raised */
			if constexpr (CodeError != err::Nothing && CodeError != err::Skip) {
				if (dec.cp == str::Invalid && (!AllowIncomplete || dec.consumed > 0)) {
					if constexpr (CodeError == err::Throw)
						throw str::CodingException(L"Codepoint could not be decoded");
					dec.cp = CodeError;
				}
			}
			return dec;
		}
		template <class DChType, class SChType, char32_t CodeError, bool AllowIncomplete, bool FastMode>
		inline constexpr str::Transcoded<DChType> TranscodeSingle(const std::basic_string_view<SChType>& view) {
			str::Transcoded<DChType> out{};

			/* check if the length can just be copied */
			if constexpr (str::EffSame<SChType, DChType> && FastMode) {
				if (view.empty())
					return out;
				uint32_t len = detail::EstimateLength<SChType>(view.data(), view.data() + view.size());

				/* check if a valid codepoint has been encountered and copy it out */
				if (len > 0) {
					if (len <= view.size()) {
						out.cp.append(reinterpret_cast<const DChType*>(view.data()), len);
						out.consumed = len;
						return out;
					}
					else if constexpr (AllowIncomplete)
						return out;
				}

				/* default-consume one character and handle the invalid/incomplete codepoint */
				out.consumed = 1;
				if constexpr (CodeError != err::Nothing && CodeError != err::Skip) {
					if constexpr (CodeError == err::Throw)
						throw str::CodingException(L"Codepoint could not be transcoded");
					if (!detail::EncodeCodepoint<DChType>(out.cp, CodeError))
						throw str::CodingException(L"Alternative coding-error codepoint could not be transcoded");
				}
				return out;
			}

			/* decode the next codepoint as efficient as possible (no need to validate on char32_t, as the destination must
			*	then not be char32_t and hence valid codepoints will be ensured, CodeError will automatically be applied) */
			char32_t cp = 0;
			if constexpr (std::is_same_v<str::EffChar<SChType>, char32_t>) {
				if (view.empty())
					return out;
				out.consumed = 1;
				cp = char32_t(view[0]);
			}
			else {
				auto [_cp, len] = detail::DecodeSingle<SChType, CodeError, AllowIncomplete>(view);
				out.consumed = len;
				if (_cp == str::Invalid)
					return out;
				cp = _cp;
			}

			/* encode the codepoint as efficient as possible (no need to validate the codepoint, as it must have already
			*	been verified by reading as it cannot have been a source char32_t, CodeError will automatically be applied) */
			if constexpr (std::is_same_v<str::EffChar<DChType>, char32_t>)
				out.cp.push_back(cp);
			else
				detail::EncodeSingle<DChType, CodeError>(out.cp, cp);
			return out;
		}
		template <char32_t CodeError>
		inline constexpr void EncodeTo(auto&& sink, char32_t cp, size_t count) {
			using ChType = str::SinkChar<decltype(sink)>;

			/* check if the codepoint can directly be written to the sink */
			if (count == 1)
				detail::EncodeSingle<ChType, CodeError>(sink, cp);
			else if (count > 0) {
				/* encode the string to a temporary buffer for faster duplication */
				str::Encoded<ChType> c;
				if (!detail::EncodeSingle<ChType, CodeError>(c, cp))
					return;

				/* write the codepoint out */
				if (c.size() == 1)
					str::CallSink(sink, c[0], count);
				else for (size_t i = 0; i < count; ++i)
					str::CallSink(sink, c);
			}
		}
	}

	/* [str::IsIterator] Initialized to the next upcoming codepoint; Once invalidated, does not become valid anymore
	*	Will only produce valid unicode codepoints or values based on CodeError
	*	Note: depending on CodeError, starting within a codepoint may result in inconsistent forward and backward codepoint iteration */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	struct Iterator {
	private:
		const ChType* pBegin = 0;
		const ChType* pCurrent = 0;
		const ChType* pEnd = 0;
		str::Decoded pOut{};

	public:
		constexpr Iterator(const std::basic_string_view<ChType>& s = {}, size_t off = 0, bool previous = false) : pBegin{ s.data() }, pEnd{ s.data() + s.size() } {
			pCurrent = std::min(pBegin + off, pEnd);

			/* initialize the first codepoint */
			if (previous)
				fPrev();
			else
				fNext();
		}

	private:
		bool fPrev() {
			while (true) {
				/* check if the end has been reached */
				if (pCurrent <= pBegin) {
					pCurrent = 0;
					return false;
				}

				/* decode the previous codepoint and update the current iterator */
				pOut = detail::DecodePrev<ChType>(pBegin, pCurrent);
				pCurrent -= pOut.consumed;

				/* check if the codepoint is invalid and needs to be corrected */
				if constexpr (CodeError != err::Nothing) {
					if (pOut.cp == str::Invalid) {
						if constexpr (CodeError == err::Skip)
							continue;
						if constexpr (CodeError == err::Throw)
							throw str::CodingException(L"Invalid codepoint encountered in str::Iterator");
						pOut.cp = CodeError;
					}
				}
				return true;
			}
		}
		bool fNext() {
			while (true) {
				/* advance the current position and check if the end has been reached */
				pCurrent += pOut.consumed;
				if (pCurrent >= pEnd) {
					pCurrent = 0;
					return false;
				}

				/* decode the next codepoint */
				pOut = detail::DecodeNext<ChType, false>(pCurrent, pEnd);

				/* check if the codepoint is invalid and needs to be corrected */
				if constexpr (CodeError != err::Nothing) {
					if (pOut.cp == str::Invalid) {
						if constexpr (CodeError == err::Skip)
							continue;
						else if constexpr (CodeError == err::Throw)
							throw str::CodingException(L"Invalid codepoint encountered in str::Iterator");
						pOut.cp = CodeError;
					}
				}
				return true;
			}
		}

	public:
		constexpr bool valid() const {
			return (pCurrent != 0);
		}
		constexpr char32_t get() const {
			return pOut.cp;
		}
		constexpr char32_t next() {
			char32_t cp = pOut.cp;
			fNext();
			return cp;
		}
		constexpr char32_t prev() {
			char32_t cp = pOut.cp;
			fPrev();
			return cp;
		}
		constexpr bool advance() {
			return fNext();
		}
		constexpr bool reverse() {
			return fPrev();
		}

	public:
		/* return the index into the current string (not valid anymore after iterator has been invalidated) */
		constexpr size_t base() const {
			return pCurrent - pBegin;
		}

		/* return the base string (also valid if iterator itself has been invalidated) */
		constexpr std::basic_string_view<ChType> string() const {
			return std::basic_string_view<ChType>{ pBegin, pEnd };
		}
	};

	/* decode single next codepoint from corresponding type, if its an ascii character, and return str::Invalid if the source is empty,
	*	otherwise at least consume one character at all times and return str::Invalid on decoding-errors or if the codepoint is not ascii */
	template <char32_t CodeError = err::DefChar>
	constexpr str::Decoded GetAscii(const str::IsStr auto& source) {
		using ChType = str::StringChar<decltype(source)>;
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
			throw str::CodingException(L"Codepoint is not a valid ascii codepoint");
		if constexpr (CodeError >= detail::AsciiRange)
			throw str::CodingException(L"Alternative coding-error codepoint is not a valid ascii codepoint");
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

	/* decode a single codepoint from the source and return it (return consumed null if the
	*	source is empty and otherwise consume at all times at least one character and return
	*	str::Invalid on errors, if CodeError does not define alternative behavior) */
	template <char32_t CodeError = err::DefChar>
	constexpr str::Decoded GetCodepoint(const str::IsStr auto& source) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		return detail::DecodeSingle<ChType, CodeError, false>(view);
	}

	/* determine if the source starts with a valid codepoint (i.e. is aligned with the start of a valid codepoint) */
	constexpr bool IsCodepoint(const str::IsStr auto& source) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		return (detail::DecodeSingle<ChType, err::Nothing, false>(view) != str::Invalid);
	}

	/* decode a single codepoint from the source and return it (return consumed null if the source is empty or
	*	the next codepoint is incomplete and otherwise consume at all times at least one character and return
	*	str::Invalid on errors, if CodeError does not define alternative behavior; will not return incomplete
	*	if MaxSize number of characters are provided) */
	template <char32_t CodeError = err::DefChar>
	constexpr str::Decoded PartialCodepoint(const str::IsStr auto& source) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		return detail::DecodeSingle<ChType, CodeError, true>(view);
	}

	/* transcode the next codepoint and return it (does guarantee valid encoding, return consumed null if the source is empty and otherwise
	*	consume at all times at least one character and return an empty string on errors, if CodeError does not define alternative behavior) */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	constexpr str::Transcoded<ChType> GetTranscode(const str::IsStr auto& source) {
		using SChType = str::StringChar<decltype(source)>;
		std::basic_string_view<SChType> view{ source };
		return detail::TranscodeSingle<ChType, SChType, CodeError, false, false>(view);
	}

	/* transcode the next codepoint and return it (does guarantee valid encoding, return consumed null if the
	*	source is empty or the next codepoint is incomplete and otherwise consume at all times at least one
	*	character and return an empty string on errors, if CodeError does not define alternative behavior) */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	constexpr str::Transcoded<ChType> PartialTranscode(const str::IsStr auto& source) {
		using SChType = str::StringChar<decltype(source)>;
		std::basic_string_view<SChType> view{ source };
		return detail::TranscodeSingle<ChType, SChType, CodeError, true, false>(view);
	}

	/* transcode the next codepoint as efficient as possible and return it (does not guarantee valid encoding, if both source
	*	and destination are of the same type, return consumed null if the source is empty and otherwise consume at all times
	*	at least one character and return an empty string on errors, if CodeError does not define alternative behavior) */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	constexpr str::Transcoded<ChType> GetFastcode(const str::IsStr auto& source) {
		using SChType = str::StringChar<decltype(source)>;
		std::basic_string_view<SChType> view{ source };
		return detail::TranscodeSingle<ChType, SChType, CodeError, false, true>(view);
	}

	/* transcode the next codepoint as efficient as possible and return it (does not guarantee valid
	*	encoding, if both source and destination are of the same type, return consumed null if the source
	*	is empty or the next codepoint is incomplete and otherwise consume at all times at least one
	*	character and return an empty string on errors, if CodeError does not define alternative behavior) */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	constexpr str::Transcoded<ChType> PartialFastcode(const str::IsStr auto& source) {
		using SChType = str::StringChar<decltype(source)>;
		std::basic_string_view<SChType> view{ source };
		return detail::TranscodeSingle<ChType, SChType, CodeError, true, true>(view);
	}

	namespace detail {
		template <char32_t CodeError, bool FastMode, class SinkType, class SourceType>
		constexpr SinkType& ConvertCodesTo(SinkType&& sink, SourceType& source) {
			using SChType = str::StringChar<decltype(source)>;
			using DChType = str::SinkChar<decltype(sink)>;
			std::basic_string_view<SChType> view{ source };

			/* check if the string can just be appended */
			if constexpr (str::EffSame<SChType, DChType> && FastMode) {
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
				while (it.valid())
					str::CallSink(sink, DChType(it.next()), 1);
				return sink;
			}

			/* iterate over the source-codepoints and transcode them */
			str::Iterator<SChType, CodeError> it{ view };
			while (it.valid())
				str::CodepointTo<CodeError>(sink, it.next(), 1);
			return sink;
		}
	}

	/* transcode the entire source-string to the sink and return it (does guarantee valid encoding at all times) */
	template <char32_t CodeError = err::DefChar>
	constexpr auto& TranscodeAllTo(str::IsSink auto&& sink, const str::IsStr auto& source) {
		return detail::ConvertCodesTo<CodeError, false>(sink, source);
	}

	/* transcode the entire source-string to an object of the given sink-type using
	*	str::TranscodeAllTo and return it (does guarantee valid encoding at all times) */
	template <str::IsSink SinkType, char32_t CodeError = err::DefChar>
	constexpr SinkType TranscodeAll(const str::IsStr auto& source) {
		SinkType out{};
		str::TranscodeAllTo<CodeError>(out, source);
		return out;
	}

	/* transcode the entire source-string as efficient as possible to the sink and return it (does
	*	not guarantee valid encoding, if both source and destination are of the same type) */
	template <char32_t CodeError = err::DefChar>
	constexpr auto& FastcodeAllTo(str::IsSink auto&& sink, const str::IsStr auto& source) {
		return detail::ConvertCodesTo<CodeError, true>(sink, source);
	}

	/* transcode the entire source-string as efficient as possible to an object of the given sink-type using str::FastcodeAllTo
	*	and return it (does not guarantee valid encoding, if both source and destination are of the same type) */
	template <str::IsSink SinkType, char32_t CodeError = err::DefChar>
	constexpr SinkType FastcodeAll(const str::IsStr auto& source) {
		SinkType out{};
		str::FastcodeAllTo<CodeError>(out, source);
		return out;
	}
}
