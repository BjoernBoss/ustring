#pragma once

#include "encode/str-utf8.h"
#include "encode/str-utf16.h"
#include "encode/str-utf32.h"
#include "encode/str-wide.h"
#include "encode/str-multibyte.h"
#include "str-common-v2.h"

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
*	CodeError:
*		str::SkipInvalid
*			=> decoding: return str::Invalid
*			=> encoding: write nothing to the sink
*		str::ThrowInvalid
*			=> Throw str::CodingException if decoding/encoding fails
*		else
*			=> decoding: if str::Invalid would be returned, return CodeError instead (no error-verification of CodeError)
*			=> encoding: if not encodable, try to encode CodeError instead (throw if it also fails)
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

	/* return the effective character type equivalent to the encoding (i.e. if wchar_t uses
	*	utf-16, will result in char16_t; will only result in char, char8_t, char16_t, char32_t) */
	template <str::IsChar Type>
	using EffChar = std::conditional_t<std::is_same_v<Type, char>, std::conditional_t<str::CharIsUtf8, char8_t, char>,
		std::conditional_t<std::is_same_v<Type, wchar_t>, std::conditional_t<str::WideIsUtf16, char16_t, char32_t>, Type>>;

	/* check if the two character-types are effectively using the same encoding */
	template <class ChTypeA, class ChTypeB>
	concept EffSame = std::is_same_v<str::EffChar<ChTypeA>, str::EffChar<ChTypeB>>;

	/* default error character (guaranteed to be an ascii-character) */
	static constexpr char32_t DefErrorChar = U'?';

	/* ignore any invalid codepoints */
	static constexpr char32_t SkipInvalid = char32_t(-1);

	/* throw str::CodingException if an invalid codepoint is encountered */
	static constexpr char32_t ThrowInvalid = char32_t(-2);

	/* invalid codepoint decoding/encoding exception */
	struct CodingException : public std::runtime_error {
		CodingException(const std::string& s) : runtime_error(s) {}
	};

	namespace detail {
		template <class ChType, bool AllowIncomplete>
		inline constexpr str::Decoded DecodeNext(const ChType* cur, const ChType* end) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::NextUtf8<AllowIncomplete>(cur, end);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::NextUtf16<AllowIncomplete>(cur, end);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::NextUtf32(cur, end);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::NextWide<AllowIncomplete>(cur, end);
			else
				return detail::NextChar<AllowIncomplete>(cur, end);
		}
		template <class ChType>
		inline constexpr str::Decoded DecodePrev(const ChType* begin, const ChType* cur) {
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
		template <class ChType>
		inline constexpr bool EncodeCodepoint(auto&& sink, char32_t cp) {
			if constexpr (std::is_same_v<ChType, char8_t>)
				return detail::MakeUtf8(sink, cp);
			else if constexpr (std::is_same_v<ChType, char16_t>)
				return detail::MakeUtf16(sink, cp);
			else if constexpr (std::is_same_v<ChType, char32_t>)
				return detail::MakeUtf32(sink, cp);
			else if constexpr (std::is_same_v<ChType, wchar_t>)
				return detail::MakeWide(sink, cp);
			else
				return detail::MakeChar(sink, cp);
		}

		template <class ChType, char32_t CodeError>
		constexpr bool EncodeToSingle(auto&& sink, char32_t cp) {
			if (detail::EncodeCodepoint<ChType>(sink, cp))
				return true;

			/* check if the codepoint could not be encoded and handle the error accordingly */
			if constexpr (CodeError == str::SkipInvalid)
				return false;
			else if constexpr (CodeError == str::ThrowInvalid)
				throw str::CodingException("Invalid codepoint encountered in str::Codepoint/str::Transcode");
			if (!detail::EncodeCodepoint<ChType>(sink, CodeError))
				throw str::CodingException("Alternative coding-error codepoint could not be encoded in str::Codepoint/str::Transcode");
			return true;
		}
		template <char32_t CodeError>
		constexpr void EncodeTo(auto&& sink, char32_t cp, size_t count) {
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
					str::CallSink<ChType>(sink, c[0], count);
				else for (size_t i = 0; i < count; ++i)
					str::CallSink<ChType>(sink, c);
			}
		}
	}

	/* [str::IsIterator] Initialized to str::Invalid; call prev()/next() to initialize the state
	*	Will only produce valid unicode codepoints or str::Invalid
	*	str::SkipInvalid will move the iterator forward/backward to the next valid codepoint */
	template <str::IsChar ChType, char32_t CodeError = str::DefErrorChar>
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
			if constexpr (CodeError == str::SkipInvalid) {
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
				if (pOut.cp == str::Invalid) {
					if constexpr (CodeError == str::ThrowInvalid)
						throw str::CodingException("Invalid codepoint encountered in str::Iterator");
					pOut.cp = CodeError;
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
			if constexpr (CodeError == str::SkipInvalid) {
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
				if (pOut.cp == str::Invalid) {
					if constexpr (CodeError == str::ThrowInvalid)
						throw str::CodingException("Invalid codepoint encountered in str::Iterator");
					pOut.cp = CodeError;
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

	/* decode single next codepoint from corresponding type, if its an ascii character, and return null-consumed
	*	if the source is empty, otherwise at least consume one character at all times and return str::Invalid */
	constexpr str::Decoded Ascii(const str::AnyStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};

		/* check if its a char and char does not hold ascii, in which case it has to be fully
		*	decoded (all other types are guaranteed to use unicode and therefore encode ascii) */
		if constexpr (std::is_same_v<ChType, char> && !str::CharHoldsAscii) {
			str::Decoded dec = detail::NextChar(view.data(), view.data() + view.size());
			if (dec.cp >= detail::AsciiRange)
				dec.cp = str::Invalid;
			return dec;
		}

		/* check if the next value is not an ascii-character, in which case it has to be decoded normally to ensure to get the normal length */
		else if (std::make_unsigned_t<ChType>(view[0]) >= detail::AsciiRange) {
			str::Decoded dec = detail::DecodeNext<ChType, false>(view.data(), view.data() + view.size());
			dec.cp = str::Invalid;
			return dec;
		}

		/* cast the single next value as codepoint */
		else
			return { char32_t(std::make_unsigned_t<ChType>(view[0])), 1 };
	}

	/* encode single codepoint to corresponding type and write it to the sink and return it */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr auto& CodepointTo(str::AnySink auto&& sink, char32_t cp, size_t count = 1) {
		detail::EncodeTo<CodeError>(sink, cp, count);
		return sink;
	}

	/* encode single codepoint to corresponding type and write it to an object of the given sink-type using str::CodepointTo */
	template <str::AnySink SinkType, char32_t CodeError = str::DefErrorChar>
	constexpr SinkType Codepoint(char32_t cp, size_t count = 1) {
		SinkType out{};
		detail::EncodeTo<CodeError>(out, cp, count);
		return out;
	}

	/* decode a single codepoint from the source and apply the CodeError handling if necssary (return str::Invalid
	*	if the source is empty or the CodeError is str::SkipInvalid and at least consume one character at all times) */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr str::Decoded ReadCodepoint(const str::AnyStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};
		str::Decoded dec = detail::DecodeNext<ChType, false>(view.data(), view.data() + view.size());

		/* check if an error occurred and the codepoint should either be replaced or an exception raised */
		if constexpr (CodeError != str::SkipInvalid) {
			if (dec.cp == str::Invalid) {
				if constexpr (CodeError == str::ThrowInvalid)
					throw str::CodingException("Invalid codepoint encountered in str::ReadCodepoint");
				dec.cp = CodeError;
			}
		}
		return dec;
	}

	/* decode a single codepoint from the source and apply the CodeError handling if necssary (return str::Invalid if the source is
	*	empty or the next codepoint is incomplete or the CodeError is str::SkipInvalid and at least consume one character at all times) */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr str::Decoded PartialCodepoint(const str::AnyStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};
		str::Decoded dec = detail::DecodeNext<ChType, true>(view.data(), view.data() + view.size());

		/* check if an error occurred and the codepoint should either be replaced or an exception raised */
		if constexpr (CodeError != str::SkipInvalid) {
			if (dec.cp == str::Invalid && dec.consumed > 0) {
				if constexpr (CodeError == str::ThrowInvalid)
					throw str::CodingException("Invalid codepoint encountered in str::ReadCodepoint");
				dec.cp = CodeError;
			}
		}
		return dec;
	}

	/* transcode the source-string as efficient as possible to the sink and return it (does
	*	not guarantee valid encoding, if both source and destination are of the same type) */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr auto& TranscodeTo(str::AnySink auto&& sink, const str::AnyStr auto& source) {
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
				detail::EncodeTo<CodeError>(sink, char32_t(c), 1);
			return sink;
		}

		/* iterate over the source-codepoints and transcode them */
		str::Iterator<SChType, CodeError> it{ view };
		while (it.next())
			detail::EncodeTo<CodeError>(sink, it.get(), 1);
		return sink;
	}

	/* transcode the source-string as efficient as possible to an object of the given sink-type using str::TranscodeTo
	*	and return it (does not guarantee valid encoding, if both source and destination are of the same type) */
	template <str::AnySink SinkType, char32_t CodeError = str::DefErrorChar>
	constexpr SinkType Transcode(const str::AnyStr auto& source) {
		SinkType out{};
		str::TranscodeTo<CodeError>(out, source);
		return out;
	}
}
