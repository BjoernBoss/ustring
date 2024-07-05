#pragma once

#include "unicode/cp-property.h"
#include "str-encode.h"

/*
*	Decoding-issues will consume as many characters as are considered invalid
*
*	Escape sequences:
*		Common patterns (such as \0, \n)
*		\xhh
*		\u{hhhh}
*
*	Escape sequences will be encoded out as single codepoints, not as one whole unit
*/
namespace str {
	namespace detail {
		/* 10 for ascii-encoding of \u{10ffff} */
		static constexpr size_t MaxEscapeHexits = 6;
		static constexpr size_t MaxEscapeSize = detail::MaxEscapeHexits + 4;

		template <class ChType, bool AllowIncomplete>
		constexpr str::Decoded ParseEscaped(const std::basic_string_view<ChType>& view) {
			enum class State : uint8_t {
				none,
				u,
				uBody,
				x
			} state = State::none;
			size_t hexits = 0;

			/* check if a valid escape-sequence is being started (will also catch empty-strings) */
			str::Decoded dec = str::Ascii(view);
			if (dec.cp == str::Invalid || dec.cp != U'\\')
				return dec;

			/* iterate until the next valid escape-sequence has been processed */
			str::Decoded out = { 0, dec.consumed };
			while (out.consumed < view.size()) {
				dec = str::Ascii(view.substr(out.consumed));

				/* check if a valid next character has been encountered */
				if (dec.cp == str::Invalid)
					return { str::Invalid, out.consumed };

				/* check if this is the first character of the escape sequence */
				if (state == State::none) {
					out.consumed += dec.consumed;

					/* check if this is a complex escape sequence */
					if (dec.cp == U'x') {
						state = State::x;
						continue;
					}
					if (dec.cp == U'u') {
						state = State::u;
						continue;
					}

					/* lookup the escape-sequence */
					switch (dec.cp) {
					case U'\\':
					case U'/':
					case U'"':
					case U'\'':
					case U'?':
						out.cp = dec.cp;
						break;
					case U't':
						out.cp = U'\t';
						break;
					case U'r':
						out.cp = U'\r';
						break;
					case U'n':
						out.cp = U'\n';
						break;
					case U'b':
						out.cp = U'\b';
						break;
					case U'f':
						out.cp = U'\f';
						break;
					case U'0':
						out.cp = U'\0';
						break;
					case U'a':
						out.cp = U'\a';
						break;
					case U'v':
						out.cp = U'\v';
						break;
					default:
						out.cp = str::Invalid;
						out.consumed -= dec.consumed;
						break;
					}
					return out;
				}

				/* check if the \xhh sequence has been completed/is still valid */
				if (state == State::x) {
					size_t val = cp::ascii::GetRadix(dec.cp);

					/* check if the next character is valid (out.cp cannot overflow as it has 32-bits and only two hexits will be added) */
					if (val >= 16)
						return { str::Invalid, out.consumed };
					out.cp = out.cp * 16 + char32_t(val);

					/* check if the end has been reached */
					out.consumed += dec.consumed;
					if (++hexits >= 2)
						return out;
					continue;
				}

				/* check if this is the opening bracket of the \u sequence */
				if (state == State::u) {
					if (dec.cp != U'{')
						return { str::Invalid, out.consumed };
					state = State::uBody;
					out.consumed += dec.consumed;
					continue;
				}

				/* check if the end of the \u sequence has been reached and the codepoint is valid */
				if (hexits > 0 && dec.cp == U'}') {
					out.consumed += dec.consumed;
					if ((out.cp >= detail::SurrogateFirst && out.cp <= detail::SurrogateLast) || out.cp >= detail::UnicodeRange)
						out.cp = str::Invalid;
					return out;
				}

				/* check if a valid digit has been encountered and update the value (if first digit was null, no other digits are allowed) */
				if ((out.cp == 0 && hexits > 0) || hexits >= detail::MaxEscapeHexits)
					return { str::Invalid, out.consumed };
				size_t val = cp::ascii::GetRadix(dec.cp);
				if (val >= 16)
					return { str::Invalid, out.consumed };
				out.cp = out.cp * 16 + char32_t(val);
				out.consumed += dec.consumed;
				++hexits;
			}

			/* the string was empty or the escape-sequence was incomplete */
			if constexpr (AllowIncomplete)
				return { str::Invalid, 0 };
			return { str::Invalid, out.consumed };
		}

		template <char32_t CodeError>
		constexpr void EscapeTo(auto&& sink, char32_t cp, bool compact, size_t count) {
			const char32_t* escaped = 0;

			/* check if the character is an escape sequence */
			if (cp == U'\t')
				escaped = (compact ? U"\\t" : U"\t");
			else if (cp == U'\n')
				escaped = (compact ? U"\\n" : U"\n");
			else if (cp == U'\0')
				escaped = U"\\0";
			else if (cp == U'\r')
				escaped = (compact ? U"\\r" : U"\r");
			else if (cp == U'\"')
				escaped = (compact ? U"\\\"" : U"\"");
			else if (cp == U'\'')
				escaped = (compact ? U"\\'" : U"'");
			else if (cp == U'\\')
				escaped = U"\\\\";

			/* check if an escape sequence was found */
			if (escaped != 0) {
				if (escaped[1] == U'\0')
					str::CodepointTo<CodeError>(sink, escaped[0], count);
				else for (size_t i = 0; i < count; ++i)
					str::TranscodeTo<CodeError>(sink, escaped);
			}

			/* check if the character can be added as-is */
			else if (cp::prop::IsAscii(cp) && !cp::prop::IsControl(cp))
				str::CodepointTo<CodeError>(sink, cp, count);

			/* check if the codepoint can be added as short-version */
			else if (cp <= 0xff) {
				char32_t buf[4] = { U'\\', U'x', cp::ascii::GetRadixLower(cp >> 4), cp::ascii::GetRadixLower(cp & 0x0f) };
				for (size_t i = 0; i < count; ++i)
					str::TranscodeTo<CodeError>(sink, std::u32string_view{ buf, 4 });
			}

			/* add the codepoint as the unicode-codepoint (must be greater than 0xff, hence non-zero) */
			else {
				int32_t shift = 28;
				while (((cp >> shift) & 0x0f) == 0)
					shift -= 4;

				/* write the escaped string to the temporary buffer */
				char32_t buf[detail::MaxEscapeSize] = { U'\\', U'u', U'{', 0 };
				size_t len = 3;
				do {
					buf[len++] = cp::ascii::GetRadixLower((cp >> shift) & 0x0f);
				} while ((shift -= 4) >= 0);
				buf[len++] = U'}';

				for (size_t i = 0; i < count; ++i)
					str::TranscodeTo<CodeError>(sink, std::u32string_view{ buf, len });
			}
		}

		template <class ChType>
		struct EscScale;
		template <> struct EscScale<char8_t> { static constexpr size_t value = 1; };
		template <> struct EscScale<char16_t> { static constexpr size_t value = 1; };
		template <> struct EscScale<char32_t> { static constexpr size_t value = 1; };
		template <> struct EscScale<wchar_t> { static constexpr size_t value = (str::WideIsUtf16 ? detail::EscScale<char16_t>::value : detail::EscScale<char32_t>::value); };
		template <> struct EscScale<char> { static constexpr size_t value = ((str::CharIsUtf8 || str::CharHoldsAscii) ? 1 : str::MaxEncSize<char>); };
	}

	/* maximum codepoint length of a single escape-sequence */
	static constexpr size_t MaxEscapeSize = detail::MaxEscapeSize;

	/* local-string to hold the single escaped codepoint for the corresponding type */
	template <str::IsChar ChType>
	using Escaped = str::Local<ChType, str::MaxEscapeSize* detail::EscScale<ChType>::value>;

	/* create the escape-sequence in ascii-only characters using ascii characters, common escape sequences, \xhh,
	*	\u{(0|[1-9a-fA-F]h*)} and write it to the sink and return it (compact is designed for one-liner strings) */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr auto& EscapeTo(str::AnySink auto&& sink, char32_t cp, bool compact, size_t count = 1) {
		detail::EscapeTo<CodeError>(sink, cp, compact, count);
		return sink;
	}

	/* create the escape-sequence in ascii-only characters using ascii characters, common escape sequences, \xhh, \u{(0|[1-9a-fA-F]h*)}
	*	and write it to an object of the given sink-type using str::EscapeTo (compact is designed for one-liner strings) */
	template <str::AnySink SinkType, char32_t CodeError = str::DefErrorChar>
	constexpr SinkType Escape(char32_t cp, bool compact, size_t count = 1) {
		SinkType out{};
		detail::EscapeTo<CodeError>(out, cp, compact, count);
		return out;
	}

	/* parse the escape-sequence in the style as produced by str::EscapeTo (return null-consumed if the source is empty, otherwise at least
	*	consume one character at all times and return cp::Invalid on invalid escape sequences and if CodeError is str::SkipInvalid) */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr str::Decoded ReadEscaped(const str::AnyStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};
		str::Decoded dec = detail::ParseEscaped<ChType, false>(view);

		/* check if an error occurred and the codepoint should either be replaced or an exception raised */
		if constexpr (CodeError != str::SkipInvalid) {
			if (dec.cp == str::Invalid) {
				if constexpr (CodeError == str::ThrowInvalid)
					throw str::CodingException("Invalid codepoint encountered in str::ReadEscaped");
				dec.cp = CodeError;
			}
		}
		return dec;
	}

	/* parse the escape-sequence in the style as produced by str::EscapeTo (return null-consumed if the source is empty or the next codepoint is incomplete,
	*	otherwise at least consume one character at all times and return cp::Invalid on invalid escape sequences and if CodeError is str::SkipInvalid) */
	template <char32_t CodeError = str::DefErrorChar>
	constexpr str::Decoded PartialEscaped(const str::AnyStr auto& source) {
		using ChType = str::StrChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return {};
		str::Decoded dec = detail::ParseEscaped<ChType, true>(view);

		/* check if an error occurred and the codepoint should either be replaced or an exception raised */
		if constexpr (CodeError != str::SkipInvalid) {
			if (dec.cp == str::Invalid && dec.consumed > 0) {
				if constexpr (CodeError == str::ThrowInvalid)
					throw str::CodingException("Invalid codepoint encountered in str::ReadEscaped");
				dec.cp = CodeError;
			}
		}
		return dec;
	}
}
