#pragma once

#include "str-common.h"

#include <algorithm>
#include <type_traits>
#include <vector>
#include <variant>

/*
*	CodePoint is identical to char32_t and thereby utf-32
*
*	All CodePoint functions can receive any value as codepoint, well or ill defined.
*/
namespace cp {
	/*
	*	Success: returned for all non-codepoint returning but succeeding operations (implicitly also valid)
	*	Invalid: Encoding error was detected in source or encoded codepoint is invalid or did not use the proper encoding-form
	*	Empty: Source was empty and therefore nothing decoded
	*	Incomplete: Codepoint seems to be encoded properly, but missing one or more characters for completion, no characters will be consumed
	*		=> if source is completed, return value will never be incomplete and at least one character will be consumed
	*	NotAscii: Read-ascii encountered a non-ascii character
	*	WriteFailed: Transcoding a character decoded it successfully from the source but could not encode it to the destination
	*/
	static constexpr char32_t Success = char32_t(0);
	static constexpr char32_t Invalid = char32_t(-1);
	static constexpr char32_t Empty = char32_t(-2);
	static constexpr char32_t Incomplete = char32_t(-3);
	static constexpr char32_t NotAscii = char32_t(-4);
	static constexpr char32_t WriteFailed = char32_t(-5);

	/* check if the codepoint is a defined error-codepoint, excluding cp::Success (does not check if the codepoint itself
	*	lies in a valid unicode-range, only useful to check if codepoint returning function failed or succeeded) */
	inline constexpr bool Valid(char32_t cp) {
		/* char32_t is guaranteed to be unsigned */
		return (cp < std::min<char32_t>({ cp::Empty, cp::Invalid, cp::Incomplete, cp::NotAscii, cp::WriteFailed }));
	}

	/* number of ascii characters lie within range: [0, 127] */
	static constexpr size_t AsciiRange = 128;

	/* number of unicode characters lie within range: [0, 0x10ffff] */
	static constexpr size_t UnicodeRange = 0x110000;

	/* default error character (guaranteed to be an ascii-character) */
	static constexpr char32_t DefErrorChar = U'?';

	/* codepoint-tester must provide next/done functions to consume the next codepoints and process them and return the final result on done */
	template <class Type, class ResType>
	concept IsTester = requires(Type t, char32_t c) {
		{ t.next(c) } -> std::same_as<void>;
		{ t.done() } -> std::same_as<ResType>;
	};
}
