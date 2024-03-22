#pragma once

#include "str-common.h"

#include <algorithm>

/*
*	CodePoint is identical to char32_t and thereby utf-32
*/
namespace str::cp {
	/*
	*	Success: returned for all non-codepoint returning but succeeding operations (implicitly also valid)
	*	Empty: Source was empty and therefore nothing decoded
	*	Invalid: Encoding error was detected in source or encoded codepoint is invalid or did not use the proper encoding-form
	*	Incomplete: Codepoint seems to be encoded properly, but missing one or more characters for completion, no characters will be consumed
	*		=> if source is completed, return value will never be incomplete and at least one character will be consumed
	*	NotAscii: Read-ascii encountered a non-ascii character
	*	WriteFailed: Transcoding a character decoded it successfully from the source but could not encode it to the destination
	*/
	static constexpr char32_t Success = char32_t(0);
	static constexpr char32_t Empty = char32_t(-1);
	static constexpr char32_t Invalid = char32_t(-2);
	static constexpr char32_t Incomplete = char32_t(-3);
	static constexpr char32_t NotAscii = char32_t(-4);
	static constexpr char32_t WriteFailed = char32_t(-5);

	/* check if the codepoint is a defined error-codepoint, excluding cp::Success (does not check if the codepoint itself lies in a valid unicode-range) */
	inline constexpr bool Valid(char32_t cp) {
		/* char32_t is guaranteed to be unsigned */
		return (cp >= std::min<char32_t>({ cp::Empty, cp::Invalid, cp::Incomplete, cp::NotAscii, cp::WriteFailed }));
	}

	/* number of ascii characters with valid range: [0, 127] */
	static constexpr size_t AsciiRange = 128;

	/* number of unicode characters lie within range: [0, 0x10ffff] */
	static constexpr size_t UnicodeRange = 0x110000;

	/* surrogate-pair boundaries */
	static constexpr uint16_t SurrogateFirst = 0xd800;
	static constexpr uint16_t SurrogateUpper = 0xdc00;
	static constexpr uint16_t SurrogateLast = 0xdfff;

	/* default error character (guaranteed to be an ascii-character) */
	static constexpr char32_t DefErrorChar = U'?';

	/* check if the codepoint is valid (within unicode-range and not a surrograte-pair) */
	inline constexpr bool Unicode(char32_t cp) {
		/* char32_t is guaranteed to be unsigned */
		return (cp < cp::UnicodeRange && (cp < cp::SurrogateFirst || cp > cp::SurrogateLast));
	}

	/* check if the codepoint is an ascii character (can also be used on upcasted characters which can directly encode ascii) */
	inline constexpr bool Ascii(char32_t cp) {
		/* char32_t is guaranteed to be unsigned */
		return (cp < cp::AsciiRange);
	}

	namespace detail {
		static constexpr uint8_t NotAnAsciiDigit = 0xff;
		static constexpr size_t LastAsciiDigit = 35;

		/* also directly used by str-numbers.h */
		static constexpr const char32_t* AsciiDigitLower = U"0123456789abcdefghijklmnopqrstuvwxyz";
		static constexpr const char32_t* AsciiDigitUpper = U"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		static constexpr uint8_t AsciiDigitMap[cp::AsciiRange] = {
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
			0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
			0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0xff, 0xff, 0xff, 0xff, 0xff
		};
	}

	/* map ascii characters (0-9; a-z; A-Z) to values between 0-35 or return cp::NotAsciiDigit
	*	(guaranteed to be larger than any valid digit, i.e. if larger than expected radix, invalid) */
	static constexpr size_t NotAsciiDigit = detail::NotAnAsciiDigit;
	inline constexpr size_t AsciiToDigit(char32_t cp) {
		if (!cp::Ascii(cp))
			return cp::NotAsciiDigit;
		return detail::AsciiDigitMap[cp];
	}

	/* return ascii character codepoint or cp::Invalid if digit is greater than 35 */
	inline constexpr char32_t DigitToAscii(size_t digit, bool upperCase = false) {
		if (digit > detail::LastAsciiDigit)
			return cp::Invalid;
		return (upperCase ? detail::AsciiDigitUpper : detail::AsciiDigitLower)[digit];
	}
}
