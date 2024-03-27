#pragma once

#include "str-common.h"
#include "generated/unicode-cp-query.h"

#include <algorithm>

/*
*	CodePoint is identical to char32_t and thereby utf-32
*/
namespace cp {
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

	/* check if the codepoint is a defined error-codepoint, excluding cp::Success (does not check if the codepoint itself
	*	lies in a valid unicode-range, only useful to check if code-point returning function failed or succeeded) */
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

	/* check if the codepoint is a valid unicode-codepoint (in principle, i.e. in Unicode-Range and not a surrogate-pair) */
	inline constexpr bool IsUnicode(char32_t cp) {
		return detail::gen::TestUnicode(cp);
	}

	/* check if the codepoint does not have the General_Category Cn, Cs, Co */
	inline constexpr bool IsAssigned(char32_t cp) {
		return detail::gen::TestAssigned(cp);
	}

	/* check if the codepoint is an ascii character [ <= 0x7f] */
	inline constexpr bool IsAscii(char32_t cp) {
		return detail::gen::TestAscii(cp);
	}

	/* check if the codepoint is an ascii alpha character [a-zA-Z] */
	inline constexpr bool IsAlpha(char32_t cp) {
		return detail::gen::TestAlpha(cp);
	}

	/* check if the codepoint is a decimal character [value within 0-9; Unicode Numeric_Type::Decimal] */
	inline constexpr bool IsDecimal(char32_t cp) {
		return (detail::gen::GetDigit(cp) < 10);
	}

	/* check if the codepoint is a digit character [Unicode Numeric_Type::Digit] */
	inline constexpr bool IsDigit(char32_t cp) {
		return (detail::gen::GetDigit(cp) < 0xf0);
	}

	/* check if the codepoint is a numerical character [Unicode Numeric_Type::Numeric] */
	inline constexpr bool IsNumeric(char32_t cp) {
		return (detail::gen::GetDigit(cp) < 0xf1);
	}

	/* check if the codepoint is a whitespace character [Unicode White_Space] */
	inline constexpr bool IsSpace(char32_t cp) {
		return detail::gen::TestWhiteSpace(cp);
	}

	/* check if the codepoint is a control character [Unicode C0, C1] */
	inline constexpr bool IsControl(char32_t cp) {
		return detail::gen::TestControl(cp);
	}

	/* check if the codepoint is a letter [Unicode Alphabetic] */
	inline constexpr bool IsLetter(char32_t cp) {
		return detail::gen::TestLetter(cp);
	}

	/* check if the codepoint is a letter or numerical [Unicode Alphabetic or Numeric_Type::Decimal or Numeric_Type::Digit or Numeric_Type::Numeric] */
	inline constexpr bool IsAlNum(char32_t cp) {
		return detail::gen::TestAlNum(cp);
	}

	/* check if the codepoint is printable [Unicode General_Category L*,M*,N*,P*,S* and either any Zs or only U' '] */
	inline constexpr bool IsPrint(char32_t cp, bool reducedSpace = false) {
		detail::gen::PrintableType val = detail::gen::GetPrintable(cp);
		if (val == detail::gen::PrintableType::none)
			return false;
		if (val == detail::gen::PrintableType::printable)
			return true;
		return (!reducedSpace || cp == U' ');
	}

	/* check if the codepoint is graphical [Unicode General_Category L*,M*,N*,P*,S* without Zs] */
	inline constexpr bool IsGraphic(char32_t cp) {
		return (detail::gen::GetPrintable(cp) == detail::gen::PrintableType::printable);
	}

	/* check if the codepoint is has different cases [Unicode property Lowercase,Uppercase or General_Category Lt] */
	inline constexpr bool IsCased(char32_t cp) {
		return (detail::gen::GetCase(cp) != detail::gen::CaseType::none);
	}

	/* check if the codepoint is uppercase [Unicode property Uppercase] */
	inline constexpr bool IsUpper(char32_t cp) {
		return (detail::gen::GetCase(cp) == detail::gen::CaseType::upperCase);
	}

	/* check if the codepoint is lowercase [Unicode property Lowercase] */
	inline constexpr bool IsLower(char32_t cp) {
		return (detail::gen::GetCase(cp) == detail::gen::CaseType::lowerCase);
	}

	/* check if the codepoint is titlecase [Unicode General_Category Lt] */
	inline constexpr bool IsTitle(char32_t cp) {
		return (detail::gen::GetCase(cp) == detail::gen::CaseType::titleCase);
	}

	/* check if the codepoint belongs to the letter category [Unicode General_Category L*] */
	inline constexpr bool IsGCLetter(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::lu
			|| val == detail::gen::CategoryType::ll
			|| val == detail::gen::CategoryType::lt
			|| val == detail::gen::CategoryType::lm
			|| val == detail::gen::CategoryType::lo);
	}

	/* check if the codepoint belongs to the mark category [Unicode General_Category M*] */
	inline constexpr bool IsGCMark(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::mn
			|| val == detail::gen::CategoryType::mc
			|| val == detail::gen::CategoryType::me);
	}

	/* check if the codepoint belongs to the number category [Unicode General_Category N*] */
	inline constexpr bool IsGCNumber(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::nd
			|| val == detail::gen::CategoryType::nl
			|| val == detail::gen::CategoryType::no);
	}

	/* check if the codepoint belongs to the punctuation category [Unicode General_Category P*] */
	inline constexpr bool IsGCPunctuation(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::pc
			|| val == detail::gen::CategoryType::pd
			|| val == detail::gen::CategoryType::ps
			|| val == detail::gen::CategoryType::pe
			|| val == detail::gen::CategoryType::pi
			|| val == detail::gen::CategoryType::pf
			|| val == detail::gen::CategoryType::po);
	}

	/* check if the codepoint belongs to the symbol category [Unicode General_Category S*] */
	inline constexpr bool IsGCSymbol(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::sm
			|| val == detail::gen::CategoryType::sc
			|| val == detail::gen::CategoryType::sk
			|| val == detail::gen::CategoryType::so);
	}

	/* check if the codepoint belongs to the separator category [Unicode General_Category Z*] */
	inline constexpr bool IsGCSeparator(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::zs
			|| val == detail::gen::CategoryType::zl
			|| val == detail::gen::CategoryType::zp);
	}

	/* check if the codepoint belongs to the other category [Unicode General_Category C*] */
	inline constexpr bool IsGCOther(char32_t cp) {
		detail::gen::CategoryType val = detail::gen::GetCategory(cp);
		return (val == detail::gen::CategoryType::cc
			|| val == detail::gen::CategoryType::cf
			|| val == detail::gen::CategoryType::cs
			|| val == detail::gen::CategoryType::co
			|| val == detail::gen::CategoryType::cn);
	}

	/* analzye the casing of the codepoint [Unicode property Lowercase,Uppercase or General_Category Lt] */
	enum class CaseType : uint8_t {
		none = detail::gen::CaseType::none,
		lowerCase = detail::gen::CaseType::lowerCase,
		upperCase = detail::gen::CaseType::upperCase,
		titleCase = detail::gen::CaseType::titleCase
	};
	inline constexpr cp::CaseType GetCase(char32_t cp) {
		return static_cast<cp::CaseType>(detail::gen::GetCase(cp));
	}

	/* analzye the general category of the codepoint [Unicode General_Category] */
	enum class GCType : uint8_t {
		uppercaseLetter = detail::gen::CategoryType::lu,
		lowercaseLetter = detail::gen::CategoryType::ll,
		titlecaseLetter = detail::gen::CategoryType::lt,
		modifierLetter = detail::gen::CategoryType::lm,
		otherLetter = detail::gen::CategoryType::lo,
		nonspacingMark = detail::gen::CategoryType::mn,
		spacingMark = detail::gen::CategoryType::mc,
		encloingMark = detail::gen::CategoryType::me,
		decimalNumber = detail::gen::CategoryType::nd,
		letterNumber = detail::gen::CategoryType::nl,
		otherNumber = detail::gen::CategoryType::no,
		connectorPunctuation = detail::gen::CategoryType::pc,
		dashPunctuation = detail::gen::CategoryType::pd,
		openPunctuation = detail::gen::CategoryType::ps,
		closePunctuation = detail::gen::CategoryType::pe,
		initialPunctuation = detail::gen::CategoryType::pi,
		finalPunctuation = detail::gen::CategoryType::pf,
		otherPunctuation = detail::gen::CategoryType::po,
		mathSymbol = detail::gen::CategoryType::sm,
		currencySymbol = detail::gen::CategoryType::sc,
		modifierSymbol = detail::gen::CategoryType::sk,
		otherSymbol = detail::gen::CategoryType::so,
		spaceSeparator = detail::gen::CategoryType::zs,
		lineSeparator = detail::gen::CategoryType::zl,
		paragraphSeparator = detail::gen::CategoryType::zp,
		control = detail::gen::CategoryType::cc,
		format = detail::gen::CategoryType::cf,
		surrogate = detail::gen::CategoryType::cs,
		privateUse = detail::gen::CategoryType::co,
		unassigned = detail::gen::CategoryType::cn,
		Lu = uppercaseLetter,
		Ll = lowercaseLetter,
		Lt = titlecaseLetter,
		Lm = modifierLetter,
		Lo = otherLetter,
		Mn = nonspacingMark,
		Mc = spacingMark,
		Me = encloingMark,
		Nd = decimalNumber,
		Nl = letterNumber,
		No = otherNumber,
		Pc = connectorPunctuation,
		Pd = dashPunctuation,
		Ps = openPunctuation,
		Pe = closePunctuation,
		Pi = initialPunctuation,
		Pf = finalPunctuation,
		Po = otherPunctuation,
		Sm = mathSymbol,
		Sc = currencySymbol,
		Sk = modifierSymbol,
		So = otherSymbol,
		Zs = spaceSeparator,
		Zl = lineSeparator,
		Zp = paragraphSeparator,
		Cc = control,
		Cf = format,
		Cs = surrogate,
		Co = privateUse,
		Cn = unassigned
	};
	inline constexpr cp::GCType GetCategory(char32_t cp) {
		return static_cast<cp::GCType>(detail::gen::GetCategory(cp));
	}

	/* check if the codepoint is an ascii alpha/numerical character [a-zA-Z0-9] and return it as a radix from [0 - 35]
	*	 and returns 0xff (a value larger than any actual radix) on failure (i.e. to check if hexit: str::GetRadix < 16) */
	static constexpr size_t ErrRadix = 0xff;
	inline constexpr size_t GetRadix(char32_t cp) {
		return detail::gen::GetRadix(cp);
	}

	/* map value [0-35] to [0-9a-z] or return cp::Invalid on bounds-issues */
	inline constexpr char32_t GetRadixLower(size_t val) {
		if (val > 35)
			return cp::Invalid;
		return U"0123456789abcdefghijklmnopqrstuvwxyz"[val];
	}

	/* map value [0-35] to [0-9A-Z] or return cp::Invalid on bounds-issues */
	inline constexpr char32_t GetRadixUpper(size_t val) {
		if (val > 35)
			return cp::Invalid;
		return U"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[val];
	}

	/* check if the character is a decimal value and either return its value (0-9) or 0xff on failure */
	static constexpr size_t ErrDecimal = 0xff;
	inline constexpr size_t GetDecimal(char32_t cp) {
		size_t val = detail::gen::GetDigit(cp);
		if (val >= 10)
			return cp::ErrDecimal;
		return val;
	}
}
