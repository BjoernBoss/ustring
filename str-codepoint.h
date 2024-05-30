#pragma once

#include "str-common.h"

#include "generated/unicode-property.h"

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

	/* check if the codepoint is a valid unicode-codepoint (in principle, i.e. in Unicode-Range and not a surrogate-pair) */
	inline constexpr bool IsUnicode(char32_t cp) {
		return detail::gen::TestUnicode(cp);
	}

	/* query properties about single codepoints optimized but only yield results for ascii */
	namespace ascii {
		/* check if the codepoint is an ascii alpha character [a-zA-Z] */
		inline constexpr bool IsAlpha(char32_t cp) {
			return detail::gen::TestAsciiAlphabetic(cp);
		}

		/* check if the codepoint is an ascii numeric character [0-9] */
		inline constexpr bool IsNum(char32_t cp) {
			return detail::gen::TestAsciiNumeric(cp);
		}

		/* check if the codepoint is an ascii alpha/numeric character [a-zA-Z0-9] */
		inline constexpr bool IsAlNum(char32_t cp) {
			return (detail::gen::TestAsciiAlphabetic(cp) || detail::gen::TestAsciiNumeric(cp));
		}

		/* check if the codepoint is an ascii alpha/numerical character [a-zA-Z0-9] and return it as a radix from [0 - 35]
		*	and returns 0xff (a value larger than any actual radix) on failure (i.e. to check if hexit: str::GetAsciiRadix < 16) */
		static constexpr size_t ErrRadix = 0xff;
		inline constexpr size_t GetRadix(char32_t cp) {
			size_t val = detail::gen::GetAsciiRadix(cp);
			return (val == detail::gen::AsciiRadixNone ? ascii::ErrRadix : val);
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
	}

	/* query unicode properties about single codepoints */
	namespace prop {
		/* check if the codepoint is an ascii character [\x00-\x7f] */
		inline constexpr bool IsAscii(char32_t cp) {
			return detail::gen::TestAscii(cp);
		}

		/* check if the codepoint is a whitespace character [Unicode White_Space] */
		inline constexpr bool IsSpace(char32_t cp) {
			return detail::gen::TestWhiteSpace(cp);
		}

		/* check if the codepoint is a control character [Unicode C0, C1] */
		inline constexpr bool IsControl(char32_t cp) {
			return detail::gen::TestControl(cp);
		}

		/* check if the codepoint does not have the General_Category Cn, Cs, Co */
		inline constexpr bool IsAssigned(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			return ((prop >> detail::gen::PropertyAssignedOff) & detail::gen::PropertyAssignedBits) != 0;
		}

		/* check if the codepoint is a alpha character [Unicode Alphabetic] */
		inline constexpr bool IsAlpha(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			return ((prop >> detail::gen::PropertyAlphabeticOff) & detail::gen::PropertyAlphabeticBits) != 0;
		}

		/* check if the codepoint is a numeric character [Unicode Numeric_Type=Numeric/Decimal/Digit] */
		inline constexpr bool IsNumeric(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			return ((prop >> detail::gen::PropertyNumericOff) & detail::gen::PropertyNumericBits) != 0;
		}

		/* check if the codepoint is a alpha/numeric character [Unicode Numeric_Type=Numeric/Decimal/Digit or Unicode Alphabetic] */
		inline constexpr bool IsAlNum(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			if (((prop >> detail::gen::PropertyAlphabeticOff) & detail::gen::PropertyAlphabeticBits) != 0)
				return true;
			return ((prop >> detail::gen::PropertyNumericOff) & detail::gen::PropertyNumericBits) != 0;
		}

		/* check if the codepoint is a decimal character and return its value [0-9] or 0xff if not a decimal codepoint [Unicode Numeric_Type::Decimal] */
		static constexpr size_t ErrDecimal = 0xff;
		inline constexpr size_t GetDecimal(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			size_t val = ((prop >> detail::gen::PropertyDecimalOff) & detail::gen::PropertyDecimalOff);
			return (val == detail::gen::PropertyDecimalNone ? prop::ErrDecimal : val);
		}

		/* check if the codepoint is printable [Unicode General_Category L*,M*,N*,P*,S* and either any Zs or only U' '] */
		inline constexpr bool IsPrint(char32_t cp, bool reducedSpace = false) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::PrintableType val = static_cast<detail::gen::PrintableType>((prop >> detail::gen::PropertyPrintableOff) & detail::gen::PropertyPrintableBits);
			if (val == detail::gen::PrintableType::none)
				return false;
			if (val == detail::gen::PrintableType::printable)
				return true;
			return (!reducedSpace || cp == U' ');
		}

		/* check if the codepoint is graphical [Unicode General_Category L*,M*,N*,P*,S* without Zs] */
		inline constexpr bool IsGraphic(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::PrintableType val = static_cast<detail::gen::PrintableType>((prop >> detail::gen::PropertyPrintableOff) & detail::gen::PropertyPrintableBits);
			return (val == detail::gen::PrintableType::printable);
		}

		/* analzye the casing of the codepoint [Unicode property Lowercase,Uppercase or General_Category Lt] */
		enum class CaseType : uint8_t {
			none = detail::gen::CaseType::none,
			lowerCase = detail::gen::CaseType::lowerCase,
			upperCase = detail::gen::CaseType::upperCase,
			titleCase = detail::gen::CaseType::titleCase
		};
		inline constexpr prop::CaseType GetCase(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseBits);
			return static_cast<prop::CaseType>(val);
		}

		/* check if the codepoint has different cases [Unicode property Lowercase, Uppercase or General_Category Lt] */
		inline constexpr bool IsCased(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseBits);
			return (val != detail::gen::CaseType::none);
		}

		/* check if the codepoint is uppercase [Unicode property Uppercase] */
		inline constexpr bool IsUpper(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseBits);
			return (val == detail::gen::CaseType::upperCase);
		}

		/* check if the codepoint is lowercase [Unicode property Lowercase] */
		inline constexpr bool IsLower(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseBits);
			return (val == detail::gen::CaseType::lowerCase);
		}

		/* check if the codepoint is titlecase [Unicode General_Category Lt] */
		inline constexpr bool IsTitle(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseBits);
			return (val == detail::gen::CaseType::titleCase);
		}

		/* analyze the general category of the codepoint [Unicode General_Category] */
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
		inline constexpr prop::GCType GetCategory(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return static_cast<prop::GCType>(val);
		}

		/* check if the codepoint belongs to the letter category [Unicode General_Category L*] */
		inline constexpr bool IsGCLetter(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return (val == detail::gen::CategoryType::lu
				|| val == detail::gen::CategoryType::ll
				|| val == detail::gen::CategoryType::lt
				|| val == detail::gen::CategoryType::lm
				|| val == detail::gen::CategoryType::lo);
		}

		/* check if the codepoint belongs to the mark category [Unicode General_Category M*] */
		inline constexpr bool IsGCMark(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return (val == detail::gen::CategoryType::mn
				|| val == detail::gen::CategoryType::mc
				|| val == detail::gen::CategoryType::me);
		}

		/* check if the codepoint belongs to the number category [Unicode General_Category N*] */
		inline constexpr bool IsGCNumber(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return (val == detail::gen::CategoryType::nd
				|| val == detail::gen::CategoryType::nl
				|| val == detail::gen::CategoryType::no);
		}

		/* check if the codepoint belongs to the punctuation category [Unicode General_Category P*] */
		inline constexpr bool IsGCPunctuation(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
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
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return (val == detail::gen::CategoryType::sm
				|| val == detail::gen::CategoryType::sc
				|| val == detail::gen::CategoryType::sk
				|| val == detail::gen::CategoryType::so);
		}

		/* check if the codepoint belongs to the separator category [Unicode General_Category Z*] */
		inline constexpr bool IsGCSeparator(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return (val == detail::gen::CategoryType::zs
				|| val == detail::gen::CategoryType::zl
				|| val == detail::gen::CategoryType::zp);
		}

		/* check if the codepoint belongs to the other category [Unicode General_Category C*] */
		inline constexpr bool IsGCOther(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryBits);
			return (val == detail::gen::CategoryType::cc
				|| val == detail::gen::CategoryType::cf
				|| val == detail::gen::CategoryType::cs
				|| val == detail::gen::CategoryType::co
				|| val == detail::gen::CategoryType::cn);
		}
	}

	namespace detail {
		template <class Type, size_t Buffer>
		class LocalBuffer {
		private:
			struct Static {
				Type buffer[Buffer]{};
			};
			using Dynamic = std::vector<Type>;

		private:
			std::variant<Static, Dynamic> pBuffer;
			Type* pBegin = 0;
			Type* pEnd = 0;

		public:
			constexpr LocalBuffer() : pBuffer{ Static{} } {
				pBegin = std::get<Static>(pBuffer).buffer;
				pEnd = pBegin;
			}

		public:
			constexpr void push(const Type& t) {
				if (std::holds_alternative<Dynamic>(pBuffer)) {
					Dynamic& d = std::get<Dynamic>(pBuffer);
					if (size_t(pEnd - d.data()) >= d.size()) {
						size_t bOff = pBegin - d.data(), eOff = pEnd - d.data();
						d.resize(d.size() + Buffer);
						pBegin = d.data() + bOff;
						pEnd = d.data() + eOff;
					}
				}
				else if (pEnd - std::get<Static>(pBuffer).buffer >= Buffer) {
					Dynamic v{ pBegin, pEnd };
					v.push_back(t);
					pBuffer = std::move(v);
					pBegin = std::get<Dynamic>(pBuffer).data();
					pEnd = pBegin + std::get<Dynamic>(pBuffer).size();
					return;
				}
				*pEnd = t;
				++pEnd;
			}
			constexpr Type pop() {
				Type val = *pBegin;
				if (++pBegin == pEnd) {
					if (std::holds_alternative<Static>(pBuffer))
						pBegin = std::get<Static>(pBuffer).buffer;
					else
						pBegin = std::get<Dynamic>(pBuffer).data();
					pEnd = pBegin;
				}
				return val;
			}
			constexpr void clear() {
				if (std::holds_alternative<Static>(pBuffer))
					pBegin = std::get<Static>(pBuffer).buffer;
				else
					pBegin = std::get<Dynamic>(pBuffer).data();
				pEnd = pBegin;
			}
			constexpr size_t size() const {
				return (pEnd - pBegin);
			}
			constexpr Type& get(size_t i) {
				return pBegin[i];
			}
			constexpr Type& front() {
				return pBegin[0];
			}
			constexpr Type& back() {
				return pEnd[-1];
			}
			constexpr Type* begin() {
				return pBegin;
			}
			constexpr Type* end() {
				return pEnd;
			}
		};

		using EmptyLambda = decltype([](char32_t) {});
	}

	/* valid sinks for char32_t must receive zero or more valid codepoints and a final call to done(), after which
	*	the object is considered burnt (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type, class... ValType>
	concept IsSink = requires(Type t, ValType... v) {
		{ t(v...) } -> std::same_as<void>;
	};

	/* codepoint-iterator must move itself upon prev()/next() and return true or return false (in which case it must stay)
	*	and must return the currently pointed to codepoint on get() */
	template <class Type>
	concept IsCPIterator = std::copyable<Type> && requires(Type t, const Type ct) {
		{ t.prev() } -> std::same_as<bool>;
		{ t.next() } -> std::same_as<bool>;
		{ ct.get() } -> std::same_as<char32_t>;
	};

	/* codepoint-mapper must receive a callable sink of type void(char32_t) and return an object, which provides next/done
	*	functions to consume the next codepoints and process them and the type of the object must be exposed as ::Type */
	template <class Type>
	concept IsMapper = requires(const Type t, char32_t c, detail::EmptyLambda l) {
		typename Type::template Type<detail::EmptyLambda>;
		{ t(l) } -> std::same_as<typename Type::template Type<detail::EmptyLambda>>;
		{ t(l).next(c) } -> std::same_as<void>;
		{ t(l).done() } -> std::same_as<void>;
	};

	/* codepoint-tester must provide next/done functions to consume the next codepoints and process them and return the final result on done */
	template <class Type, class ResType>
	concept IsTester = requires(Type t, char32_t c) {
		{ t.next(c) } -> std::same_as<void>;
		{ t.done() } -> std::same_as<ResType>;
	};
}
