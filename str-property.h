#pragma once

#include "str-common.h"
#include "str-codepoint.h"

#include "generated/unicode-property.h"

namespace cp {
	namespace detail {
		class EmojiAnalysis {
			static_assert(size_t(detail::gen::EmojiType::_last) == 10, "Only types 0-9 are known by the state-machine");
		public:
			using Type = detail::gen::EmojiType;
		private:
			enum class State : uint8_t {
				uninit,
				reinit,
				burnt,
				textEmojiUninit,
				imageEmojiUninit,
				imageEmojiReinit,
				keyCapSeqFirst,
				keyCapSeqSecond,
				modSeqStart,
				flagSeqStart,
				tagSeqStart,
				textKeyCapSeq,
				imageKeyCapSeq,
				modSeq,
				flagSeq,
				tagSeq,
				textPres,
				emojiPres
			};

		private:
			Type pLast = Type::_last;
			State pState = State::uninit;
			bool pEmoji = false;
			bool pText = false;

		public:
			constexpr EmojiAnalysis(bool emoji, bool text) : pEmoji(emoji), pText(text) {}

		private:
			constexpr State fUpdate(State state, Type type, bool isEmoji, bool isEmojiPres) {
				switch (state) {
				case State::uninit:
					/* check if its an emoji character (base of multiple different emoji-types) */
					if (isEmoji) {
						pLast = type;
						return (isEmojiPres ? State::imageEmojiUninit : State::textEmojiUninit);
					}
					[[fallthrough]];
				case State::reinit:
					if (isEmoji) {
						pLast = type;
						return State::imageEmojiReinit;
					}

					/* emoji_keycap_sequence */
					else if (type == Type::keyCapStart)
						return State::keyCapSeqFirst;

					/* emoji_modifier_sequence */
					else if (type == Type::modBase)
						return State::modSeqStart;

					/* emoji_flag_sequence */
					else if (type == Type::regInd)
						return State::flagSeqStart;
					break;
				case State::imageEmojiUninit:
				case State::textEmojiUninit:
					/* text_presentation_sequence */
					if (type == Type::textPres)
						return State::textPres;
					[[fallthrough]];
				case State::imageEmojiReinit:
					/* emoji_presentation_sequence or emoji_keycap_sequence */
					if (type == Type::emojiPres) {
						if (pLast == Type::keyCapStart)
							return State::keyCapSeqSecond;
						return State::emojiPres;
					}

					/* emoji_zwj_sequence */
					else if (type == Type::zwj)
						return State::reinit;

					/* emoji_tag_sequence */
					else if (type == Type::tagSpec)
						return State::tagSeqStart;

					/* check if the emoji-character was the start of another chain */
					else if (pLast == Type::keyCapStart)
						return fUpdate(State::keyCapSeqFirst, type, false, false);
					else if (pLast == Type::modBase)
						return fUpdate(State::modSeqStart, type, false, false);
					else if (pLast == Type::regInd)
						return fUpdate(State::flagSeqStart, type, false, false);
					break;
				case State::modSeq:
				case State::emojiPres:
					/* emoji_tag_sequence */
					if (type == Type::tagSpec)
						return State::tagSeqStart;
					[[fallthrough]];
				case State::flagSeq:
				case State::textKeyCapSeq:
				case State::imageKeyCapSeq:
				case State::tagSeq:
					/* emoji_zwj_sequence */
					if (type == Type::zwj)
						return State::reinit;
					break;
				case State::textPres:
					break;
				case State::tagSeqStart:
					/* emoji_tag_sequence */
					if (type == Type::tagEnd)
						return State::tagSeq;
					else if (type == Type::tagSpec)
						return State::tagSeqStart;
					break;
				case State::keyCapSeqFirst:
					/* emoji_keycap_sequence */
					if (type == Type::emojiPres)
						return State::keyCapSeqSecond;
					[[fallthrough]];
				case State::keyCapSeqSecond:
					/* emoji_keycap_sequence */
					if (type == Type::keyCapEnd)
						return (state == State::keyCapSeqFirst ? State::textKeyCapSeq : State::imageKeyCapSeq);
					break;
				case State::modSeqStart:
					/* emoji_modifier_sequence */
					if (type == Type::mod)
						return State::modSeq;
					break;
				case State::flagSeqStart:
					/* emoji_flag_sequence */
					if (type == Type::regInd)
						return State::flagSeq;
					break;
				}
				return State::burnt;
			}

		public:
			constexpr void next(char32_t cp) {
				auto prop = detail::gen::GetProperty(cp);
				Type type = static_cast<Type>((prop >> detail::gen::PropertyEmojiOff) & detail::gen::PropertyEmojiMask);

				/* update the current state based on the incoming character */
				pState = fUpdate(pState, type, (prop & detail::gen::PropertyIsEmoji), (prop & detail::gen::PropertyIsPresentation));
			}
			constexpr bool done() const {
				/* check if its a valid emoji-sequence */
				if (pEmoji) {
					if (pState == State::emojiPres || pState == State::imageEmojiUninit || pState == State::imageEmojiReinit)
						return true;
					if (pState == State::imageKeyCapSeq || pState == State::modSeq || pState == State::flagSeq || pState == State::tagSeq)
						return true;
				}

				/* check if its a valid text-emoji-sequence */
				if (pText) {
					if (pState == State::textKeyCapSeq || pState == State::textEmojiUninit || pState == State::textPres)
						return true;
				}
				return false;
			}
		};
	}

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
			return ((prop >> detail::gen::PropertyAssignedOff) & detail::gen::PropertyAssignedMask) != 0;
		}

		/* check if the codepoint is a alpha character [Unicode Alphabetic] */
		inline constexpr bool IsAlpha(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			return ((prop >> detail::gen::PropertyAlphabeticOff) & detail::gen::PropertyAlphabeticMask) != 0;
		}

		/* check if the codepoint is a numeric character [Unicode Numeric_Type=Numeric/Decimal/Digit] */
		inline constexpr bool IsNumeric(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			return ((prop >> detail::gen::PropertyNumericOff) & detail::gen::PropertyNumericMask) != 0;
		}

		/* check if the codepoint is a alpha/numeric character [Unicode Numeric_Type=Numeric/Decimal/Digit or Unicode Alphabetic] */
		inline constexpr bool IsAlNum(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			if (((prop >> detail::gen::PropertyAlphabeticOff) & detail::gen::PropertyAlphabeticMask) != 0)
				return true;
			return ((prop >> detail::gen::PropertyNumericOff) & detail::gen::PropertyNumericMask) != 0;
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
			detail::gen::PrintableType val = static_cast<detail::gen::PrintableType>((prop >> detail::gen::PropertyPrintableOff) & detail::gen::PropertyPrintableMask);
			if (val == detail::gen::PrintableType::none)
				return false;
			if (val == detail::gen::PrintableType::printable)
				return true;
			return (!reducedSpace || cp == U' ');
		}

		/* check if the codepoint is graphical [Unicode General_Category L*,M*,N*,P*,S* without Zs] */
		inline constexpr bool IsGraphic(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::PrintableType val = static_cast<detail::gen::PrintableType>((prop >> detail::gen::PropertyPrintableOff) & detail::gen::PropertyPrintableMask);
			return (val == detail::gen::PrintableType::printable);
		}

		/* analyze the casing of the codepoint [Unicode property Lowercase,Uppercase or General_Category Lt] */
		enum class CaseType : uint8_t {
			none = detail::gen::CaseType::none,
			lowerCase = detail::gen::CaseType::lowerCase,
			upperCase = detail::gen::CaseType::upperCase,
			titleCase = detail::gen::CaseType::titleCase
		};
		inline constexpr prop::CaseType GetCase(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseMask);
			return static_cast<prop::CaseType>(val);
		}

		/* check if the codepoint has different cases [Unicode property Lowercase, Uppercase or General_Category Lt] */
		inline constexpr bool IsCased(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseMask);
			return (val != detail::gen::CaseType::none);
		}

		/* check if the codepoint is uppercase [Unicode property Uppercase] */
		inline constexpr bool IsUpper(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseMask);
			return (val == detail::gen::CaseType::upperCase);
		}

		/* check if the codepoint is lowercase [Unicode property Lowercase] */
		inline constexpr bool IsLower(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseMask);
			return (val == detail::gen::CaseType::lowerCase);
		}

		/* check if the codepoint is titlecase [Unicode General_Category Lt] */
		inline constexpr bool IsTitle(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CaseType val = static_cast<detail::gen::CaseType>((prop >> detail::gen::PropertyCaseOff) & detail::gen::PropertyCaseMask);
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
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return static_cast<prop::GCType>(val);
		}

		/* check if the codepoint belongs to the letter category [Unicode General_Category L*] */
		inline constexpr bool IsGCLetter(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return (val == detail::gen::CategoryType::lu
				|| val == detail::gen::CategoryType::ll
				|| val == detail::gen::CategoryType::lt
				|| val == detail::gen::CategoryType::lm
				|| val == detail::gen::CategoryType::lo);
		}

		/* check if the codepoint belongs to the mark category [Unicode General_Category M*] */
		inline constexpr bool IsGCMark(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return (val == detail::gen::CategoryType::mn
				|| val == detail::gen::CategoryType::mc
				|| val == detail::gen::CategoryType::me);
		}

		/* check if the codepoint belongs to the number category [Unicode General_Category N*] */
		inline constexpr bool IsGCNumber(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return (val == detail::gen::CategoryType::nd
				|| val == detail::gen::CategoryType::nl
				|| val == detail::gen::CategoryType::no);
		}

		/* check if the codepoint belongs to the punctuation category [Unicode General_Category P*] */
		inline constexpr bool IsGCPunctuation(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
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
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return (val == detail::gen::CategoryType::sm
				|| val == detail::gen::CategoryType::sc
				|| val == detail::gen::CategoryType::sk
				|| val == detail::gen::CategoryType::so);
		}

		/* check if the codepoint belongs to the separator category [Unicode General_Category Z*] */
		inline constexpr bool IsGCSeparator(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return (val == detail::gen::CategoryType::zs
				|| val == detail::gen::CategoryType::zl
				|| val == detail::gen::CategoryType::zp);
		}

		/* check if the codepoint belongs to the other category [Unicode General_Category C*] */
		inline constexpr bool IsGCOther(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::CategoryType val = static_cast<detail::gen::CategoryType>((prop >> detail::gen::PropertyCategoryOff) & detail::gen::PropertyCategoryMask);
			return (val == detail::gen::CategoryType::cc
				|| val == detail::gen::CategoryType::cf
				|| val == detail::gen::CategoryType::cs
				|| val == detail::gen::CategoryType::co
				|| val == detail::gen::CategoryType::cn);
		}

		/* analyze the east-asian width of the codepoint [Unicode property East_Asian_Width] */
		enum class EAWidthType : uint8_t {
			neutral = detail::gen::EAWidthType::neutral,
			fullWidth = detail::gen::EAWidthType::fullWidth,
			halfWidth = detail::gen::EAWidthType::halfWidth,
			wide = detail::gen::EAWidthType::wide,
			narrow = detail::gen::EAWidthType::narrow,
			ambiguous = detail::gen::EAWidthType::ambiguous
		};
		inline constexpr prop::EAWidthType GetEAWidth(char32_t cp) {
			auto prop = detail::gen::GetProperty(cp);
			detail::gen::EAWidthType val = static_cast<detail::gen::EAWidthType>((prop >> detail::gen::PropertyEAWidthOff) & detail::gen::PropertyEAWidthMask);
			return static_cast<prop::EAWidthType>(val);
		}
	}

	/* [cp::IsTester<bool>] check if the entire stream of codepoints defines a valid emoji */
	class TestEmoji : public detail::EmojiAnalysis {
	public:
		constexpr TestEmoji(bool emoji = true, bool text = false) : detail::EmojiAnalysis(emoji, text) {}
	};
}
