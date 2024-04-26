#pragma once

#include "str-common.h"
#include "str-codepoint.h"

#include "generated/unicode-cp-maps.h"
#include "generated/unicode-cp-analysis.h"

#include <algorithm>
#include <string>
#include <concepts>
#include <vector>
#include <utility>

namespace cp {
	/* inclusive range */
	struct Range {
	public:
		size_t first = 0;
		size_t last = 0;

	public:
		constexpr Range() = default;
		explicit constexpr Range(size_t v) : first(v), last(v) {}
		explicit constexpr Range(size_t f, size_t l) : first(f), last(l) {}
	};

	/* Valid sink must receive zero or more valid codepoints and a final cp::EndOfTokens, which must reset the state, after which,
	*	a new independent codepoints-stream may be started (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type, class ValType>
	concept IsSink = requires(Type t, ValType v) {
		{ t(v) } -> std::same_as<void>;
	};

	namespace detail {
		template<class SnkType>
		struct WordBreak {
		private:
			using Type = detail::gen::WordType;
			static_assert(size_t(Type::_last) == 19, "Only types 0-18 are known by the state-machine");

		private:
			enum class State : uint8_t {
				uninit,
				none,
				wb6,
				wb7a,
				wb7b,
				wb11
			};
			enum class Cont : uint8_t {
				uncertain,
				fullCombine,
				firstBreak,
				firstCombine
			};
			enum class Break : uint8_t {
				separate,
				combine,
				uncertain
			};

		private:
			Range pCurrent;
			Range pCached;
			SnkType pSink;
			size_t pRICount = 0;
			Type pLast = Type::other;
			Type pLastActual = Type::other;
			State pState = State::uninit;

		public:
			constexpr WordBreak(SnkType&& sink) : pSink{ sink } {}

		private:
			constexpr Cont fCheckState(Type right) const {
				/* check if this is a silent reduction */
				if (right == Type::extend || right == Type::format || right == Type::zwj)
					return Cont::uncertain;

				/* cleanup the state */
				switch (pState) {
				case State::wb6:
					if (right == Type::a_letter || right == Type::hebrew_letter)
						return Cont::fullCombine;
					return Cont::firstBreak;
				case State::wb7a:
					if (right == Type::a_letter || right == Type::hebrew_letter)
						return Cont::fullCombine;
					return Cont::firstCombine;
				case State::wb7b:
					if (right == Type::hebrew_letter)
						return Cont::fullCombine;
					return Cont::firstBreak;
				case State::wb11:
					if (right == Type::numeric)
						return Cont::fullCombine;
					return Cont::firstBreak;
				}
				return Cont::firstCombine;
			}
			constexpr Break fCheck(Type l, Type lActual, Type r, bool rPictographic) {
				switch (lActual) {
				case Type::cr:
					/* WB3 */
					if (r == Type::lf)
						return Break::combine;
					[[fallthrough]];
				case Type::newline:
					[[fallthrough]];
				case Type::lf:
					/* WB3a */
					return Break::separate;
				case Type::zwj:
					/* WB3c */
					if (rPictographic)
						return Break::combine;
					break;
				case Type::w_seg_space:
					/* WB3d */
					if (r == Type::w_seg_space)
						return Break::combine;
					break;
				}

				/* WB3b */
				if (r == Type::newline || r == Type::cr || r == Type::lf)
					return Break::separate;

				/* WB4 (leaves ri-counter unchanged) */
				if (r == Type::extend || r == Type::format || r == Type::zwj)
					return Break::combine;

				/* handle the remaining cases based on the left-side type */
				switch (l) {
				case Type::hebrew_letter:
					/* WB7b/WB7c */
					if (r == Type::double_quote) {
						pState = State::wb7b;
						return Break::uncertain;
					}

					/* WB7a */
					if (r == Type::single_quote) {
						pState = State::wb7a;
						return Break::uncertain;
					}
					[[fallthrough]];
				case Type::a_letter:
					/* WB5/WB9 */
					if (r == Type::a_letter || r == Type::hebrew_letter || r == Type::numeric)
						return Break::combine;

					/* WB6/WB7 */
					if (r == Type::mid_letter || r == Type::mid_num_letter || r == Type::single_quote) {
						pState = State::wb6;
						return Break::uncertain;
					}

					/* partial: WB13a */
					if (r == Type::extend_num_let)
						return Break::combine;
					return Break::separate;
				case Type::numeric:
					/* WB8/WB10 */
					if (r == Type::numeric || r == Type::a_letter || r == Type::hebrew_letter)
						return Break::combine;

					/* partial: WB13a */
					if (r == Type::extend_num_let)
						return Break::combine;

					/* WB11/WB12 */
					if (r == Type::mid_num || r == Type::mid_num_letter || r == Type::single_quote) {
						pState = State::wb11;
						return Break::uncertain;
					}
					return Break::separate;
				case Type::katakana:
					/* WB13 */
					if (r == Type::katakana)
						return Break::combine;

					/* partial: WB13a */
					if (r == Type::extend_num_let)
						return Break::combine;
					return Break::separate;
				case Type::extend_num_let:
					/* partial: WB13a */
					if (r == Type::extend_num_let)
						return Break::combine;

					/* WB13b */
					if (r == Type::a_letter || r == Type::hebrew_letter || r == Type::numeric || r == Type::katakana)
						return Break::combine;
					return Break::separate;
				case Type::regional_indicator:
					/* WB15/WB16 */
					if (pRICount > 0 && (pRICount & 0x01) == 0)
						return Break::combine;
					return Break::separate;
				}

				/* WB999 */
				return Break::separate;
			}

		public:
			constexpr void operator()(char32_t cp, size_t index) {
				uint8_t rawRight = detail::gen::LookupWordType(cp);
				Type right = static_cast<Type>(rawRight & ~detail::gen::FlagIsPictographic);

				/* fetch and update the left/last-values (will automatically reset the state on the last cp::EndOfTokens, as it will result in Type::other) */
				Type left = pLast, leftActual = pLastActual;
				if (right != Type::extend && right != Type::format && right != Type::zwj) {
					pRICount = (right == Type::regional_indicator ? pRICount + 1 : 0);
					pLast = right;
				}
				pLastActual = right;

				/* WB2: check if the end has been reached */
				if (cp == cp::EndOfTokens) {
					if (pState == State::uninit)
						return;

					/* handle the last open states and sink the remaining word */
					if (pState == State::wb6 || pState == State::wb7b || pState == State::wb11) {
						pSink(pCurrent);
						pCurrent = pCached;
					}
					else if (pState == State::wb7a)
						pCurrent.last = pCached.last;
					pState = State::uninit;
					pSink(pCurrent);
					return;
				}

				/* WB1: check if this is the initial character */
				if (pState == State::uninit) {
					pCurrent = Range(index);
					pState = State::none;
					return;
				}

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					switch (fCheckState(right)) {
					case Cont::fullCombine:
						pState = State::none;
						pCurrent.last = index;
						return;
					case Cont::uncertain:
						pCached.last = index;
						return;
					case Cont::firstBreak:
						pState = State::none;
						pSink(pCurrent);
						pCurrent = pCached;
						break;
					case Cont::firstCombine:
						pState = State::none;
						pCurrent.last = pCached.last;
						break;
					}
				}

				/* check the current values and update the state */
				switch (fCheck(left, leftActual, right, (rawRight & detail::gen::FlagIsPictographic) != 0)) {
				case Break::combine:
					pCurrent.last = index;
					break;
				case Break::separate:
					pSink(pCurrent);
					pCurrent = Range(index);
					break;
				case Break::uncertain:
					pCached = Range(index);
					break;
				}
			}
		};

		enum class CaseLocale : uint8_t {
			none,
			lt, /* lt; lit */
			tr, /* tr; tur */
			az  /* az; aze */
		};
		inline constexpr detail::CaseLocale ParseCaseLocale(const char8_t* locale) {
			detail::CaseLocale out = detail::CaseLocale::none;

			/* fast way out for empty/english/chinese */
			if (locale == 0 || *locale == u8'e' || *locale == u8'z' || *locale == u8'\0')
				return out;

			/* check for lt/lit */
			if (*locale == u8'l' || *locale == u8'L') {
				out = detail::CaseLocale::lt;
				++locale;

				if (*locale == u8'i' || *locale == u8'I')
					++locale;
				if (*locale != u8't' && *locale != u8'T')
					return detail::CaseLocale::none;
				++locale;
			}

			/* check for tr/tur */
			else if (*locale == u8't' || *locale == u8'T') {
				out = detail::CaseLocale::tr;
				++locale;

				if (*locale == u8'u' || *locale == u8'U')
					++locale;
				if (*locale != u8'r' && *locale != u8'R')
					return detail::CaseLocale::none;
				++locale;
			}

			/* check for az/aze */
			else if (*locale == u8'a' || *locale == u8'A') {
				out = detail::CaseLocale::az;
				++locale;

				if (*locale != u8'z' && *locale != u8'Z')
					return detail::CaseLocale::none;
				++locale;
				if (*locale == u8'e' || *locale == u8'E')
					++locale;
			}
			else
				return detail::CaseLocale::none;

			/* check for a valid separator */
			if (*locale == 0 || *locale == u8'-' || *locale == u8'_')
				return out;
			return detail::CaseLocale::none;
		}

		template<class SnkType, int32_t TypeFlag>
		struct CaseMapper {
		private:
			using Cond = detail::gen::CaseCond;
			static_assert(size_t(Cond::_last) == 11, "Only conditions 0-10 are known by the state-machine");

		private:
			struct Cache {
				char32_t cp = 0;
				const int32_t* data = 0;
				size_t size = 0;
			};

		private:
			std::vector<Cache> pCached;
			SnkType pSink;
			size_t pProcessed = 0;
			struct {
				const int32_t* begin = 0;
				const int32_t* end = 0;
				int32_t flags = 0;
				char32_t cp = 0;
			} pActive;
			struct {
				bool cased = false;
				bool softDotted = false;
				bool charI = false;
			} pBefore;
			struct {
				bool testNotCased = false;
				bool testCombClass = false;
			} pAfter;
			detail::CaseLocale pLocale = detail::CaseLocale::none;

		public:
			constexpr CaseMapper(SnkType&& sink, detail::CaseLocale locale) : pSink{ sink }, pLocale(locale) {}

		private:
			constexpr void fBeforeState(int32_t val) {
				/* update the state-machine for 'final_sigma: Before C' */
				if ((val & detail::gen::FlagIsIgnorable) == 0)
					pBefore.cased = ((val & detail::gen::FlagIsCased) != 0);

				/* update the state-machine for 'after_soft_dotted: Before C' and 'after_i: Before C' */
				if ((val & (detail::gen::FlagCombClass0or230)) != 0) {
					pBefore.softDotted = ((val & detail::gen::FlagIsSoftDotted) != 0);
					pBefore.charI = ((val & detail::gen::FlagIs0049) != 0);
				}
			}
			constexpr int8_t fAfterState(int32_t val) const {
				/* check the 'final_sigma: After C' condition (is inverted) */
				if (pAfter.testNotCased) {
					if ((val & detail::gen::FlagIsIgnorable) != 0)
						return 0;
					return ((val & detail::gen::FlagIsCased) != 0 ? -1 : 1);
				}

				/* check the 'more_above: After C' condition */
				else if (pAfter.testCombClass) {
					if ((val & (detail::gen::FlagCombClass0or230)) == 0)
						return 0;
					return ((val & detail::gen::FlagCombClass230) != 0 ? 1 : -1);
				}

				/* check the 'before_dot: After C' condition (inverted as it is only used as 'not_...') */
				else {
					if ((val & (detail::gen::FlagCombClass0or230)) == 0)
						return 0;
					return ((val & detail::gen::FlagIs0307) != 0 ? -1 : 1);
				}
			}
			constexpr char32_t fUnpackSingle(char32_t cp, int32_t data) {
				if ((data & TypeFlag) == 0) {
					/* check if the stored state should be reset */
					if (cp == cp::EndOfTokens) {
						pBefore = { false, false, false };
						pAfter = { false, false };
					}
					return cp;
				}

				int32_t value = (data & detail::gen::BitsOfPayload);
				if (data & detail::gen::FlagIsNegative)
					value = -value;

				return char32_t(int32_t(cp) + value);
			}
			constexpr int8_t fSetupCondition() {
				pAfter = { false, false };

				/* check if the casing matches */
				if ((*pActive.begin & TypeFlag) == 0)
					return -1;

				/* setup the state for the corresponding condition */
				switch (static_cast<Cond>(*pActive.begin & detail::gen::BitsOfPayload)) {
				case Cond::none:
					return 1;
				case Cond::final_sigma:
					if (pBefore.cased) {
						pAfter.testNotCased = true;
						return 0;
					}
					break;
				case Cond::lt_after_soft_dotted:
					if (pBefore.softDotted && pLocale == detail::CaseLocale::lt)
						return 1;
					break;
				case Cond::lt_more_above:
					if (pLocale == detail::CaseLocale::lt) {
						pAfter.testCombClass = true;
						return 0;
					}
					break;
				case Cond::lt:
					if (pLocale == detail::CaseLocale::lt)
						return 1;
					break;
				case Cond::tr:
					if (pLocale == detail::CaseLocale::tr)
						return 1;
					break;
				case Cond::az:
					if (pLocale == detail::CaseLocale::az)
						return 1;
					break;
				case Cond::tr_after_i:
					if (pBefore.charI && pLocale == detail::CaseLocale::tr)
						return 1;
					break;
				case Cond::az_after_i:
					if (pBefore.charI && pLocale == detail::CaseLocale::az)
						return 1;
					break;
				case Cond::tr_not_before_dot:
					/* default pAfter is not_before_dot */
					if (pLocale == detail::CaseLocale::tr)
						return 0;
					break;
				case Cond::az_not_before_dot:
					/* default pAfter is not_before_dot */
					if (pLocale == detail::CaseLocale::az)
						return 0;
					break;
				}
				return -1;
			}

		public:
			constexpr void operator()(char32_t cp) {
				auto [size, data] = detail::gen::MapCase(cp);

				/* check for the fast-way out of no active condition and a single diff-value (will be entered for cp::EndOfTokens and result in a diff of zero) */
				if (pActive.begin == pActive.end && size == 1) {
					fBeforeState(data[0]);
					pSink(fUnpackSingle(cp, data[0]));
					return;
				}
				pCached.push_back({ cp, data, size });

				/* iterate until an incomplete character has been encountered or the cached entries have been processed */
				size_t index = pCached.size() - 1;
				while (true) {
					int8_t cond = 0;

					/* check if a new block needs to be opened */
					if (pActive.begin == pActive.end) {
						if (pProcessed >= pCached.size())
							return;
						cp = pCached[pProcessed].cp;
						data = pCached[pProcessed].data;
						size = pCached[pProcessed].size;
						if (++pProcessed >= pCached.size()) {
							pCached.clear();
							pProcessed = 0;
						}

						/* check if this is a single block (will also catch cp::EndOfTokens) */
						if (size == 1) {
							fBeforeState(data[0]);
							pSink(fUnpackSingle(cp, data[0]));
							continue;
						}
						pActive = { data, data + size, data[0], cp };
						cond = fSetupCondition();
						index = pProcessed;
					}

					while (true) {
						/* feed the cached entries into the condition and check if its still incomplete or failed */
						for (; cond == 0 && index < pCached.size(); ++index) {
							cond = fAfterState(pCached[index].data[0]);
							if (cond == 0 && pCached[index].cp == cp::EndOfTokens)
								cond = -1;
						}
						if (cond == 0)
							return;

						/* in case of the condition */
						if (cond > 0)
							break;
						pActive.begin += 2 + pActive.begin[1];
						if (pActive.begin == pActive.end)
							break;
						cond = fSetupCondition();
						index = pProcessed;
					}

					/* write the characters to the output and update the before-state */
					if (pActive.begin != pActive.end) {
						pActive.begin += 2;
						pActive.end = pActive.begin + pActive.begin[-1];
						while (pActive.begin != pActive.end)
							pSink(pActive.cp + *(pActive.begin++));
					}
					else
						pSink(pActive.cp);
					fBeforeState(pActive.flags);
				}
			}
		};
	}

	/* create a sink, which splits the stream into ranges of words and writes them to the sink
	*	Sink(char32_t, size_t): code-point and index used to reference it in the output-ranges */
	struct WordBreak {
		template <cp::IsSink<Range> SnkType>
		constexpr detail::WordBreak<SnkType> operator()(SnkType&& sink) {
			return detail::WordBreak<SnkType>{ std::forward<SnkType>(sink) };
		}
	};

	/* create a sink, which writes the upper-cased stream to the given sink
	*	Sink(char32_t): code-point */
	struct UpperCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr UpperCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink<char32_t> SnkType>
		constexpr detail::CaseMapper<SnkType, detail::gen::FlagIsUpper> operator()(SnkType&& sink) {
			return detail::CaseMapper<SnkType, detail::gen::FlagIsUpper>{ std::forward<SnkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the lower-cased stream to the given sink
	*	Sink(char32_t): code-point */
	struct LowerCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr LowerCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink<char32_t> SnkType>
		constexpr detail::CaseMapper<SnkType, detail::gen::FlagIsLower> operator()(SnkType&& sink) {
			return detail::CaseMapper<SnkType, detail::gen::FlagIsLower>{ std::forward<SnkType>(sink), pLocale };
		}
	};
}
