#pragma once

#include "str-common.h"
#include "str-codepoint.h"
#include "str-segmentation.h"

#include "generated/unicode-mapping.h"

#include <utility>

namespace cp {
	namespace detail {
		template <class SelfType>
		class TitleCasing {
		private:
			struct Lambda {
				TitleCasing& self;
				constexpr Lambda(TitleCasing& s) : self{ s } {}
				constexpr void operator()(size_t, cp::BreakMode mode) {
					self.fSeparate(mode != cp::BreakMode::none);
				}
			};

		private:
			detail::LocalBuffer<size_t, 2> pWords;
			detail::LocalBuffer<char32_t, 2> pChars;
			cp::WordBreak::Type<Lambda> pSeparator;
			bool pLower = false;

		public:
			constexpr TitleCasing() : pSeparator{ cp::WordBreak{}(Lambda{ *this }) } {
				pWords.push(1);
			}

		private:
			constexpr void fSeparate(bool separate) {
				if (pWords.size() == 0) {
					pLower = !separate;
					pWords.push(1);
				}
				else if (separate)
					pWords.push(1);
				else
					++pWords.back();
			}

		protected:
			constexpr void processed() {
				pLower = true;
				if (--pWords.front() == 0) {
					pWords.pop();
					pLower = false;
				}
			}
			constexpr void done() {
				/* finalize the separator and flush the last cached characters (word-counter can be ignored) */
				pSeparator.done();
				while (pChars.size() > 0)
					static_cast<SelfType*>(this)->fNext(pChars.pop());
			}
			constexpr bool lower() const {
				return pLower;
			}
			constexpr void next(char32_t cp) {
				pSeparator.next(cp, 0);

				/* fast-path of no characters being cached and characters being immediately processable */
				if (pChars.size() == 0) {
					static_cast<SelfType*>(this)->fNext(cp);
					return;
				}

				/* flush as many of the cached characters as possible */
				while (pWords.size() > 0 && pChars.size() > 0)
					static_cast<SelfType*>(this)->fNext(pChars.pop());

				/* process the current most-recent codepoint */
				if (pWords.size() > 0)
					static_cast<SelfType*>(this)->fNext(cp);
				else
					pChars.push(cp);
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

		template <class SinkType, class SelfType>
		class CaseMapper {
		private:
			using Cond = detail::gen::CaseCond;
			static_assert(size_t(Cond::_last) == 12, "Only conditions 0-11 are known by the state-machine");

		private:
			enum class Condition : uint8_t {
				incomplete,
				match,
				failed
			};
			struct Cache {
				const uint32_t* data = 0;
				size_t size = 0;
				char32_t cp = 0;
			};

		private:
			detail::LocalBuffer<Cache, 2> pCached;
			SinkType pSink;
			struct {
				const uint32_t* begin = 0;
				const uint32_t* end = 0;
				char32_t cp = 0;
				uint32_t first = 0;
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
			constexpr CaseMapper(SinkType&& sink, detail::CaseLocale locale) : pSink{ sink }, pLocale(locale) {}

		private:
			constexpr Condition fAfterState(uint32_t val) const {
				/* check the 'finalSigma: After C' condition (is inverted) */
				if (pAfter.testNotCased) {
					if ((val & detail::gen::CaseIsIgnorable) != 0)
						return Condition::incomplete;
					return ((val & detail::gen::CaseIsCased) != 0 ? Condition::failed : Condition::match);
				}

				/* check the 'moreAbove: After C' condition */
				else if (pAfter.testCombClass) {
					if ((val & (detail::gen::CaseIsCombClass0or230)) == 0)
						return Condition::incomplete;
					return ((val & detail::gen::CaseIsCombClass230) != 0 ? Condition::match : Condition::failed);
				}

				/* check the 'beforeDot: After C' condition (inverted as it is only used as 'not_...') */
				else {
					if ((val & (detail::gen::CaseIsCombClass0or230)) == 0)
						return Condition::incomplete;
					return ((val & detail::gen::CaseIs0307) != 0 ? Condition::failed : Condition::match);
				}
			}
			constexpr Condition fSetupCondition() {
				pAfter = { false, false };

				/* check if the casing matches */
				if ((*pActive.begin & static_cast<SelfType*>(this)->fTypeFlag()) == 0)
					return Condition::failed;

				/* setup the state for the corresponding condition */
				switch (static_cast<Cond>(*pActive.begin & detail::gen::CaseValueMask)) {
				case Cond::none:
					return Condition::match;
				case Cond::finalSigma:
					if (pBefore.cased) {
						pAfter.testNotCased = true;
						return Condition::incomplete;
					}
					break;
				case Cond::ltAfterSoftDotted:
					if (pBefore.softDotted && pLocale == detail::CaseLocale::lt)
						return Condition::match;
					break;
				case Cond::ltMoreAbove:
					if (pLocale == detail::CaseLocale::lt) {
						pAfter.testCombClass = true;
						return Condition::incomplete;
					}
					break;
				case Cond::trOrAz:
					if (pLocale == detail::CaseLocale::tr || pLocale == detail::CaseLocale::az)
						return Condition::match;
					break;
				case Cond::lt:
					if (pLocale == detail::CaseLocale::lt)
						return Condition::match;
					break;
				case Cond::tr:
					if (pLocale == detail::CaseLocale::tr)
						return Condition::match;
					break;
				case Cond::az:
					if (pLocale == detail::CaseLocale::az)
						return Condition::match;
					break;
				case Cond::trAfterI:
					if (pBefore.charI && pLocale == detail::CaseLocale::tr)
						return Condition::match;
					break;
				case Cond::azAfterI:
					if (pBefore.charI && pLocale == detail::CaseLocale::az)
						return Condition::match;
					break;
				case Cond::trNotBeforeDot:
					/* default this->pAfter is notBeforeDot */
					if (pLocale == detail::CaseLocale::tr)
						return Condition::incomplete;
					break;
				case Cond::azNotBeforeDot:
					/* default this->pAfter is notBeforeDot */
					if (pLocale == detail::CaseLocale::az)
						return Condition::incomplete;
					break;
				}
				return Condition::failed;
			}
			constexpr void fCompleteChar(uint32_t val) {
				/* update the state-machine for 'finalSigma: Before C' */
				if ((val & detail::gen::CaseIsIgnorable) == 0)
					pBefore.cased = ((val & detail::gen::CaseIsCased) != 0);

				/* update the state-machine for 'afterSoftDotted: Before C' and 'afterI: Before C' */
				if ((val & (detail::gen::CaseIsCombClass0or230)) != 0) {
					pBefore.softDotted = ((val & detail::gen::CaseIsSoftDotted) != 0);
					pBefore.charI = ((val & detail::gen::CaseIs0049) != 0);
				}

				/* notify the self-type of the completed character */
				static_cast<SelfType*>(this)->fDone();
			}
			constexpr void fProcessSingle(char32_t cp, uint32_t data) {
				/* extract the proper codepoint to be used */
				if ((data & static_cast<SelfType*>(this)->fTypeFlag()) != 0) {
					uint32_t value = (data & detail::gen::CaseValueMask);
					if (data & detail::gen::CaseIsNegative)
						cp = char32_t(uint32_t(cp) - value);
					else
						cp = char32_t(uint32_t(cp) + value);
				}

				/* write the codepoint to the sink and update the before state */
				pSink(cp);
				fCompleteChar(data);
			}
			template <bool IsCleanup>
			constexpr void fProcessQueue(Condition cond) {
				while (true) {
					/* check if the condition is incomplete and either return, as nothing can be done
					*	anymore, or mark it as failed, as no more characters will be encountered */
					if (cond == Condition::incomplete) {
						if constexpr (!IsCleanup)
							return;
						cond = Condition::failed;
					}

					/* check if the condition has not been satisfied and advance the current state
					*	and write the result out on success or if the last condition has failed */
					if (cond == Condition::match) {
						pActive.begin += 2;
						pActive.end = pActive.begin + pActive.begin[-1];
						while (pActive.begin != pActive.end) {
							uint32_t value = (*pActive.begin & detail::gen::CaseValueMask);
							if (*(pActive.begin++) & detail::gen::CaseIsNegative)
								pSink(pActive.cp - value);
							else
								pSink(pActive.cp + value);
						}
						fCompleteChar(pActive.first);
					}
					else if ((pActive.begin += 2 + pActive.begin[1]) == pActive.end) {
						pSink(pActive.cp);
						fCompleteChar(pActive.first);
					}

					/* check if another condition exists and set it up and otherwise process the next characters in the cache */
					if (pActive.begin < pActive.end)
						cond = fSetupCondition();
					else while (true) {
						if (pCached.size() == 0)
							return;
						Cache _c = pCached.pop();

						/* check if the next value is a single value and process it and otherwise setup the next condition */
						if (_c.size == 1) {
							fProcessSingle(_c.cp, _c.data[0]);
							continue;
						}
						pActive = { _c.data, _c.data + _c.size, _c.cp, _c.data[0] };
						cond = fSetupCondition();
						break;
					}

					/* feed all remaining cached entries into the condition and check if it can be satisfied */
					for (size_t i = 1; cond == Condition::incomplete && i < pCached.size(); ++i)
						cond = fAfterState(pCached.get(i).data[0]);
				}
			}

		public:
			constexpr void next(char32_t cp) {
				auto [size, data] = detail::gen::MapCase(cp);

				/* check for the fast-way out of no active condition and a single diff-value */
				if (pActive.begin == pActive.end && size == 1) {
					fProcessSingle(cp, data[0]);
					return;
				}
				Condition cond = Condition::incomplete;

				/* write the value to the cache and process the remaining queue */
				if (pActive.begin == pActive.end) {
					pActive = { data, data + size, cp, data[0] };
					cond = fSetupCondition();
				}
				else {
					pCached.push({ data, size, cp });
					cond = fAfterState(data[0]);
				}
				fProcessQueue<false>(cond);
			}
			constexpr void done() {
				/* process the remaining queue and fail any incomplete conditions */
				if (pActive.begin != pActive.end)
					fProcessQueue<true>(Condition::failed);
			}
		};

		template <class SinkType>
		class UpperMapper final : public detail::CaseMapper<SinkType, detail::UpperMapper<SinkType>> {
			friend class detail::CaseMapper<SinkType, detail::UpperMapper<SinkType>>;
		private:
			using Super = detail::CaseMapper<SinkType, detail::UpperMapper<SinkType>>;

		public:
			using Super::Super;

		private:
			constexpr uint32_t fTypeFlag() {
				return detail::gen::CaseIsUpper;
			}
			constexpr void fDone() {}
		};

		template <class SinkType>
		class LowerMapper final : public detail::CaseMapper<SinkType, detail::LowerMapper<SinkType>> {
			friend class detail::CaseMapper<SinkType, detail::LowerMapper<SinkType>>;
		private:
			using Super = detail::CaseMapper<SinkType, detail::LowerMapper<SinkType>>;

		public:
			using Super::Super;

		private:
			constexpr uint32_t fTypeFlag() {
				return detail::gen::CaseIsLower;
			}
			constexpr void fDone() {}
		};

		template <class SinkType>
		class TitleMapper final : private detail::TitleCasing<detail::TitleMapper<SinkType>>, private detail::CaseMapper<SinkType, detail::TitleMapper<SinkType>> {
			friend class detail::TitleCasing<detail::TitleMapper<SinkType>>;
			friend class detail::CaseMapper<SinkType, detail::TitleMapper<SinkType>>;
		private:
			using Super0 = detail::TitleCasing<detail::TitleMapper<SinkType>>;
			using Super1 = detail::CaseMapper<SinkType, detail::TitleMapper<SinkType>>;

		public:
			constexpr TitleMapper(SinkType&& sink, detail::CaseLocale locale) : Super1{ std::forward<SinkType>(sink), locale } {}

		private:
			constexpr uint32_t fTypeFlag() {
				return (Super0::lower() ? detail::gen::CaseIsLower : detail::gen::CaseIsTitle);
			}
			constexpr void fDone() {
				Super0::processed();
			}
			constexpr void fNext(char32_t cp) {
				Super1::next(cp);
			}

		public:
			constexpr void next(char32_t cp) {
				Super0::next(cp);
			}
			constexpr void done() {
				Super0::done();
				Super1::done();
			}
		};

		template <class SinkType>
		class FoldingMapper final : public detail::CaseMapper<SinkType, detail::FoldingMapper<SinkType>> {
			friend class detail::CaseMapper<SinkType, detail::FoldingMapper<SinkType>>;
		private:
			using Super = detail::CaseMapper<SinkType, detail::FoldingMapper<SinkType>>;

		public:
			using Super::Super;

		private:
			constexpr uint32_t fTypeFlag() {
				return detail::gen::CaseIsFold;
			}
			constexpr void fDone() {}
		};

		template <class SelfType>
		class TestCasing {
		private:
			bool pMatches = true;
			bool pChars = false;

		public:
			constexpr TestCasing() = default;

		public:
			constexpr void next(char32_t cp) {
				auto [size, data] = detail::gen::MapCase(cp);
				pChars = true;

				/* check the fast path of no conditions existing if the character would change */
				if (size == 1) {
					if (data[0] & static_cast<SelfType*>(this)->fTypeFlag())
						pMatches = false;
					return;
				}

				/* iterate over the conditions and look for any null-conditions */
				for (size_t i = 0; i < size; i += 2 + size_t(data[i + 1])) {
					if (static_cast<detail::gen::CaseCond>(data[i] & detail::gen::CaseValueMask) != detail::gen::CaseCond::none)
						continue;
					if ((data[i] & static_cast<SelfType*>(this)->fTypeFlag()) == 0)
						continue;
					pMatches = false;
					break;
				}
			}
			constexpr bool done() {
				return (pChars && pMatches);
			}
		};

		template <size_t TypeFlag>
		class TestConstCasing : public detail::TestCasing<TestConstCasing<TypeFlag>> {
			friend class detail::TestCasing<TestConstCasing<TypeFlag>>;
		public:
			constexpr TestConstCasing() = default;

		private:
			constexpr uint32_t fTypeFlag() const {
				return TypeFlag;
			}
		};

		class TestTitleCasing : private detail::TitleCasing<detail::TestTitleCasing>, private detail::TestCasing<detail::TestTitleCasing> {
			friend class detail::TitleCasing<detail::TestTitleCasing>;
			friend class detail::TestCasing<detail::TestTitleCasing>;
		private:
			using Super0 = detail::TitleCasing<detail::TestTitleCasing>;
			using Super1 = detail::TestCasing<detail::TestTitleCasing>;

		public:
			constexpr TestTitleCasing() = default;

		private:
			constexpr uint32_t fTypeFlag() const {
				return (Super0::lower() ? detail::gen::CaseIsLower : detail::gen::CaseIsTitle);
			}
			constexpr void fNext(char32_t cp) {
				Super1::next(cp);
				Super0::processed();
			}

		public:
			constexpr void next(char32_t cp) {
				Super0::next(cp);
			}
			constexpr bool done() {
				Super0::done();
				return Super1::done();
			}
		};
	}

	/* create a sink, which writes the upper-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): source codepoint
	*	OutSink(char32_t): upper-cased codepoint(s) */
	class UpperCase {
	public:
		template <cp::IsSink<char32_t> SinkType>
		using Type = detail::UpperMapper<SinkType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr UpperCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <cp::IsSink<char32_t> SinkType>
		constexpr Type<SinkType> operator()(SinkType&& sink) {
			return Type<SinkType>{ std::forward<SinkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the lower-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): source codepoint
	*	OutSink(char32_t): lower-cased codepoint(s) */
	class LowerCase {
	public:
		template <cp::IsSink<char32_t> SinkType>
		using Type = detail::LowerMapper<SinkType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr LowerCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <cp::IsSink<char32_t> SinkType>
		constexpr Type<SinkType> operator()(SinkType&& sink) {
			return Type<SinkType>{ std::forward<SinkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the title-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): source codepoint
	*	OutSink(char32_t): title-cased codepoint(s) */
	class TitleCase {
	public:
		template <cp::IsSink<char32_t> SinkType>
		using Type = detail::TitleMapper<SinkType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr TitleCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <cp::IsSink<char32_t> SinkType>
		constexpr Type<SinkType> operator()(SinkType&& sink) {
			return Type<SinkType>{ std::forward<SinkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the case-folded stream to the given sink (will be produced in-order)
	*	InSink(char32_t): source codepoint
	*	OutSink(char32_t): case-folded codepoint(s) */
	class FoldCase {
	public:
		template <cp::IsSink<char32_t> SinkType>
		using Type = detail::FoldingMapper<SinkType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr FoldCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <cp::IsSink<char32_t> SinkType>
		constexpr Type<SinkType> operator()(SinkType&& sink) {
			return Type<SinkType>{ std::forward<SinkType>(sink), pLocale };
		}
	};

	/* check if the entire stream of codepoints is not empty and upper-cased (i.e. UpperCase(...) would result in the same codepoints; for simplification
	*	purposes locale and codepoint context will not be considered, meaning for very few codepoints in certain locale/context, they might change afterall) */
	class TestUpperCase : public detail::TestConstCasing<detail::gen::CaseIsUpper> {
	public:
		constexpr TestUpperCase() = default;
	};

	/* check if the entire stream of codepoints is not empty and lower-cased (i.e. LowerCase(...) would result in the same codepoints; for simplification
	*	purposes locale and codepoint context will not be considered, meaning for very few codepoints in certain locale/context, they might change afterall) */
	class TestLowerCase : public detail::TestConstCasing<detail::gen::CaseIsLower> {
	public:
		constexpr TestLowerCase() = default;
	};

	/* check if the entire stream of codepoints is not empty and title-cased (i.e. TitleCase(...) would result in the same codepoints; for simplification
	*	purposes locale and codepoint context will not be considered, meaning for very few codepoints in certain locale/context, they might change afterall) */
	class TestTitleCase : public detail::TestTitleCasing {
	public:
		constexpr TestTitleCase() = default;
	};

	/* check if the entire stream of codepoints is not empty and case-folded (i.e. FoldCase(...) would result in the same codepoints; for simplification
	*	purposes locale and codepoint context will not be considered, meaning for very few codepoints in certain locale/context, they might change afterall) */
	class TestFoldCase : public detail::TestConstCasing<detail::gen::CaseIsFold> {
	public:
		constexpr TestFoldCase() = default;
	};
}