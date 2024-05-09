#pragma once

#include "str-common.h"
#include "str-codepoint.h"
#include "str-segmentation.h"

#include "generated/unicode-mapping.h"

#include <algorithm>
#include <string>
#include <concepts>
#include <vector>
#include <utility>
#include <variant>
#include <type_traits>

namespace cp {
	namespace detail {
		template <class SinkType>
		class WordBreakSeparate {
		private:
			struct Lambda {
				WordBreakSeparate<SinkType>& self;
				constexpr Lambda(WordBreakSeparate<SinkType>& s) : self{ s } {}
				constexpr void operator()(uint8_t, cp::BreakMode mode) {
					self.pSink(mode != cp::BreakMode::none);
				}
			};

		private:
			detail::WordForward<Lambda, uint8_t> pBreaker;
			SinkType pSink;
			bool pInitialized = false;

		public:
			constexpr WordBreakSeparate(SinkType&& sink) : pBreaker{ Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp) {
				if (pInitialized)
					pBreaker.next(cp, 0);
				else {
					pInitialized = true;
					pBreaker.first(cp);
				}
			}
			constexpr void done() {
				if (pInitialized)
					pBreaker.done();
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
			static_assert(size_t(Cond::_last) == 11, "Only conditions 0-10 are known by the state-machine");

		private:
			enum class Condition : uint8_t {
				incomplete,
				match,
				failed
			};
			struct Cache {
				const int32_t* data = 0;
				size_t size = 0;
				char32_t cp = 0;
			};

		private:
			detail::LocalBuffer<Cache, 2> pCached;
			SinkType pSink;
			struct {
				const int32_t* begin = 0;
				const int32_t* end = 0;
				char32_t cp = 0;
				int32_t first = 0;
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
			constexpr Condition fAfterState(int32_t val) const {
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
				switch (static_cast<Cond>(*pActive.begin & detail::gen::CaseBitsOfPayload)) {
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
			constexpr void fCompleteChar(int32_t val) {
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
			constexpr void fProcessSingle(char32_t cp, int32_t data) {
				/* extract the proper codepoint to be used */
				if ((data & static_cast<SelfType*>(this)->fTypeFlag()) != 0) {
					int32_t value = (data & detail::gen::CaseBitsOfPayload);
					if (data & detail::gen::CaseIsNegative)
						value = -value;
					cp = char32_t(int32_t(cp) + value);
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
						while (pActive.begin != pActive.end)
							pSink(pActive.cp + *(pActive.begin++));
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
		class UpperMapper final : private detail::CaseMapper<SinkType, detail::UpperMapper<SinkType>> {
			friend class detail::CaseMapper<SinkType, detail::UpperMapper<SinkType>>;
		private:
			using Super = detail::CaseMapper<SinkType, detail::UpperMapper<SinkType>>;

		public:
			using Super::Super;

		private:
			constexpr int32_t fTypeFlag() {
				return detail::gen::CaseIsUpper;
			}
			constexpr void fDone() {}

		public:
			constexpr void next(char32_t cp) {
				Super::next(cp);
			}
			constexpr void done() {
				Super::done();
			}
		};

		template <class SinkType>
		class LowerMapper final : private detail::CaseMapper<SinkType, detail::LowerMapper<SinkType>> {
			friend class detail::CaseMapper<SinkType, detail::LowerMapper<SinkType>>;
		private:
			using Super = detail::CaseMapper<SinkType, detail::LowerMapper<SinkType>>;

		public:
			using Super::Super;

		private:
			constexpr int32_t fTypeFlag() {
				return detail::gen::CaseIsLower;
			}
			constexpr void fDone() {}

		public:
			constexpr void next(char32_t cp) {
				Super::next(cp);
			}
			constexpr void done() {
				Super::done();
			}
		};

		template <class SinkType>
		class TitleMapper final : private detail::CaseMapper<SinkType, detail::TitleMapper<SinkType>> {
			friend class detail::CaseMapper<SinkType, detail::TitleMapper<SinkType>>;
		private:
			using Super = detail::CaseMapper<SinkType, detail::TitleMapper<SinkType>>;
			struct Lambda {
				TitleMapper<SinkType>& self;
				constexpr Lambda(TitleMapper<SinkType>& s) : self{ s } {}
				constexpr void operator()(bool separate) {
					self.fSeparate(separate);
				}
			};

		private:
			detail::LocalBuffer<size_t, 2> pWords;
			detail::LocalBuffer<char32_t, 2> pChars;
			detail::WordBreakSeparate<Lambda> pSeparator;
			bool pLower = false;

		public:
			constexpr TitleMapper(SinkType&& sink, detail::CaseLocale locale) : Super{ std::forward<SinkType>(sink), locale }, pSeparator{ Lambda{ *this } } {
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
			constexpr int32_t fTypeFlag() {
				return (pLower ? detail::gen::CaseIsLower : detail::gen::CaseIsTitle);
			}
			constexpr void fDone() {
				pLower = true;
				if (--pWords.front() == 0) {
					pWords.pop();
					pLower = false;
				}
			}

		public:
			constexpr void next(char32_t cp) {
				pSeparator.next(cp);

				/* flush the characters to the mapper */
				if (pChars.size() == 0) {
					Super::next(cp);
					return;
				}
				while (pWords.size() > 0 && pChars.size() > 0)
					Super::next(pChars.pop());
				if (pWords.size() > 0)
					Super::next(cp);
				else
					pChars.push(cp);
			}
			constexpr void done() {
				pSeparator.done();

				while (pChars.size() > 0)
					Super::next(pChars.pop());
				Super::done();
			}
		};
	}


	class GraphemeNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::GraphemeIterator{}(it);
			while (gIt.next() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class GraphemePrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::GraphemeIterator{}(it);
			while (gIt.prev() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class WordNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::WordIterator{}(it);
			while (gIt.next() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class WordPrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::WordIterator{}(it);
			while (gIt.prev() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class SentenceNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::SentenceIterator{}(it);
			while (gIt.next() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class SentencePrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::SentenceIterator{}(it);
			while (gIt.prev() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class LineNext {
	private:
		bool pEmergency = false;
		bool pGraphemes = false;

	public:
		constexpr LineNext(bool emergencyBreak = true, bool graphemeAware = true) : pEmergency{ emergencyBreak }, pGraphemes{ graphemeAware } {}

	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::LineIterator{ pEmergency, pGraphemes }(it);
			while (gIt.next() == cp::BreakMode::none);
			return gIt.get();
		}
	};
	class LinePrev {
	private:
		bool pEmergency = false;
		bool pGraphemes = false;

	public:
		constexpr LinePrev(bool emergencyBreak = true, bool graphemeAware = true) : pEmergency{ emergencyBreak }, pGraphemes{ graphemeAware } {}

	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			auto gIt = cp::LineIterator{ pEmergency, pGraphemes }(it);
			while (gIt.prev() == cp::BreakMode::none);
			return gIt.get();
		}
	};



	/* create a sink, which writes the upper-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): codepoint
	*	OutSink(char32_t): codepoint */
	struct UpperCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr UpperCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink<char32_t> SnkType>
		constexpr detail::UpperMapper<SnkType> operator()(SnkType&& sink) {
			return detail::UpperMapper<SnkType>{ std::forward<SnkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the lower-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): codepoint
	*	OutSink(char32_t): codepoint */
	struct LowerCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr LowerCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink<char32_t> SnkType>
		constexpr detail::LowerMapper<SnkType> operator()(SnkType&& sink) {
			return detail::LowerMapper<SnkType>{ std::forward<SnkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the title-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): codepoint
	*	OutSink(char32_t): codepoint */
	struct TitleCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr TitleCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink<char32_t> SnkType>
		constexpr detail::TitleMapper<SnkType> operator()(SnkType&& sink) {
			return detail::TitleMapper<SnkType>{ std::forward<SnkType>(sink), pLocale };
		}
	};
}
