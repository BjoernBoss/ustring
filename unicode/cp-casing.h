/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "../str-common.h"
#include "cp-segmentation.h"

#include "../generated/unicode-casing.h"

#include <utility>

namespace cp {
	namespace detail {
		enum class CaseLocale : uint8_t {
			none,
			lt, /* lt; lit */
			tr, /* tr; tur */
			az  /* az; aze */
		};
		inline constexpr detail::CaseLocale ParseCaseLocale(std::wstring_view localeView) {
			detail::CaseLocale out = detail::CaseLocale::none;
			if (localeView.empty())
				return out;
			const wchar_t* locale = localeView.data();

			/* fast way out for empty/english/chinese */
			if (*locale == L'e' || *locale == L'z')
				return out;

			/* check for lt/lit */
			if (*locale == L'l' || *locale == L'L') {
				out = detail::CaseLocale::lt;
				++locale;

				if (*locale == L'i' || *locale == L'I')
					++locale;
				if (*locale != L't' && *locale != L'T')
					return detail::CaseLocale::none;
				++locale;
			}

			/* check for tr/tur */
			else if (*locale == L't' || *locale == L'T') {
				out = detail::CaseLocale::tr;
				++locale;

				if (*locale == L'u' || *locale == L'U')
					++locale;
				if (*locale != L'r' && *locale != L'R')
					return detail::CaseLocale::none;
				++locale;
			}

			/* check for az/aze */
			else if (*locale == L'a' || *locale == L'A') {
				out = detail::CaseLocale::az;
				++locale;

				if (*locale != L'z' && *locale != L'Z')
					return detail::CaseLocale::none;
				++locale;
				if (*locale == L'e' || *locale == L'E')
					++locale;
			}
			else
				return detail::CaseLocale::none;

			/* check for a valid separator */
			if (*locale == 0 || *locale == L'-' || *locale == L'_')
				return out;
			return detail::CaseLocale::none;
		}

		template <class CollType, class SelfType>
		class CaseMapper {
		private:
			using Cond = detail::gen::CaseCond;
			static_assert(size_t(Cond::_end) == 12, "Only conditions 0-11 are known by the state-machine");

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
			str::detail::LocalBuffer<Cache> pCached;
			CollType pCollector;
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
			constexpr CaseMapper(CollType&& collector, detail::CaseLocale locale) : pCollector{ collector }, pLocale(locale) {}

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

				case Cond::_end:
					/* to silence warning */
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

				/* write the codepoint to the collector and update the before state */
				pCollector.next(cp);
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
								pCollector.next(pActive.cp - value);
							else
								pCollector.next(pActive.cp + value);
						}
						fCompleteChar(pActive.first);
					}
					else if ((pActive.begin += 2 + pActive.begin[1]) == pActive.end) {
						pCollector.next(pActive.cp);
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
				pCollector.done();
			}
		};

		template <class CollType>
		class UpperMapper final : public detail::CaseMapper<CollType, detail::UpperMapper<CollType>> {
			friend class detail::CaseMapper<CollType, detail::UpperMapper<CollType>>;
		private:
			using Super = detail::CaseMapper<CollType, detail::UpperMapper<CollType>>;

		public:
			using Super::Super;

		private:
			constexpr uint32_t fTypeFlag() {
				return detail::gen::CaseIsUpper;
			}
			constexpr void fDone() {}
		};

		template <class CollType>
		class LowerMapper final : public detail::CaseMapper<CollType, detail::LowerMapper<CollType>> {
			friend class detail::CaseMapper<CollType, detail::LowerMapper<CollType>>;
		private:
			using Super = detail::CaseMapper<CollType, detail::LowerMapper<CollType>>;

		public:
			using Super::Super;

		private:
			constexpr uint32_t fTypeFlag() {
				return detail::gen::CaseIsLower;
			}
			constexpr void fDone() {}
		};

		template <class CollType>
		class TitleMapper final : private detail::CaseMapper<CollType, detail::TitleMapper<CollType>> {
			friend class detail::CaseMapper<CollType, detail::TitleMapper<CollType>>;
		private:
			using Super = detail::CaseMapper<CollType, detail::TitleMapper<CollType>>;
			struct Lambda {
				detail::TitleMapper<CollType>& self;
				constexpr Lambda(detail::TitleMapper<CollType>& s) : self{ s } {}
				constexpr void operator()(size_t, cp::BreakMode mode) {
					self.fSeparate(mode != cp::BreakMode::none);
				}
			};

		private:
			str::detail::LocalBuffer<size_t> pWords;
			str::detail::LocalBuffer<char32_t> pChars;
			cp::WordBreak::Type<Lambda> pSeparator;
			bool pLower = false;

		public:
			constexpr TitleMapper(CollType&& collector, detail::CaseLocale locale) : Super{ std::forward<CollType>(collector), locale }, pSeparator{ cp::WordBreak{}(Lambda{ *this }) } {
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
			constexpr uint32_t fTypeFlag() {
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
				pSeparator.next(cp, 0);

				/* fast-path of no characters being cached and characters being immediately processable */
				if (pChars.size() == 0) {
					Super::next(cp);
					return;
				}

				/* flush as many of the cached characters as possible */
				while (pWords.size() > 0 && pChars.size() > 0)
					Super::next(pChars.pop());

				/* process the current most-recent codepoint */
				if (pWords.size() > 0)
					Super::next(cp);
				else
					pChars.push(cp);
			}
			constexpr void done() {
				/* finalize the separator and flush the last cached characters (word-counter can be ignored, as
				*	flushing the separator will already have matched the word-counter to the actual codepoints) */
				pSeparator.done();
				while (pChars.size() > 0)
					Super::next(pChars.pop());
				Super::done();
			}
		};

		template <class CollType>
		class FoldingMapper final : public detail::CaseMapper<CollType, detail::FoldingMapper<CollType>> {
			friend class detail::CaseMapper<CollType, detail::FoldingMapper<CollType>>;
		private:
			using Super = detail::CaseMapper<CollType, detail::FoldingMapper<CollType>>;

		public:
			using Super::Super;

		private:
			constexpr uint32_t fTypeFlag() {
				return detail::gen::CaseIsFold;
			}
			constexpr void fDone() {}
		};

		template <template<class> class MapType>
		class TestCasing {
		private:
			struct Lambda {
				detail::TestCasing<MapType>& self;
				constexpr Lambda(detail::TestCasing<MapType>& s) : self{ s } {}
				constexpr void next(char32_t cp) {
					self.fNext(cp);
				}
				constexpr void done() {}
			};

		private:
			MapType<Lambda> pMapper;
			str::detail::LocalBuffer<char32_t> pChars;
			bool pMatches = true;

		public:
			constexpr TestCasing(std::wstring_view locale = {}) : pMapper{ Lambda{ *this }, detail::ParseCaseLocale(locale) } {}

		private:
			constexpr void fNext(char32_t cp) {
				if (pChars.size() == 0 || pChars.pop() != cp)
					pMatches = false;
			}

		public:
			constexpr void next(char32_t cp) {
				if (pMatches) {
					pChars.push(cp);
					pMapper.next(cp);
				}
			}
			constexpr bool done() {
				if (pMatches)
					pMapper.done();
				return pMatches;
			}
		};
	}

	/* [str::IsMapper] create a collector, which writes the upper-cased stream to the given collector */
	class UpperCase {
	public:
		template <str::IsCollector CollType>
		using Type = detail::UpperMapper<CollType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr UpperCase(std::wstring_view locale = {}) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector), pLocale };
		}
	};

	/* [str::IsMapper] create a collector, which writes the lower-cased stream to the given collector */
	class LowerCase {
	public:
		template <str::IsCollector CollType>
		using Type = detail::LowerMapper<CollType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr LowerCase(std::wstring_view locale = {}) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector), pLocale };
		}
	};

	/* [str::IsMapper] create a collector, which writes the title-cased stream to the given collector */
	class TitleCase {
	public:
		template <str::IsCollector CollType>
		using Type = detail::TitleMapper<CollType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr TitleCase(std::wstring_view locale = {}) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector), pLocale };
		}
	};

	/* [str::IsMapper] create a collector, which writes the case-folded stream to the given collector */
	class FoldCase {
	public:
		template <str::IsCollector CollType>
		using Type = detail::FoldingMapper<CollType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr FoldCase(std::wstring_view locale = {}) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector), pLocale };
		}
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is upper-cased (i.e. cp::UpperCase(...) would result in the same codepoints) */
	class TestUpperCase : public detail::TestCasing<detail::UpperMapper> {
	public:
		constexpr TestUpperCase(std::wstring_view locale = {}) : detail::TestCasing<detail::UpperMapper>(locale) {}
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is lower-cased (i.e. cp::LowerCase(...) would result in the same codepoints) */
	class TestLowerCase : public detail::TestCasing<detail::LowerMapper> {
	public:
		constexpr TestLowerCase(std::wstring_view locale = {}) : detail::TestCasing<detail::LowerMapper>(locale) {}
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is title-cased (i.e. cp::TitleCase(...) would result in the same codepoints) */
	class TestTitleCase : public detail::TestCasing<detail::TitleMapper> {
	public:
		constexpr TestTitleCase(std::wstring_view locale = {}) : detail::TestCasing<detail::TitleMapper>(locale) {}
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is case-folded (i.e. cp::FoldCase(...) would result in the same codepoints) */
	class TestFoldCase : public detail::TestCasing<detail::FoldingMapper> {
	public:
		constexpr TestFoldCase(std::wstring_view locale = {}) : detail::TestCasing<detail::FoldingMapper>(locale) {}
	};
}
