#pragma once

#include "str-common.h"
#include "str-codepoint.h"

#include "generated/unicode-cp-maps.h"

#include <algorithm>
#include <string>
#include <limits>
#include <concepts>
#include <vector>

namespace cp {
	/* type must behave valid if receiving valid characters and one last cp::EndOfTokens (otherwise undefined behavior allowed) */
	template <class Type>
	concept IsSink = requires(Type t, char32_t c) {
		{ t(c) } -> std::same_as<void>;
	};

	/* construct a sink, which applies its transformation to the input and passes it to the type-unspecified received sink */
	template <class Type>
	concept IsTransform = requires(Type t) {
		{ t([](char32_t) {}) } -> cp::IsSink;
	};

	namespace detail {
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
			static_assert(size_t(detail::gen::CaseCond::_last) == 11, "Only conditions 0-10 are known by state-machine");
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
			CaseMapper(SnkType&& sink, detail::CaseLocale locale) : pSink(sink), pLocale(locale) {}

		private:
			void fBeforeState(int32_t val) {
				/* update the state-machine for 'final_sigma: Before C' */
				if ((val & detail::gen::FlagIsIgnorable) == 0)
					pBefore.cased = ((val & detail::gen::FlagIsCased) != 0);

				/* update the state-machine for 'after_soft_dotted: Before C' and 'after_i: Before C' */
				if ((val & (detail::gen::FlagCombClass0or230)) != 0) {
					pBefore.softDotted = ((val & detail::gen::FlagIsSoftDotted) != 0);
					pBefore.charI = ((val & detail::gen::FlagIs0049) != 0);
				}
			}
			int8_t fAfterState(int32_t val) const {
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
			char32_t fUnpackSingle(char32_t cp, int32_t data) const {
				if ((data & TypeFlag) == 0)
					return cp;

				int32_t value = (data & detail::gen::BitsOfPayload);
				if (data & detail::gen::FlagIsNegative)
					value = -value;

				return char32_t(int32_t(cp) + value);
			}
			int8_t fSetupCondition() {
				pAfter = { false, false };

				/* check if the casing matches */
				if ((*pActive.begin & TypeFlag) == 0)
					return -1;

				/* setup the state for the corresponding condition */
				switch (static_cast<detail::gen::CaseCond>(*pActive.begin & detail::gen::BitsOfPayload)) {
				case detail::gen::CaseCond::none:
					return 1;
				case detail::gen::CaseCond::final_sigma:
					if (pBefore.cased) {
						pAfter.testNotCased = true;
						return 0;
					}
					break;
				case detail::gen::CaseCond::lt_after_soft_dotted:
					if (pBefore.softDotted && pLocale == detail::CaseLocale::lt)
						return 1;
					break;
				case detail::gen::CaseCond::lt_more_above:
					if (pLocale == detail::CaseLocale::lt) {
						pAfter.testCombClass = true;
						return 0;
					}
					break;
				case detail::gen::CaseCond::lt:
					if (pLocale == detail::CaseLocale::lt)
						return 1;
					break;
				case detail::gen::CaseCond::tr:
					if (pLocale == detail::CaseLocale::tr)
						return 1;
					break;
				case detail::gen::CaseCond::az:
					if (pLocale == detail::CaseLocale::az)
						return 1;
					break;
				case detail::gen::CaseCond::tr_after_i:
					if (pBefore.charI && pLocale == detail::CaseLocale::tr)
						return 1;
					break;
				case detail::gen::CaseCond::az_after_i:
					if (pBefore.charI && pLocale == detail::CaseLocale::az)
						return 1;
					break;
				case detail::gen::CaseCond::tr_not_before_dot:
					/* default pAfter is not_before_dot */
					if (pLocale == detail::CaseLocale::tr)
						return 0;
					break;
				case detail::gen::CaseCond::az_not_before_dot:
					/* default pAfter is not_before_dot */
					if (pLocale == detail::CaseLocale::az)
						return 0;
					break;
				}
				return -1;
			}

		public:
			void operator()(char32_t cp) {
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

	/* implements cp::IsTransform */
	struct UpperCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		UpperCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink SnkType>
		detail::CaseMapper<SnkType, detail::gen::FlagIsUpper> operator()(SnkType&& sink) {
			return detail::CaseMapper<SnkType, detail::gen::FlagIsUpper>{ std::forward<SnkType>(sink), pLocale };
		}
	};
	struct LowerCase {
	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		LowerCase(const char8_t* locale = 0) {
			pLocale = detail::ParseCaseLocale(locale);
		}
		template <cp::IsSink SnkType>
		detail::CaseMapper<SnkType, detail::gen::FlagIsLower> operator()(SnkType&& sink) {
			return detail::CaseMapper<SnkType, detail::gen::FlagIsLower>{ std::forward<SnkType>(sink), pLocale };
		}
	};
}
