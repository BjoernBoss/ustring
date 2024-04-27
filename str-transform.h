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
#include <variant>

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

	/* Valid sinks for char32_t must receive zero or more valid codepoints and a final cp::EndOfTokens, after which
	*	the object is considered burnt (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type, class ValType>
	concept IsSink = requires(Type t, ValType v) {
		{ t(v) } -> std::same_as<void>;
	};

	namespace detail {
		template <class Type, size_t Buffer>
		class LocalBuffer {
		private:
			struct Static {
				Type buffer[Buffer]{};
				size_t size = 0;
			};

		private:
			std::variant<Static, std::vector<Type>> pBuffer;
			size_t pProcessed = 0;

		public:
			LocalBuffer() : pBuffer{ Static{} } {}

		public:
			void push(const Type& t) {
				if (std::holds_alternative<std::vector<Type>>(pBuffer)) {
					std::get<std::vector<Type>>(pBuffer).push_back(t);
				}
				else {
					Static& s = std::get<Static>(pBuffer);
					if (s.size < Buffer)
						s.buffer[s.size++] = t;
					else {
						std::vector<Type> v{ s.buffer + pProcessed, s.buffer + s.size };
						v.push_back(t);
						pBuffer = std::move(v);
					}
				}
			}
			Type pop() {
				if (std::holds_alternative<Static>(pBuffer)) {
					Static& s = std::get<Static>(pBuffer);
					Type val = s.buffer[pProcessed++];
					if (pProcessed >= s.size)
						pProcessed = (s.size = 0);
					return val;
				}

				std::vector<Type>& v = std::get<std::vector<Type>>(pBuffer);
				Type val = v[pProcessed++];
				if (pProcessed >= v.size()) {
					v.clear();
					pProcessed = 0;
				}
				return val;
			}
			size_t size() const {
				return (std::holds_alternative<Static>(pBuffer) ? std::get<Static>(pBuffer).size : std::get<std::vector<Type>>(pBuffer).size()) - pProcessed;
			}
			Type& get(size_t i) {
				if (std::holds_alternative<Static>(pBuffer))
					return std::get<Static>(pBuffer).buffer[i - pProcessed];
				return std::get<std::vector<Type>>(pBuffer)[i - pProcessed];
			}
			Type& front() {
				if (std::holds_alternative<Static>(pBuffer))
					return std::get<Static>(pBuffer).buffer[pProcessed];
				return std::get<std::vector<Type>>(pBuffer)[pProcessed];
			}
			Type& back() {
				if (std::holds_alternative<Static>(pBuffer)) {
					Static& s = std::get<Static>(pBuffer);
					return s.buffer[s.size - pProcessed - 1];
				}
				std::vector<Type>& v = std::get<std::vector<Type>>(pBuffer);
				return v[v.size() - pProcessed - 1];
			}
		};

		template <class SelfType>
		class WordBreak {
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
			enum class Continue : uint8_t {
				addCached,
				combineIncludingRight,
				combineExcludingRight,
				breakBeforeCached
			};
			enum class Break : uint8_t {
				separate,
				combine,
				uncertain
			};

		private:
			size_t pRICount = 0;
			Type pLast = Type::other;
			Type pLastActual = Type::other;
			State pState = State::uninit;

		public:
			constexpr WordBreak() = default;

		private:
			constexpr Continue fCheckState(Type right) const {
				/* check if this is a silent reduction */
				if (right == Type::extend || right == Type::format || right == Type::zwj)
					return Continue::addCached;

				/* cleanup the state */
				switch (pState) {
				case State::wb6:
					if (right == Type::a_letter || right == Type::hebrew_letter)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;
				case State::wb7a:
					if (right == Type::a_letter || right == Type::hebrew_letter)
						return Continue::combineIncludingRight;
					return Continue::combineExcludingRight;
				case State::wb7b:
					if (right == Type::hebrew_letter)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;
				case State::wb11:
					if (right == Type::numeric)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;
				}
				return Continue::combineExcludingRight;
			}
			constexpr Break fCheck(Type l, Type lActual, Type r, bool rPictographic) {
				switch (lActual) {
				case Type::cr:
					/* WB3 */
					if (r == Type::lf)
						return Break::combine;
					[[fallthrough]];
				case Type::newline:
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
			template <class PayloadType>
			constexpr void operator()(char32_t cp, const PayloadType& payload) {
				uint8_t rawRight = detail::gen::LookupWordType(cp);
				Type right = static_cast<Type>(rawRight & ~detail::gen::FlagIsPictographic);

				/* update the regional_indicator counter and update the last-state */
				Type left = pLast, leftActual = pLastActual;
				if (right != Type::extend && right != Type::format && right != Type::zwj) {
					pRICount = (right == Type::regional_indicator ? pRICount + 1 : 0);
					pLast = right;
				}
				pLastActual = right;

				/* WB2: check if the end has been reached (before checking WB1 to prevent initialization on empty stream) */
				if (cp == cp::EndOfTokens) {
					if (pState == State::uninit)
						return;

					/* handle the last open states and sink the remaining word */
					if (pState == State::wb6 || pState == State::wb7b || pState == State::wb11)
						static_cast<SelfType*>(this)->fEndUnknown(true);
					else if (pState == State::wb7a)
						static_cast<SelfType*>(this)->fEndUnknown(false);
					static_cast<SelfType*>(this)->fDone();
					return;
				}

				/* WB1: check if this is the initial character */
				if (pState == State::uninit) {
					pState = State::none;
					static_cast<SelfType*>(this)->fBegin(payload);
					return;
				}

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					switch (fCheckState(right)) {
					case Continue::combineIncludingRight:
						pState = State::none;
						static_cast<SelfType*>(this)->fEndUnknown(false);
						static_cast<SelfType*>(this)->fNext(payload, false);
						return;
					case Continue::combineExcludingRight:
						pState = State::none;
						static_cast<SelfType*>(this)->fEndUnknown(false);
						break;
					case Continue::addCached:
						static_cast<SelfType*>(this)->fAddUnknown(payload);
						return;
					case Continue::breakBeforeCached:
						pState = State::none;
						static_cast<SelfType*>(this)->fEndUnknown(true);
						break;
					}
				}

				/* check the current values and update the state */
				switch (fCheck(left, leftActual, right, (rawRight & detail::gen::FlagIsPictographic) != 0)) {
				case Break::combine:
					static_cast<SelfType*>(this)->fNext(payload, false);
					break;
				case Break::separate:
					static_cast<SelfType*>(this)->fNext(payload, true);
					break;
				case Break::uncertain:
					static_cast<SelfType*>(this)->fBeginUnknown(payload);
					break;
				}
			}
		};

		template <class SnkType>
		class WordBreakRange final : private detail::WordBreak<detail::WordBreakRange<SnkType>> {
			friend class detail::WordBreak<detail::WordBreakRange<SnkType>>;
		private:
			Range pCurrent;
			Range pCached;
			SnkType pSink;

		public:
			constexpr WordBreakRange(SnkType&& sink) : pSink{ sink } {}

		private:
			constexpr void fBeginUnknown(size_t index) {
				pCached = Range(index);
			}
			constexpr void fEndUnknown(bool separated) {
				if (separated) {
					pSink(pCurrent);
					pCurrent.first = pCached.first;
				}
				pCurrent.last = pCached.last;
			}
			constexpr void fAddUnknown(size_t index) {
				pCached.last = index;
			}
			constexpr void fNext(size_t index, bool separated) {
				if (separated) {
					pSink(pCurrent);
					pCurrent.first = index;
				}
				pCurrent.last = index;
			}
			constexpr void fBegin(size_t index) {
				pCurrent = Range(index);
			}
			constexpr void fDone() {
				pSink(pCurrent);
			}

		public:
			constexpr void operator()(char32_t cp, size_t index) {
				detail::WordBreak<detail::WordBreakRange<SnkType>>::operator()(cp, index);
			}
		};

		template <class SnkType>
		class WordBreakSeparate final : private detail::WordBreak<detail::WordBreakSeparate<SnkType>> {
			friend class detail::WordBreak<detail::WordBreakSeparate<SnkType>>;
		private:
			size_t pCached = 0;
			SnkType pSink;

		public:
			constexpr WordBreakSeparate(SnkType&& sink) : pSink{ sink } {}

		private:
			constexpr void fBeginUnknown(int) {
				pCached = 0;
			}
			constexpr void fEndUnknown(bool separated) {
				pSink(separated);
				for (size_t i = 0; i < pCached; ++i)
					pSink(false);
			}
			constexpr void fAddUnknown(int) {
				++pCached;
			}
			constexpr void fNext(int, bool separated) {
				pSink(separated);
			}
			constexpr void fBegin(int) {}
			constexpr void fDone() {}

		public:
			constexpr void operator()(char32_t cp) {
				detail::WordBreak<detail::WordBreakSeparate<SnkType>>::operator()(cp, 0);
			}
		};

		template <class SelfType>
		class GraphemeBreak {
		private:
			using Type = detail::gen::GraphemeType;
			static_assert(size_t(Type::_last) == 15, "Only types 0-14 are known by the state-machine");

		private:
			enum class Break : uint8_t {
				separate,
				combine
			};
			enum class GB9cState : uint8_t {
				none,
				linker,
				match
			};
			enum class GB11State : uint8_t {
				none,
				zwj,
				match
			};

		private:
			size_t pRICount = 0;
			GB9cState pGB9cState = GB9cState::none;
			GB11State pGB11State = GB11State::none;
			uint8_t pLast = static_cast<uint8_t>(Type::other);
			bool pInitialized = false;

		public:
			constexpr GraphemeBreak() = default;

		private:
			constexpr Break fCheck(Type l, Type r, bool rIsInCBConsonant) const {
				/* GB3/GB4/GB5 */
				if (l == Type::cr)
					return (r == Type::lf ? Break::combine : Break::separate);
				else if (l == Type::control || l == Type::lf || r == Type::control || r == Type::cr || r == Type::lf)
					return Break::separate;

				/* GB9/GB9a */
				if (r == Type::extend || r == Type::zwj || r == Type::space_marking)
					return Break::combine;

				switch (l) {
				case Type::zwj:
					/* GB11 */
					if (pGB11State == GB11State::match && r == Type::extended_pictographic)
						return Break::combine;
					break;
				case Type::regional_indicator:
					/* GB12/GB13 */
					if ((pRICount & 0x01) != 0 && r == Type::regional_indicator)
						return Break::combine;
					break;
				case Type::prepend:
					/* GB9b */
					return Break::combine;
				case Type::l:
					/* GB6 */
					if (r == Type::l || r == Type::v || r == Type::lv || r == Type::lvt)
						return Break::combine;
					break;
				case Type::v:
				case Type::lv:
					/* GB7 */
					if (r == Type::v || r == Type::t)
						return Break::combine;
					break;
				case Type::t:
				case Type::lvt:
					/* GB8 */
					if (r == Type::t)
						return Break::combine;
					break;
				}

				/* GB9c */
				if (pGB9cState == GB9cState::match && rIsInCBConsonant)
					return Break::combine;

				/* GB999 */
				return Break::separate;
			}
			constexpr GB9cState fUpdateGB9cState(uint8_t l) const {
				if ((l & detail::gen::FlagIsInCBConsonant) != 0)
					return GB9cState::linker;

				if (pGB9cState == GB9cState::linker) {
					if ((l & detail::gen::FlagIsInCBExtend) != 0)
						return GB9cState::linker;
					if ((l & detail::gen::FlagIsInCBLinker) != 0)
						return GB9cState::match;
				}
				else if (pGB9cState == GB9cState::match && (l & (detail::gen::FlagIsInCBExtend | detail::gen::FlagIsInCBLinker)) != 0)
					return GB9cState::match;
				return GB9cState::none;
			}
			constexpr GB11State fUpdateGB11State(Type l) const {
				if (l == Type::extended_pictographic)
					return GB11State::zwj;

				if (pGB11State == GB11State::zwj) {
					if (l == Type::extend)
						return GB11State::zwj;
					if (l == Type::zwj)
						return GB11State::match;
				}
				return GB11State::none;
			}

		public:
			template <class PayloadType>
			constexpr void operator()(char32_t cp, const PayloadType& payload) {
				uint8_t rawLeft = pLast, rawRight = detail::gen::LookupGraphemeType(cp);
				Type left = static_cast<Type>(rawLeft & ~(detail::gen::FlagIsInCBExtend | detail::gen::FlagIsInCBConsonant | detail::gen::FlagIsInCBLinker));
				Type right = static_cast<Type>(rawRight & ~(detail::gen::FlagIsInCBExtend | detail::gen::FlagIsInCBConsonant | detail::gen::FlagIsInCBLinker));
				pLast = rawRight;

				/* GB2: check if the end has been reached (before checking GB1 to prevent initialization on empty stream) */
				if (cp == cp::EndOfTokens) {
					if (!pInitialized)
						return;
					static_cast<SelfType*>(this)->fDone();
					return;
				}

				/* GB1: check if this is the initial character */
				if (!pInitialized) {
					pInitialized = true;
					static_cast<SelfType*>(this)->fBegin(payload);
					return;
				}

				/* update the states */
				pGB9cState = fUpdateGB9cState(rawLeft);
				pGB11State = fUpdateGB11State(left);
				pRICount = (left == Type::regional_indicator ? pRICount + 1 : 0);

				/* check the current values and update the state */
				switch (fCheck(left, right, (rawRight & detail::gen::FlagIsInCBConsonant) != 0)) {
				case Break::combine:
					static_cast<SelfType*>(this)->fNext(payload, false);
					break;
				case Break::separate:
					static_cast<SelfType*>(this)->fNext(payload, true);
					break;
				}
			}
		};

		template <class SnkType>
		class GraphemeBreakRange final : private detail::GraphemeBreak<detail::GraphemeBreakRange<SnkType>> {
			friend class detail::GraphemeBreak<detail::GraphemeBreakRange<SnkType>>;
		private:
			Range pCurrent;
			SnkType pSink;

		public:
			constexpr GraphemeBreakRange(SnkType&& sink) : pSink{ sink } {}

		private:
			constexpr void fNext(size_t index, bool separated) {
				if (separated) {
					pSink(pCurrent);
					pCurrent.first = index;
				}
				pCurrent.last = index;
			}
			constexpr void fBegin(size_t index) {
				pCurrent = Range(index);
			}
			constexpr void fDone() {
				pSink(pCurrent);
			}

		public:
			constexpr void operator()(char32_t cp, size_t index) {
				detail::GraphemeBreak<detail::GraphemeBreakRange<SnkType>>::operator()(cp, index);
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

		template <class SelfType, class SnkType>
		class CaseMapper {
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
			detail::LocalBuffer<Cache, 2> pCached;
			SnkType pSink;
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
				if ((data & static_cast<SelfType*>(this)->fTypeFlag()) == 0)
					return cp;

				int32_t value = (data & detail::gen::BitsOfPayload);
				if (data & detail::gen::FlagIsNegative)
					value = -value;

				return char32_t(int32_t(cp) + value);
			}
			constexpr int8_t fSetupCondition() {
				pAfter = { false, false };

				/* check if the casing matches */
				if ((*pActive.begin & static_cast<SelfType*>(this)->fTypeFlag()) == 0)
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
				if (pActive.begin == pActive.end && size == 1 && !static_cast<SelfType*>(this)->fIncomplete()) {
					fBeforeState(data[0]);
					pSink(fUnpackSingle(cp, data[0]));
					static_cast<SelfType*>(this)->fDone();
					return;
				}
				pCached.push({ cp, data, size });

				/* iterate until an incomplete character has been encountered or the cached entries have been processed */
				size_t index = pCached.size() - 1;
				while (true) {
					int8_t cond = 0;

					/* check if a new block needs to be opened */
					if (pActive.begin == pActive.end) {
						if (pCached.size() == 0 || static_cast<SelfType*>(this)->fIncomplete())
							return;
						Cache _c = pCached.pop();
						cp = _c.cp;
						data = _c.data;
						size = _c.size;

						/* check if this is a single block (will also catch cp::EndOfTokens) */
						if (size == 1) {
							fBeforeState(data[0]);
							pSink(fUnpackSingle(cp, data[0]));
							static_cast<SelfType*>(this)->fDone();
							continue;
						}
						pActive = { data, data + size, data[0], cp };
						cond = fSetupCondition();
						index = 0;
					}

					while (true) {
						/* feed the cached entries into the condition and check if its still incomplete or failed */
						for (; cond == 0 && index < pCached.size(); ++index) {
							cond = fAfterState(pCached.get(index).data[0]);
							if (cond == 0 && pCached.get(index).cp == cp::EndOfTokens)
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
						index = 0;
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
					static_cast<SelfType*>(this)->fDone();
					fBeforeState(pActive.flags);
				}
			}
		};

		template <class SnkType>
		class UpperMapper final : private detail::CaseMapper<detail::UpperMapper<SnkType>, SnkType> {
			friend class detail::CaseMapper<detail::UpperMapper<SnkType>, SnkType>;
		private:
			using Super = detail::CaseMapper<detail::UpperMapper<SnkType>, SnkType>;

		public:
			using Super::Super;

		private:
			constexpr int32_t fTypeFlag() {
				return detail::gen::FlagIsUpper;
			}
			constexpr bool fIncomplete() {
				return false;
			}
			constexpr void fDone() {}

		public:
			constexpr void operator()(char32_t cp) {
				Super::operator()(cp);
			}
		};

		template <class SnkType>
		class LowerMapper final : private detail::CaseMapper<detail::LowerMapper<SnkType>, SnkType> {
			friend class detail::CaseMapper<detail::LowerMapper<SnkType>, SnkType>;
		private:
			using Super = detail::CaseMapper<detail::LowerMapper<SnkType>, SnkType>;

		public:
			using Super::Super;

		private:
			constexpr int32_t fTypeFlag() {
				return detail::gen::FlagIsLower;
			}
			constexpr bool fIncomplete() {
				return false;
			}
			constexpr void fDone() {}

		public:
			constexpr void operator()(char32_t cp) {
				Super::operator()(cp);
			}
		};

		template <class SnkType>
		class TitleMapper final : private detail::CaseMapper<detail::TitleMapper<SnkType>, SnkType> {
			friend class detail::CaseMapper<detail::TitleMapper<SnkType>, SnkType>;
		private:
			using Super = detail::CaseMapper<detail::TitleMapper<SnkType>, SnkType>;
			struct Lambda {
				TitleMapper<SnkType>& self;
				constexpr Lambda(TitleMapper<SnkType>& s) : self{ s } {}
				constexpr void operator()(bool separate) {
					self.fSeparate(separate);
				}
			};

		private:
			detail::LocalBuffer<size_t, 2> pWords;
			detail::WordBreakSeparate<Lambda> pSeparator;
			bool pLower = false;

		public:
			constexpr TitleMapper(SnkType&& sink, detail::CaseLocale locale) : Super{ std::forward<SnkType>(sink), locale }, pSeparator{ Lambda{ *this } } {
				pWords.push(1);
			}

		private:
			constexpr int32_t fTypeFlag() {
				return (pLower ? detail::gen::FlagIsLower : detail::gen::FlagIsTitle);
			}
			constexpr bool fIncomplete() {
				return (pWords.size() == 0);
			}
			constexpr void fDone() {
				pLower = true;
				if (--pWords.front() == 0) {
					pWords.pop();
					pLower = false;
				}
			}
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

		public:
			constexpr void operator()(char32_t cp) {
				pSeparator(cp);
				if (cp == cp::EndOfTokens)
					fSeparate(false);
				Super::operator()(cp);
			}
		};
	}

	/* create a sink, which splits the stream into ranges of words and writes them to the sink
	*	Sink(char32_t, size_t): code-point and index used to reference it in the output-ranges */
	struct WordBreak {
		template <cp::IsSink<Range> SnkType>
		constexpr detail::WordBreakRange<SnkType> operator()(SnkType&& sink) {
			return detail::WordBreakRange<SnkType>{ std::forward<SnkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of grapheme-clusters and writes them to the sink
	*	Sink(char32_t, size_t): code-point and index used to reference it in the output-ranges */
	struct GraphemeBreak {
		template <cp::IsSink<Range> SnkType>
		constexpr detail::GraphemeBreakRange<SnkType> operator()(SnkType&& sink) {
			return detail::GraphemeBreakRange<SnkType>{ std::forward<SnkType>(sink) };
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
		constexpr detail::UpperMapper<SnkType> operator()(SnkType&& sink) {
			return detail::UpperMapper<SnkType>{ std::forward<SnkType>(sink), pLocale };
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
		constexpr detail::LowerMapper<SnkType> operator()(SnkType&& sink) {
			return detail::LowerMapper<SnkType>{ std::forward<SnkType>(sink), pLocale };
		}
	};

	/* create a sink, which writes the title-cased stream to the given sink
	*	Sink(char32_t): code-point */
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
