#pragma once

#include "str-common.h"
#include "str-codepoint.h"

#include "generated/unicode-mapping.h"
#include "generated/unicode-segmentation.h"

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
	struct LineRange {
	public:
		cp::Range range;
		bool breakAfter = false;

	public:
		constexpr LineRange() = default;
		explicit constexpr LineRange(const cp::Range& r) : range(r), breakAfter(false) {}
		explicit constexpr LineRange(const cp::Range& r, bool brkAfter) : range(r), breakAfter(brkAfter) {}
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
				/* right will be 'other' for the final iteration and automatically clean it up properly */

				/* check if this is a silent reduction */
				if (right == Type::extend || right == Type::format || right == Type::zwj)
					return Continue::addCached;

				/* cleanup the state */
				switch (pState) {
				case State::wb6:
					if (right == Type::aLetter || right == Type::hebrewLetter)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;
				case State::wb7a:
					if (right == Type::aLetter || right == Type::hebrewLetter)
						return Continue::combineIncludingRight;
					return Continue::combineExcludingRight;
				case State::wb7b:
					if (right == Type::hebrewLetter)
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
				case Type::wSegSpace:
					/* WB3d */
					if (r == Type::wSegSpace)
						return Break::combine;
					break;
				}

				/* WB3b */
				if (r == Type::newline || r == Type::cr || r == Type::lf)
					return Break::separate;

				/* WB4 */
				if (r == Type::extend || r == Type::format || r == Type::zwj)
					return Break::combine;

				/* handle the remaining cases based on the left-side type */
				switch (l) {
				case Type::hebrewLetter:
					/* WB7b/WB7c */
					if (r == Type::doubleQuote) {
						pState = State::wb7b;
						return Break::uncertain;
					}

					/* WB7a */
					if (r == Type::singleQuote) {
						pState = State::wb7a;
						return Break::uncertain;
					}
					[[fallthrough]];
				case Type::aLetter:
					/* WB5/WB9 */
					if (r == Type::aLetter || r == Type::hebrewLetter || r == Type::numeric)
						return Break::combine;

					/* WB6/WB7 */
					if (r == Type::midLetter || r == Type::midNumLetter || r == Type::singleQuote) {
						pState = State::wb6;
						return Break::uncertain;
					}

					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;
					return Break::separate;
				case Type::numeric:
					/* WB8/WB10 */
					if (r == Type::numeric || r == Type::aLetter || r == Type::hebrewLetter)
						return Break::combine;

					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;

					/* WB11/WB12 */
					if (r == Type::midNum || r == Type::midNumLetter || r == Type::singleQuote) {
						pState = State::wb11;
						return Break::uncertain;
					}
					return Break::separate;
				case Type::katakana:
					/* WB13 */
					if (r == Type::katakana)
						return Break::combine;

					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;
					return Break::separate;
				case Type::extendNumLet:
					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;

					/* WB13b */
					if (r == Type::aLetter || r == Type::hebrewLetter || r == Type::numeric || r == Type::katakana)
						return Break::combine;
					return Break::separate;
				case Type::regionalIndicator:
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
				uint8_t rawRight = ((detail::gen::GetSegmentation(cp) >> detail::gen::WordSegmentationOff) & detail::gen::SegmentationMask);
				Type right = static_cast<Type>(rawRight & ~detail::gen::WordIsPictographic);

				/* update the regionalIndicator counter and update the last-state */
				Type left = pLast, leftActual = pLastActual;
				if (right != Type::extend && right != Type::format && right != Type::zwj) {
					pRICount = (right == Type::regionalIndicator ? pRICount + 1 : 0);
					pLast = right;
				}
				pLastActual = right;

				/* WB1: check if this is the initial character (only if its not an empty string) */
				if (pState == State::uninit) {
					if (cp == cp::EndOfTokens)
						return;
					pState = State::none;
					static_cast<SelfType*>(this)->fBegin(payload);
					return;
				}

				/* WB2: check if the end has been reached */
				if (cp == cp::EndOfTokens) {
					if (pState != State::none)
						static_cast<SelfType*>(this)->fEndUnknown(fCheckState(Type::other) == Continue::breakBeforeCached);
					static_cast<SelfType*>(this)->fDone();
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
					case Continue::addCached:
						static_cast<SelfType*>(this)->fAddUnknown(payload);
						return;
					case Continue::combineExcludingRight:
						pState = State::none;
						static_cast<SelfType*>(this)->fEndUnknown(false);
						break;
					case Continue::breakBeforeCached:
						pState = State::none;
						static_cast<SelfType*>(this)->fEndUnknown(true);
						break;
					}
				}

				/* check the current values and update the state */
				switch (fCheck(left, leftActual, right, (rawRight & detail::gen::WordIsPictographic) != 0)) {
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
				if (r == Type::extend || r == Type::zwj || r == Type::spaceMarking)
					return Break::combine;

				switch (l) {
				case Type::zwj:
					/* GB11 */
					if (pGB11State == GB11State::match && r == Type::extendedPictographic)
						return Break::combine;
					break;
				case Type::regionalIndicator:
					/* GB12/GB13 */
					if ((pRICount & 0x01) != 0 && r == Type::regionalIndicator)
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
				if ((l & detail::gen::GraphemeIsInCBConsonant) != 0)
					return GB9cState::linker;

				if (pGB9cState == GB9cState::linker) {
					if ((l & detail::gen::GraphemeIsInCBExtend) != 0)
						return GB9cState::linker;
					if ((l & detail::gen::GraphemeIsInCBLinker) != 0)
						return GB9cState::match;
				}
				else if (pGB9cState == GB9cState::match && (l & (detail::gen::GraphemeIsInCBExtend | detail::gen::GraphemeIsInCBLinker)) != 0)
					return GB9cState::match;
				return GB9cState::none;
			}
			constexpr GB11State fUpdateGB11State(Type l) const {
				if (l == Type::extendedPictographic)
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
				uint8_t rawLeft = pLast;
				uint8_t rawRight = ((detail::gen::GetSegmentation(cp) >> detail::gen::GraphemeSegmentationOff) & detail::gen::SegmentationMask);
				Type left = static_cast<Type>(rawLeft & ~(detail::gen::GraphemeIsInCBExtend | detail::gen::GraphemeIsInCBConsonant | detail::gen::GraphemeIsInCBLinker));
				Type right = static_cast<Type>(rawRight & ~(detail::gen::GraphemeIsInCBExtend | detail::gen::GraphemeIsInCBConsonant | detail::gen::GraphemeIsInCBLinker));
				pLast = rawRight;

				/* GB1: check if this is the initial character (only if its not an empty string) */
				if (!pInitialized) {
					if (cp == cp::EndOfTokens)
						return;
					pInitialized = true;
					static_cast<SelfType*>(this)->fBegin(payload);
					return;
				}

				/* GB2: check if the end has been reached */
				if (cp == cp::EndOfTokens) {
					static_cast<SelfType*>(this)->fDone();
					return;
				}

				/* update the states */
				pGB9cState = fUpdateGB9cState(rawLeft);
				pGB11State = fUpdateGB11State(left);
				pRICount = (left == Type::regionalIndicator ? pRICount + 1 : 0);

				/* check the current values and update the state */
				switch (fCheck(left, right, (rawRight & detail::gen::GraphemeIsInCBConsonant) != 0)) {
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

		template <class SelfType, class PayloadType>
		class SentenceBreak {
		private:
			using Type = detail::gen::SentenceType;
			static_assert(size_t(Type::_last) == 15, "Only types 0-14 are known by the state-machine");

		private:
			enum class Chain : uint8_t {
				none,
				aClose,
				sClose,
				aSpace,
				sSpace,
				paraSep
			};
			enum class SB7State : uint8_t {
				none,
				aTerm,
				match
			};
			struct Cache {
				char32_t cp = 0;
				PayloadType payload{};
				Type type = Type::other;
			};

		private:
			detail::LocalBuffer<Cache, 2> pCache;
			Type pLast = Type::other;
			Type pActual = Type::other;
			Chain pChain = Chain::none;
			SB7State pSB7State = SB7State::none;
			bool pInitialized = false;
			bool pSB8Uncertain = false;

		public:
			constexpr SentenceBreak() = default;

		private:
			constexpr Chain fChainState(Type l) const {
				if (l == Type::aTerm)
					return Chain::aClose;
				if (l == Type::sTerm)
					return Chain::sClose;

				if (l == Type::separator || l == Type::cr || l == Type::lf)
					return (pChain == Chain::none ? Chain::none : Chain::paraSep);

				switch (pChain) {
				case Chain::aClose:
					if (l == Type::close)
						return Chain::aClose;
					if (l == Type::space)
						return Chain::aSpace;
					break;
				case Chain::sClose:
					if (l == Type::close)
						return Chain::sClose;
					if (l == Type::space)
						return Chain::sSpace;
					break;
				case Chain::aSpace:
					if (l == Type::space)
						return Chain::aSpace;
					break;
				case Chain::sSpace:
					if (l == Type::space)
						return Chain::sSpace;
					break;
				}
				return Chain::none;
			}
			constexpr SB7State fSB7State(Type l) const {
				if (l == Type::upper || l == Type::lower)
					return SB7State::aTerm;
				if (pSB7State == SB7State::aTerm && l == Type::aTerm)
					return SB7State::match;
				return SB7State::none;
			}
			constexpr bool fNext(Type r, const PayloadType& payload, bool done) {
				/* SB2: check if the end has been reached */
				if (done) {
					static_cast<SelfType*>(this)->fDone();
					return false;
				}
				bool isExtFmt = (r == Type::extend || r == Type::format);

				/* update the last-states and the state-machine */
				Type l = pLast, lActual = pActual;
				if (!isExtFmt) {
					pChain = fChainState(l);
					pSB7State = fSB7State(l);
					pLast = r;
				}
				pActual = r;

				/* SB3/SB4 */
				if (lActual == Type::cr || lActual == Type::lf || lActual == Type::separator) {
					if (lActual == Type::cr && r == Type::lf)
						return true;
					static_cast<SelfType*>(this)->fNext(payload, true);
					pLast = r;
					return false;
				}

				/* fast-path */
				if (l == Type::other)
					return true;

				/* SB5 */
				if (isExtFmt)
					return true;

				/* SB6/SB7 */
				if (l == Type::aTerm) {
					if (r == Type::numeric)
						return true;
					if (pSB7State == SB7State::match && r == Type::upper)
						return true;
				}

				/* SB9/SB10 partial (consume as many spaces/closes as possible) */
				if (pChain == Chain::aClose || pChain == Chain::sClose) {
					if (r == Type::close || r == Type::space)
						return true;
				}
				else if ((pChain == Chain::aSpace || pChain == Chain::sSpace) && r == Type::space)
					return true;

				/* SB8a */
				if (pChain != Chain::none && pChain != Chain::paraSep && (r == Type::sContinue || r == Type::sTerm || r == Type::aTerm))
					return true;

				/* SB8 */
				if (pChain == Chain::aClose || pChain == Chain::aSpace) {
					if (r == Type::lower)
						return true;
					if (r == Type::other || r == Type::numeric || r == Type::close) {
						static_cast<SelfType*>(this)->fBeginUnknown(payload);
						pSB8Uncertain = true;
						return false;
					}
				}

				if (r == Type::cr || r == Type::lf || r == Type::separator) {
					/* SB9/SB10 rest */
					if (pChain != Chain::none && pChain != Chain::paraSep)
						return true;
				}

				/* SB11 */
				if (pChain != Chain::none) {
					static_cast<SelfType*>(this)->fNext(payload, true);
					return false;
				}

				/* SB998 */
				return true;
			}
			constexpr void fCloseState(Type r, bool forceClose) {
				/* check if a determined end has been encountered */
				if (r == Type::lower || forceClose) {
					static_cast<SelfType*>(this)->fEndUnknown(r != Type::lower);
					pSB8Uncertain = false;
					return;
				}

				/* check if the chain remains uncertain */
				if (r == Type::other || r == Type::extend || r == Type::format || r == Type::space || r == Type::numeric || r == Type::sContinue || r == Type::close)
					return;
				static_cast<SelfType*>(this)->fEndUnknown(true);
				pSB8Uncertain = false;
			}

		public:
			constexpr void operator()(char32_t cp, const PayloadType& payload) {
				Type type = static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::SentenceSegmentationOff) & detail::gen::SegmentationMask);
				bool done = (cp == cp::EndOfTokens);

				/* SB1: check if this is the initial character (only if its not an empty string) */
				if (!pInitialized) {
					if (done)
						return;
					pInitialized = true;
					pLast = type;
					pActual = type;
					static_cast<SelfType*>(this)->fBegin(payload);
					return;
				}

				/* fast-path of not being in a look-ahead state */
				if (!pSB8Uncertain) {
					if (fNext(type, payload, done))
						static_cast<SelfType*>(this)->fNext(payload, false);
					return;
				}

				/* feed the value into the cache and check if the state can be completed */
				pCache.push({ cp, payload, type });
				fCloseState(type, cp == cp::EndOfTokens);

				/* process the cached characters */
				while (!pSB8Uncertain && pCache.size() > 0) {
					auto [_cp, _payload, _type] = pCache.pop();
					if (fNext(_type, _payload, _cp == cp::EndOfTokens))
						static_cast<SelfType*>(this)->fNext(payload, false);

					/* check if a state-chain has been created and feed the cached values to it */
					for (size_t i = 0; pSB8Uncertain && i < pCache.size(); ++i) {
						Cache& cache = pCache.get(i);
						fCloseState(cache.type, cache.cp == cp::EndOfTokens);
					}
				}
			}
		};

		template <class SnkType>
		class SentenceBreakRange final : private detail::SentenceBreak<detail::SentenceBreakRange<SnkType>, size_t> {
			friend class detail::SentenceBreak<detail::SentenceBreakRange<SnkType>, size_t>;
		private:
			Range pCurrent;
			Range pCached;
			SnkType pSink;

		public:
			constexpr SentenceBreakRange(SnkType&& sink) : pSink{ sink } {}

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
				detail::SentenceBreak<detail::SentenceBreakRange<SnkType>, size_t>::operator()(cp, index);
			}
		};

		enum class BreakType : uint8_t {
			mandatory,
			optional,
			combine
		};
		template <class SelfType>
		class LineBreak {
		private:
			using Type = detail::gen::LineType;
			static_assert(size_t(Type::_last) == 50, "Only types 0-49 are known by the state-machine");

		private:
			enum class State : uint8_t {
				uninit,
				none,
				lb15b,
				lb28a,
				lb25
			};
			enum class Break : uint8_t {
				optional,
				mandatory,
				combine,
				uncertain
			};
			enum class Chain : uint8_t {
				none,
				lb15a,
				lb21a,
				lb28a,

				lb25,
				lb25PrOrPo
			};

		private:
			Type pLast = Type::_last;
			Type pActual = Type::_last;
			State pState = State::uninit;
			Chain pChain = Chain::none;
			bool pSpaces = false;
			bool pRICountOdd = false;

		public:
			constexpr LineBreak() = default;

		private:
			constexpr Break fCheckState(Type r, bool endOfTokens) {
				/* type is Type::_last for end-of-tokens */
				if (r == Type::cm || r == Type::zwj) {
					pActual = r;
					pSpaces = false;
					return Break::uncertain;
				}
				pChain = Chain::none;

				/* LB15b with certainty that it was followed by space, and would therefore otherwise be an optional break */
				if (pState == State::lb15b) {
					if (r == Type::sp || r == Type::gl || r == Type::wj || r == Type::cl || r == Type::quDef || r == Type::quPf || r == Type::quPi ||
						r == Type::cpDef || r == Type::cpNoFWH || r == Type::ex || r == Type::is || r == Type::sy || r == Type::bk || r == Type::cr ||
						r == Type::lf || r == Type::nl || r == Type::zw || endOfTokens)
						return Break::combine;
					return Break::optional;
				}

				/* LB25 */
				if (pState == State::lb25) {
					if (r == Type::nu) {
						pChain = Chain::lb25;
						return Break::combine;
					}
					return Break::optional;
				}

				/* LB28a (type is other for end-of-tokens) */
				if (r == Type::vf)
					return Break::combine;
				return Break::optional;
			}
			constexpr Break fCheck(Type r) {
				/* LB4/LB5 */
				if (pActual == Type::cr || pActual == Type::bk || pActual == Type::lf || pActual == Type::nl) {
					Break out = ((pActual == Type::cr && r == Type::lf) ? Break::combine : Break::mandatory);
					pChain = (r == Type::quPi ? Chain::lb15a : Chain::none);
					pActual = (pLast = r);
					pSpaces = (r == Type::sp);
					pRICountOdd = false;
					return out;
				}

				/* LB7 */
				if (r == Type::sp) {
					pSpaces = true;
					pRICountOdd = false;
					return Break::combine;
				}

				/* update the spaces and left-value to be the actual last left value */
				bool spaces = pSpaces;
				pSpaces = false;
				Type l = pActual;
				pActual = r;

				/* LB6/LB7 */
				if (r == Type::bk || r == Type::cr || r == Type::lf || r == Type::nl || r == Type::zw) {
					pChain = Chain::none;
					pLast = r;
					pRICountOdd = false;
					return Break::combine;
				}

				/* LB8/LB8a */
				if (l == Type::zw || (l == Type::zwj && !spaces)) {
					pLast = r;
					pRICountOdd = false;
					if (l != Type::zw)
						return Break::combine;
					pChain = (r == Type::quPi ? Chain::lb15a : Chain::none);
					return Break::optional;
				}

				/* LB9 */
				if ((r == Type::cm || r == Type::zwj) && !spaces)
					return Break::combine;

				/* update the left-value and ri-counter */
				l = pLast;
				pLast = r;
				pRICountOdd = (l == Type::ri ? !pRICountOdd : false);

				switch (pChain) {
				case Chain::lb15a:
					/* LB15a */
					pChain = Chain::none;
					return Break::combine;

				case Chain::lb21a:
					/* LB21a */
					pChain = Chain::none;
					if (spaces)
						break;
					return Break::combine;

				case Chain::lb28a:
					/* LB28a */
					pChain = Chain::none;
					if (!spaces && (r == Type::ak || r == Type::alDotCircle))
						return Break::combine;
					break;

				case Chain::lb25:
					/* LB25 */
					if (spaces) {
						pChain = Chain::none;
						break;
					}
					if (r == Type::nu || r == Type::sy || r == Type::is)
						return Break::combine;
					if (r == Type::cl || r == Type::cpDef || r == Type::cpNoFWH) {
						pChain = Chain::lb25PrOrPo;
						return Break::combine;
					}
					[[fallthrough]];

				case Chain::lb25PrOrPo:
					/* LB25 */
					pChain = Chain::none;
					if (!spaces && (r == Type::pr || r == Type::po))
						return Break::combine;
					break;
				};

				switch (r) {
				case Type::wj:
					/* LB11 */
					return Break::combine;

				case Type::cl:
				case Type::cpDef:
				case Type::cpNoFWH:
				case Type::ex:
				case Type::is:
				case Type::sy:
					/* LB13 */
					return Break::combine;

				case Type::quPi:
					/* LB15a */
					if (l == Type::bk || l == Type::cr || l == Type::lf || l == Type::nl || l == Type::opDef || l == Type::opNoFWH ||
						l == Type::quDef || l == Type::quPf || l == Type::quPi || l == Type::gl || l == Type::zw || spaces)
						pChain = Chain::lb15a;
					break;

				case Type::quPf:
					/* LB15b */
					if (spaces) {
						pState = State::lb15b;
						return Break::uncertain;
					}
					break;

				case Type::gl:
					/* LB12a */
					if (!spaces && l != Type::ba && l != Type::hy)
						return Break::combine;
					break;
				}

				switch (l) {
				case Type::hl:
					/* LB21a */
					if (!spaces && (r == Type::hy || r == Type::ba))
						pChain = Chain::lb21a;
					break;

				case Type::b2:
					/* LB17 */
					if (r == Type::b2)
						return Break::combine;
					break;

				case Type::cl:
				case Type::cpDef:
				case Type::cpNoFWH:
					/* LB16 */
					if (r == Type::ns)
						return Break::combine;
					break;

				case Type::opDef:
				case Type::opNoFWH:
					/* LB25 */
					if (!spaces && r == Type::nu)
						pChain = Chain::lb25;

					/* LB14 */
					return Break::combine;

				case Type::wj:
				case Type::gl:
					/* LB18/LB11/LB12 */
					if (spaces)
						return Break::optional;
					return Break::combine;

				case Type::quDef:
				case Type::quPf:
				case Type::quPi:
					/* LB18/LB19 */
					if (spaces)
						return Break::optional;
					return Break::combine;

				case Type::cb:
					/* LB18/LB19/LB20 */
					if (spaces || (r != Type::quDef && r != Type::quPf && r != Type::quPi))
						return Break::optional;
					return Break::combine;

				case Type::bb:
					/* LB18/LB20/LB21 */
					if (spaces || r == Type::cb)
						return Break::optional;
					return Break::combine;
				}

				/* LB18 */
				if (spaces)
					return Break::optional;

				/* LB10 - explicitly implemented */
				switch (r) {
				case Type::quDef:
				case Type::quPf:
				case Type::quPi:
					/* LB19 */
					return Break::combine;

				case Type::cb:
					/* LB20 */
					return Break::optional;

				case Type::hy:
				case Type::ba:
				case Type::ns:
					/* LB21 */
					return Break::combine;

				case Type::hl:
					/* LB21b */
					if (l == Type::sy)
						return Break::combine;

					/* LB23 */
					if (l == Type::nu)
						return Break::combine;

					/* LB24 */
					if (l == Type::pr || l == Type::po)
						return Break::combine;

					/* LB28 */
					if (l == Type::alDef || l == Type::alDotCircle || l == Type::cm || l == Type::zwj || l == Type::hl)
						return Break::combine;

					/* LB29 */
					if (l == Type::is)
						return Break::combine;

					/* LB30 */
					if (l == Type::cpNoFWH)
						return Break::combine;
					break;

				case Type::in:
					/* LB22 */
					return Break::combine;

				case Type::nu:
					/* LB23 */
					if (l == Type::alDef || l == Type::alDotCircle || l == Type::cm || l == Type::zwj || l == Type::hl)
						return Break::combine;

					/* LB25 */
					pChain = Chain::lb25;
					if (l == Type::pr || l == Type::po || l == Type::opDef || l == Type::opNoFWH || l == Type::hy)
						return Break::combine;

					/* LB30 */
					if (l == Type::cpNoFWH)
						return Break::combine;
					break;

				case Type::alDotCircle:
					/* LB28a */
					if (l == Type::ap)
						return Break::combine;

					/* LB28a */
					if (l == Type::ak || l == Type::as) {
						pState = State::lb28a;
						return Break::uncertain;
					}
					[[fallthrough]];

				case Type::alDef:
				case Type::cm:
				case Type::zwj:
					/* LB23 */
					if (l == Type::nu)
						return Break::combine;

					/* LB24 */
					if (l == Type::pr || l == Type::po)
						return Break::combine;

					/* LB28 */
					if (l == Type::alDef || l == Type::alDotCircle || l == Type::cm || l == Type::zwj || l == Type::hl)
						return Break::combine;

					/* LB29 */
					if (l == Type::is)
						return Break::combine;

					/* LB30 */
					if (l == Type::cpNoFWH)
						return Break::combine;
					break;

				case Type::ak:
				case Type::as:
					/* LB28a */
					if (l == Type::ap)
						return Break::combine;
					if (l == Type::ak || l == Type::alDotCircle || l == Type::as) {
						pState = State::lb28a;
						return Break::uncertain;
					}
					break;

				case Type::vi:
					/* LB28a */
					if (l == Type::ak || l == Type::alDotCircle || l == Type::as) {
						pChain = Chain::lb28a;
						return Break::combine;
					}
					break;

				case Type::vf:
					/* LB28a */
					if (l == Type::ak || l == Type::alDotCircle || l == Type::as)
						return Break::combine;
					break;

				case Type::em:
					/* LB30b */
					if (l == Type::eb || l == Type::defCnPict || l == Type::idCnPict)
						return Break::combine;
					[[fallthrough]];

				case Type::idDef:
				case Type::idCnPict:
				case Type::eb:
					/* LB23a */
					if (l == Type::pr)
						return Break::combine;
					break;

				case Type::po:
					/* LB23a */
					if (l == Type::idDef || l == Type::idCnPict || l == Type::eb || l == Type::em)
						return Break::combine;

					/* LB24 */
					if (l == Type::alDef || l == Type::alDotCircle || l == Type::cm || l == Type::zwj || l == Type::hl)
						return Break::combine;

					/* LB27 */
					if (l == Type::jl || l == Type::jv || l == Type::jt || l == Type::h2 || l == Type::h3)
						return Break::combine;
					break;

				case Type::pr:
					/* LB24 */
					if (l == Type::alDef || l == Type::alDotCircle || l == Type::cm || l == Type::zwj || l == Type::hl)
						return Break::combine;
					break;

				case Type::opNoFWH:
					/* LB30 */
					if (l == Type::alDef || l == Type::alDotCircle || l == Type::cm || l == Type::zwj || l == Type::hl || l == Type::nu)
						return Break::combine;
					[[fallthrough]];

				case Type::opDef:
					/* LB25 */
					if (l == Type::pr || l == Type::po) {
						pState = State::lb25;
						return Break::uncertain;
					}
					break;

				case Type::jl:
				case Type::h2:
				case Type::h3:
					/* LB26 */
					if (l == Type::jl)
						return Break::combine;

					/* LB27 */
					if (l == Type::pr)
						return Break::combine;
					break;

				case Type::jv:
					/* LB26 */
					if (l == Type::jl || l == Type::jv || l == Type::h2)
						return Break::combine;

					/* LB27 */
					if (l == Type::pr)
						return Break::combine;
					break;

				case Type::jt:
					/* LB26 */
					if (l == Type::jv || l == Type::h2 || l == Type::jt || l == Type::h3)
						return Break::combine;

					/* LB27 */
					if (l == Type::pr)
						return Break::combine;
					break;

				case Type::ri:
					/* LB30a */
					if (l == Type::ri && pRICountOdd)
						return Break::combine;
					break;
				}

				/* LB31 */
				return Break::optional;
			}

		public:
			template <class PayloadType>
			constexpr void operator()(char32_t cp, const PayloadType& payload) {
				Type right = static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::LineSegmentationOff) & detail::gen::SegmentationMask);

				/* LB2: check if this is the initial character (only if its not an empty string) */
				if (pState == State::uninit) {
					if (cp == cp::EndOfTokens)
						return;
					static_cast<SelfType*>(this)->fBegin(payload);
					pState = State::none;

					/* initialize the local states */
					pLast = (pActual = right);
					if (right == Type::sp)
						pSpaces = true;
					else if (right == Type::nu)
						pChain = Chain::lb25;
					else if (right == Type::quPi)
						pChain = Chain::lb15a;
					return;
				}

				/* LB3: check if the end has been reached */
				if (cp == cp::EndOfTokens) {
					if (pState != State::none)
						static_cast<SelfType*>(this)->fEndUnknown((fCheckState(Type::_last, true) == Break::optional) ? BreakType::optional : BreakType::combine);
					static_cast<SelfType*>(this)->fDone();
					return;
				}

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					switch (fCheckState(right, false)) {
					case Break::uncertain:
						static_cast<SelfType*>(this)->fAddUnknown(payload);
						return;
					case Break::combine:
						static_cast<SelfType*>(this)->fEndUnknown(BreakType::combine);
						break;
					case Break::optional:
						static_cast<SelfType*>(this)->fEndUnknown(BreakType::optional);
						break;
					}
					pState = State::none;
				}

				/* check the current values and update the state */
				switch (fCheck(right)) {
				case Break::combine:
					static_cast<SelfType*>(this)->fNext(payload, BreakType::combine);
					break;
				case Break::optional:
					static_cast<SelfType*>(this)->fNext(payload, BreakType::optional);
					break;
				case Break::mandatory:
					static_cast<SelfType*>(this)->fNext(payload, BreakType::mandatory);
					break;
				case Break::uncertain:
					static_cast<SelfType*>(this)->fBeginUnknown(payload);
					break;
				}
			}
		};

		template <class SnkType>
		class LineBreakRange final : private detail::LineBreak<detail::LineBreakRange<SnkType>> {
			friend class detail::LineBreak<detail::LineBreakRange<SnkType>>;
		private:
			Range pCurrent;
			Range pCached;
			SnkType pSink;

		public:
			constexpr LineBreakRange(SnkType&& sink) : pSink{ sink } {}

		private:
			constexpr void fBeginUnknown(size_t index) {
				pCached = Range(index);
			}
			constexpr void fEndUnknown(BreakType type) {
				if (type != BreakType::combine) {
					pSink(cp::LineRange(pCurrent, type == BreakType::mandatory));
					pCurrent.first = pCached.first;
				}
				pCurrent.last = pCached.last;
			}
			constexpr void fAddUnknown(size_t index) {
				pCached.last = index;
			}
			constexpr void fNext(size_t index, BreakType type) {
				if (type != BreakType::combine) {
					pSink(cp::LineRange(pCurrent, type == BreakType::mandatory));
					pCurrent.first = index;
				}
				pCurrent.last = index;
			}
			constexpr void fBegin(size_t index) {
				pCurrent = Range(index);
			}
			constexpr void fDone() {
				pSink(cp::LineRange(pCurrent, false));
			}

		public:
			constexpr void operator()(char32_t cp, size_t index) {
				detail::LineBreak<detail::LineBreakRange<SnkType>>::operator()(cp, index);
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
				/* update the state-machine for 'finalSigma: Before C' */
				if ((val & detail::gen::CaseIsIgnorable) == 0)
					pBefore.cased = ((val & detail::gen::CaseIsCased) != 0);

				/* update the state-machine for 'afterSoftDotted: Before C' and 'afterI: Before C' */
				if ((val & (detail::gen::CaseIsCombClass0or230)) != 0) {
					pBefore.softDotted = ((val & detail::gen::CaseIsSoftDotted) != 0);
					pBefore.charI = ((val & detail::gen::CaseIs0049) != 0);
				}
			}
			constexpr int8_t fAfterState(int32_t val) const {
				/* check the 'finalSigma: After C' condition (is inverted) */
				if (pAfter.testNotCased) {
					if ((val & detail::gen::CaseIsIgnorable) != 0)
						return 0;
					return ((val & detail::gen::CaseIsCased) != 0 ? -1 : 1);
				}

				/* check the 'moreAbove: After C' condition */
				else if (pAfter.testCombClass) {
					if ((val & (detail::gen::CaseIsCombClass0or230)) == 0)
						return 0;
					return ((val & detail::gen::CaseIsCombClass230) != 0 ? 1 : -1);
				}

				/* check the 'beforeDot: After C' condition (inverted as it is only used as 'not_...') */
				else {
					if ((val & (detail::gen::CaseIsCombClass0or230)) == 0)
						return 0;
					return ((val & detail::gen::CaseIs0307) != 0 ? -1 : 1);
				}
			}
			constexpr char32_t fUnpackSingle(char32_t cp, int32_t data) {
				if ((data & static_cast<SelfType*>(this)->fTypeFlag()) == 0)
					return cp;

				int32_t value = (data & detail::gen::CaseBitsOfPayload);
				if (data & detail::gen::CaseIsNegative)
					value = -value;

				return char32_t(int32_t(cp) + value);
			}
			constexpr int8_t fSetupCondition() {
				pAfter = { false, false };

				/* check if the casing matches */
				if ((*pActive.begin & static_cast<SelfType*>(this)->fTypeFlag()) == 0)
					return -1;

				/* setup the state for the corresponding condition */
				switch (static_cast<Cond>(*pActive.begin & detail::gen::CaseBitsOfPayload)) {
				case Cond::none:
					return 1;
				case Cond::finalSigma:
					if (pBefore.cased) {
						pAfter.testNotCased = true;
						return 0;
					}
					break;
				case Cond::ltAfterSoftDotted:
					if (pBefore.softDotted && pLocale == detail::CaseLocale::lt)
						return 1;
					break;
				case Cond::ltMoreAbove:
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
				case Cond::trAfterI:
					if (pBefore.charI && pLocale == detail::CaseLocale::tr)
						return 1;
					break;
				case Cond::azAfterI:
					if (pBefore.charI && pLocale == detail::CaseLocale::az)
						return 1;
					break;
				case Cond::trNotBeforeDot:
					/* default pAfter is not_beforeDot */
					if (pLocale == detail::CaseLocale::tr)
						return 0;
					break;
				case Cond::azNotBeforeDot:
					/* default pAfter is not_beforeDot */
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
				return detail::gen::CaseIsUpper;
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
				return detail::gen::CaseIsLower;
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
				return (pLower ? detail::gen::CaseIsLower : detail::gen::CaseIsTitle);
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

	/* create a sink, which splits the stream into ranges of words and writes them to the sink (will be produced in-order)
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(Range): sink the range (not called for empty strings) */
	struct WordBreak {
		template <cp::IsSink<cp::Range> SnkType>
		constexpr detail::WordBreakRange<SnkType> operator()(SnkType&& sink) {
			return detail::WordBreakRange<SnkType>{ std::forward<SnkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of grapheme-clusters and writes them to the sink (will be produced in-order)
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(Range): sink the range (not called for empty strings) */
	struct GraphemeBreak {
		template <cp::IsSink<cp::Range> SnkType>
		constexpr detail::GraphemeBreakRange<SnkType> operator()(SnkType&& sink) {
			return detail::GraphemeBreakRange<SnkType>{ std::forward<SnkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of sentence-clusters and writes them to the sink (will be produced in-order)
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(Range): sink the range (not called for empty strings) */
	struct SentenceBreak {
		template <cp::IsSink<cp::Range> SnkType>
		constexpr detail::SentenceBreakRange<SnkType> operator()(SnkType&& sink) {
			return detail::SentenceBreakRange<SnkType>{ std::forward<SnkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of line-clusters and writes them to the sink (will be produced in-order)
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(LineRange): sink the range (not called for empty strings) */
	struct LineBreak {
		template <cp::IsSink<cp::LineRange> SnkType>
		constexpr detail::LineBreakRange<SnkType> operator()(SnkType&& sink) {
			return detail::LineBreakRange<SnkType>{ std::forward<SnkType>(sink) };
		}
	};

	/* create a sink, which writes the upper-cased stream to the given sink (will be produced in-order)
	*	InSink(char32_t): code-point
	*	OutSink(char32_t): code-point */
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
	*	InSink(char32_t): code-point
	*	OutSink(char32_t): code-point */
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
	*	InSink(char32_t): code-point
	*	OutSink(char32_t): code-point */
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
