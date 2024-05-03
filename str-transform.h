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
#include <type_traits>

namespace cp {
	enum class LineMode : uint8_t {
		emergency,
		mandatory,
		optional,
		combine
	};
	enum class BreakMode : uint8_t {
		emergency,
		mandatory,
		optional
	};

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
	struct LRange : public cp::Range {
	public:
		cp::BreakMode breakBefore = cp::BreakMode::emergency;

	public:
		constexpr LRange() = default;
		explicit constexpr LRange(const cp::Range& r) : cp::Range(r), breakBefore(cp::BreakMode::emergency) {}
		constexpr LRange(const cp::Range& r, cp::BreakMode brkBefore) : cp::Range(r), breakBefore(brkBefore) {}
		constexpr LRange(size_t f, size_t l, cp::BreakMode brkBefore) : cp::Range(f, l), breakBefore(brkBefore) {}
	};

	/* valid sinks for char32_t must receive zero or more valid codepoints and a final call to done(), after which
	*	the object is considered burnt (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type, class... ValType>
	concept IsSink = requires(Type t, ValType... v) {
		{ t(v...) } -> std::same_as<void>;
	};

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
			LocalBuffer() : pBuffer{ Static{} } {
				pBegin = std::get<Static>(pBuffer).buffer;
				pEnd = pBegin;
			}

		public:
			void push(const Type& t) {
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
			Type pop() {
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
			size_t size() const {
				return (pEnd - pBegin);
			}
			Type& get(size_t i) {
				return pBegin[i];
			}
			Type& front() {
				return pBegin[0];
			}
			Type& back() {
				return pEnd[-1];
			}
		};

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
			GB9cState pGB9cState = GB9cState::none;
			GB11State pGB11State = GB11State::none;
			uint8_t pLast = static_cast<uint8_t>(Type::other);
			bool pRICountOdd = false;

		public:
			constexpr GraphemeBreak() {}

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
					if (pRICountOdd && r == Type::regionalIndicator)
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
			constexpr void first(uint32_t raw) {
				/* GB1: initialize the state */
				pLast = ((raw >> detail::gen::GraphemeSegmentationOff) & detail::gen::SegmentationMask);
			}
			constexpr bool next(uint32_t raw) {
				/* GB2: no special handling for cleanup necessary */
				raw = ((raw >> detail::gen::GraphemeSegmentationOff) & detail::gen::SegmentationMask);
				Type left = static_cast<Type>(pLast & ~(detail::gen::GraphemeIsInCBExtend | detail::gen::GraphemeIsInCBConsonant | detail::gen::GraphemeIsInCBLinker));
				Type right = static_cast<Type>(raw & ~(detail::gen::GraphemeIsInCBExtend | detail::gen::GraphemeIsInCBConsonant | detail::gen::GraphemeIsInCBLinker));

				/* update the states */
				pGB9cState = fUpdateGB9cState(pLast);
				pGB11State = fUpdateGB11State(left);
				pRICountOdd = (left == Type::regionalIndicator ? !pRICountOdd : false);
				pLast = raw;

				/* check if the two values should be separated */
				return (fCheck(left, right, (raw & detail::gen::GraphemeIsInCBConsonant) != 0) == Break::separate);
			}
		};

		template <class SinkType, class PayloadType>
		class WordBreak {
		private:
			using Type = detail::gen::WordType;
			static_assert(size_t(Type::_last) == 19, "Only types 0-18 are known by the state-machine");

		private:
			enum class State : uint8_t {
				none,
				wb6,
				wb7a,
				wb7b,
				wb11
			};
			enum class Continue : uint8_t {
				uncertain,
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
			detail::LocalBuffer<PayloadType, 2> pCache;
			SinkType pSink;
			PayloadType pUncertainPayload{};
			Type pLast = Type::other;
			Type pLastActual = Type::other;
			State pState = State::none;
			int8_t pRICountEven = -1;

		public:
			constexpr WordBreak(SinkType&& sink) : pSink{ sink } {}

		private:
			constexpr Continue fCheckState(Type right) const {
				/* right will be 'other' for the final iteration and automatically clean it up properly */

				/* check if this is a silent reduction */
				if (right == Type::extend || right == Type::format || right == Type::zwj)
					return Continue::uncertain;

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
					if (pRICountEven == 1)
						return Break::combine;
					return Break::separate;
				}

				/* WB999 */
				return Break::separate;
			}

		public:
			constexpr void first(char32_t cp) {
				uint8_t raw = ((detail::gen::GetSegmentation(cp) >> detail::gen::WordSegmentationOff) & detail::gen::SegmentationMask);
				Type right = static_cast<Type>(raw & ~detail::gen::WordIsPictographic);

				/* WB1: initialize the regionalIndicator counter and the last-state */
				pRICountEven = (right == Type::regionalIndicator ? 0 : -1);
				pLast = right;
				pLastActual = right;
			}
			constexpr void next(char32_t cp, const PayloadType& payload) {
				uint8_t raw = ((detail::gen::GetSegmentation(cp) >> detail::gen::WordSegmentationOff) & detail::gen::SegmentationMask);
				Type right = static_cast<Type>(raw & ~detail::gen::WordIsPictographic);

				/* update the regionalIndicator counter and update the last-state */
				Type left = pLast, lActual = pLastActual;
				if (right != Type::extend && right != Type::format && right != Type::zwj) {
					if (right != Type::regionalIndicator)
						pRICountEven = -1;
					else
						pRICountEven = (pRICountEven < 0 ? 0 : 1 - pRICountEven);
					pLast = right;
				}
				pLastActual = right;

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					/* update the state and check if the state can be resolved */
					Continue cont = fCheckState(right);
					if (cont == Continue::uncertain) {
						pCache.push(payload);
						return;
					}
					pState = State::none;

					/* flush the cached characters */
					pSink(pUncertainPayload, cont == Continue::breakBeforeCached);
					while (pCache.size() > 0)
						pSink(pCache.pop(), false);

					/* check if the upcoming element can be consumed as well */
					if (cont == Continue::combineIncludingRight) {
						pSink(payload, false);
						return;
					}
				}

				/* check the current values and update the state */
				switch (fCheck(left, lActual, right, (raw & detail::gen::WordIsPictographic) != 0)) {
				case Break::combine:
					pSink(payload, false);
					break;
				case Break::separate:
					pSink(payload, true);
					break;
				case Break::uncertain:
					pUncertainPayload = payload;
					break;
				}
			}
			constexpr void done() {
				/* WB2: check if a cached state needs to be ended */
				if (pState == State::none)
					return;

				/* flush the cached characters */
				pSink(pUncertainPayload, fCheckState(Type::other) == Continue::breakBeforeCached);
				while (pCache.size() > 0)
					pSink(pCache.pop(), false);
			}
		};

		template <class SinkType, class PayloadType>
		class SentenceBreak {
		private:
			using Type = detail::gen::SentenceType;
			static_assert(size_t(Type::_last) == 15, "Only types 0-14 are known by the state-machine");

		private:
			enum class Break : uint8_t {
				separate,
				combine,
				uncertain
			};
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
				PayloadType payload{};
				Type type = Type::other;
			};

		private:
			detail::LocalBuffer<Cache, 2> pCache;
			PayloadType pUncertainPayload{};
			SinkType pSink;
			Type pLast = Type::other;
			Type pActual = Type::other;
			Chain pChain = Chain::none;
			SB7State pSB7State = SB7State::none;
			bool pUncertain = false;

		public:
			constexpr SentenceBreak(SinkType&& sink) : pSink{ sink } {}

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
			constexpr Break fNext(Type r) {
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
						return Break::combine;
					pLast = r;
					return Break::separate;
				}

				/* fast-path */
				if (l == Type::other)
					return Break::combine;

				/* SB5 */
				if (isExtFmt)
					return Break::combine;

				/* SB6/SB7 */
				if (l == Type::aTerm) {
					if (r == Type::numeric)
						return Break::combine;
					if (pSB7State == SB7State::match && r == Type::upper)
						return Break::combine;
				}

				/* SB9/SB10 partial (consume as many spaces/closes as possible) */
				if (pChain == Chain::aClose || pChain == Chain::sClose) {
					if (r == Type::close || r == Type::space)
						return Break::combine;
				}
				else if ((pChain == Chain::aSpace || pChain == Chain::sSpace) && r == Type::space)
					return Break::combine;

				/* SB8a */
				if (pChain != Chain::none && pChain != Chain::paraSep && (r == Type::sContinue || r == Type::sTerm || r == Type::aTerm))
					return Break::combine;

				/* SB8 */
				if (pChain == Chain::aClose || pChain == Chain::aSpace) {
					if (r == Type::lower)
						return Break::combine;
					if (r == Type::other || r == Type::numeric || r == Type::close)
						return Break::uncertain;
				}

				if (r == Type::cr || r == Type::lf || r == Type::separator) {
					/* SB9/SB10 rest */
					if (pChain != Chain::none && pChain != Chain::paraSep)
						return Break::combine;
				}

				/* SB11 */
				if (pChain != Chain::none)
					return Break::separate;

				/* SB998 */
				return Break::combine;
			}
			constexpr Break fCloseState(Type r) const {
				/* check if a determined end has been encountered */
				if (r == Type::lower)
					return Break::combine;

				/* check if the chain remains uncertain */
				if (r == Type::other || r == Type::extend || r == Type::format || r == Type::space || r == Type::numeric || r == Type::sContinue || r == Type::close)
					return Break::uncertain;
				return Break::separate;
			}
			template <bool IsCleanup>
			constexpr void fProcessQueue(Break brk) {
				/* process the cached characters */
				while (true) {
					/* check if the last state needs to be forcefully aborted or if the result is incomplete */
					if (brk == Break::uncertain) {
						if constexpr (!IsCleanup)
							return;
						brk = Break::separate;
					}
					pSink(pUncertainPayload, brk == Break::separate);

					/* process the next character */
					if (pCache.size() == 0) {
						pUncertain = false;
						return;
					}
					brk = fNext(pCache.front().type);
					pUncertainPayload = pCache.pop().payload;

					/* feed the cached characters to it to try to complete the state */
					for (size_t i = 0; brk == Break::uncertain && i < pCache.size(); ++i)
						brk = fCloseState(pCache.get(i).type);
				}
			}

		public:
			constexpr void first(char32_t cp) {
				/* SB1: initialize the state */
				Type type = static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::SentenceSegmentationOff) & detail::gen::SegmentationMask);
				pLast = type;
				pActual = type;
			}
			constexpr void next(char32_t cp, const PayloadType& payload) {
				Type type = static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::SentenceSegmentationOff) & detail::gen::SegmentationMask);

				/* fast-path of not being in a look-ahead state */
				if (!pUncertain) {
					if (Break brk = fNext(type); brk != Break::uncertain)
						pSink(payload, brk == Break::separate);
					else {
						pUncertainPayload = payload;
						pUncertain = true;
					}
					return;
				}

				/* add the character to the cache and update the current state */
				pCache.push({ payload, type });
				fProcessQueue<false>(fCloseState(type));
			}
			constexpr void done() {
				/* SB2: clear the cache and abort the current cached state */
				if (pUncertain)
					fProcessQueue<true>(Break::separate);
			}
		};

		template <class SinkType, class PayloadType>
		class LineBreak {
		private:
			using Type = detail::gen::LineType;
			static_assert(size_t(Type::_last) == 50, "Only types 0-49 are known by the state-machine");

		private:
			enum class Emergency : uint8_t {
				emergency,
				graphemeAware,
				none
			};
			enum class State : uint8_t {
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
			struct Cache {
				PayloadType payload{};
				bool combine = false;
			};

		private:
			detail::GraphemeBreak pGrapheme;
			detail::LocalBuffer<Cache, 2> pCache;
			PayloadType pUncertainPayload{};
			SinkType pSink;
			Type pLast = Type::_last;
			Type pActual = Type::_last;
			State pState = State::none;
			Chain pChain = Chain::none;
			bool pSpaces = false;
			bool pRICountOdd = false;
			Emergency pCheckEmergency = Emergency::none;

		public:
			constexpr LineBreak(bool checkGrapheme, bool createEmergency, SinkType&& sink) : pSink{ sink } {
				if (checkGrapheme)
					pCheckEmergency = (createEmergency ? Emergency::emergency : Emergency::graphemeAware);
			}

		private:
			constexpr Break fCheckState(Type r) {
				/* check if the character does not close the state */
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
						r == Type::lf || r == Type::nl || r == Type::zw)
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
			constexpr void fPopQueue(bool combine) {
				/* post all cached characters (first cannot be a grapheme-internal) */
				pSink(pUncertainPayload, (combine ? cp::LineMode::combine : cp::LineMode::optional));
				while (pCache.size() > 0) {
					auto [_payload, _combine] = pCache.pop();
					pSink(_payload, _combine ? cp::LineMode::combine : cp::LineMode::emergency);
				}
			}

		public:
			constexpr void first(char32_t cp) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				Type right = static_cast<Type>((raw >> detail::gen::LineSegmentationOff) & detail::gen::SegmentationMask);

				/* LB2: initialize the state */
				if (pCheckEmergency != Emergency::none)
					pGrapheme.first(raw);
				pLast = (pActual = right);
				if (right == Type::sp)
					pSpaces = true;
				else if (right == Type::nu)
					pChain = Chain::lb25;
				else if (right == Type::quPi)
					pChain = Chain::lb15a;
			}
			constexpr void next(char32_t cp, const PayloadType& payload) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				Type right = static_cast<Type>((raw >> detail::gen::LineSegmentationOff) & detail::gen::SegmentationMask);

				/* check if this is part of a single grapheme (No need to check for mandatory breaks separately, as
				*	they are all considered 'control' by grapheme-clusters, which are broken on either side anyways) */
				if (pCheckEmergency != Emergency::none && !pGrapheme.next(raw)) {
					if (pState == State::none)
						pSink(payload, cp::LineMode::combine);
					else
						pCache.push({ payload, true });
					return;
				}

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					Break brk = fCheckState(right);
					if (brk == Break::uncertain) {
						pCache.push({ payload, (pCheckEmergency != Emergency::emergency) });
						return;
					}
					pState = State::none;

					/* clear all cached characters */
					fPopQueue(brk == Break::combine);
				}

				/* check the current values and update the state */
				switch (fCheck(right)) {
				case Break::combine:
					pSink(payload, (pCheckEmergency == Emergency::emergency ? cp::LineMode::emergency : cp::LineMode::combine));
					break;
				case Break::optional:
					pSink(payload, cp::LineMode::optional);
					break;
				case Break::mandatory:
					pSink(payload, cp::LineMode::mandatory);
					break;
				case Break::uncertain:
					pUncertainPayload = payload;
					break;
				}
			}
			constexpr void done() {
				/* LB3: cleanup the final cached state */
				if (pState != State::none)
					fPopQueue(pState == State::lb15b);
			}
		};




		template <class SinkType>
		class WordBreakSeparate {
		private:
			struct Lambda {
				WordBreakSeparate<SinkType>& self;
				constexpr Lambda(WordBreakSeparate<SinkType>& s) : self{ s } {}
				constexpr void operator()(uint8_t, bool separate) {
					self.pSink(separate);
				}
			};

		private:
			detail::WordBreak<Lambda, uint8_t> pBreaker;
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

	/* create a sink, which receives the 'grapheme-break-before' attribute for every codepoint except for the first codepoint (will be produced in-order)
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output
	*	OutSink(size_t, bool): insert break before codepoint at given index */
	class GraphemeBreak {
	private:
		template <class SinkType>
		class Impl {
		private:
			detail::GraphemeBreak pBreaker;
			SinkType pSink;
			bool pInitialized = false;

		public:
			constexpr Impl(SinkType&& sink) : pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				if (pInitialized)
					pSink(index, pBreaker.next(raw));
				else {
					pInitialized = true;
					pBreaker.first(raw);
				}
			}
			constexpr void done() {}
		};

	public:
		template <cp::IsSink<size_t, bool> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of grapheme-clusters and writes them to the sink (will be produced in-order; no output if string is empty)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(cp::Range): range of a single grapheme-cluster */
	class GraphemeRanges {
	private:
		template <class SinkType>
		class Impl {
		private:
			detail::GraphemeBreak pBreaker;
			SinkType pSink;
			size_t pStart = 0;
			size_t pLast = 0;
			bool pInitialized = false;

		public:
			constexpr Impl(SinkType&& sink) : pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				if (pInitialized) {
					if (pBreaker.next(raw)) {
						pSink(cp::Range(pStart, pLast));
						pStart = index;
					}
					pLast = index;
				}
				else {
					pInitialized = true;
					pStart = index;
					pLast = index;
					pBreaker.first(raw);
				}
			}
			constexpr void done() {
				if (pInitialized)
					pSink(cp::Range(pStart, pLast));
			}
		};

	public:
		template <cp::IsSink<cp::Range> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which receives the 'word-break-before' attribute for every codepoint except for the first codepoint (will be produced in-order)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output
	*	OutSink(size_t, bool): insert break before codepoint at given index */
	class WordBreak {
	private:
		template <class SinkType>
		class Impl {
		private:
			struct Lambda {
				Impl<SinkType>& self;
				constexpr Lambda(Impl<SinkType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, bool separate) {
					self.pSink(payload, separate);
				}
			};

		private:
			detail::WordBreak<Lambda, size_t> pBreaker;
			SinkType pSink;
			bool pInitialized = false;

		public:
			constexpr Impl(SinkType&& sink) : pBreaker{ Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				if (pInitialized)
					pBreaker.next(cp, index);
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

	public:
		template <cp::IsSink<size_t, bool> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of words and writes them to the sink (will be produced in-order; no output if string is empty)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(cp::Range): range of a single word */
	class WordRanges {
	private:
		template <class SinkType>
		class Impl {
		private:
			struct Lambda {
				Impl<SinkType>& self;
				constexpr Lambda(Impl<SinkType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, bool separate) {
					if (separate) {
						self.pSink(cp::Range(self.pStart, self.pLast));
						self.pStart = payload;
					}
					self.pLast = payload;
				}
			};

		private:
			detail::WordBreak<Lambda, size_t> pBreaker;
			SinkType pSink;
			size_t pStart = 0;
			size_t pLast = 0;
			bool pInitialized = false;

		public:
			constexpr Impl(SinkType&& sink) : pBreaker{ Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				if (pInitialized)
					pBreaker.next(cp, index);
				else {
					pInitialized = true;
					pStart = index;
					pLast = index;
					pBreaker.first(cp);
				}
			}
			constexpr void done() {
				if (pInitialized) {
					pBreaker.done();
					pSink(cp::Range(pStart, pLast));
				}
			}
		};

	public:
		template <cp::IsSink<cp::Range> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which receives the 'sentence-break-before' attribute for every codepoint except for the first codepoint (will be produced in-order)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output
	*	OutSink(size_t, bool): insert break before codepoint at given index */
	class SentenceBreak {
	private:
		template <class SinkType>
		class Impl {
		private:
			struct Lambda {
				Impl<SinkType>& self;
				constexpr Lambda(Impl<SinkType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, bool separate) {
					self.pSink(payload, separate);
				}
			};

		private:
			detail::SentenceBreak<Lambda, size_t> pBreaker;
			SinkType pSink;
			bool pInitialized = false;

		public:
			constexpr Impl(SinkType&& sink) : pBreaker{ Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				if (pInitialized)
					pBreaker.next(cp, index);
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

	public:
		template <cp::IsSink<size_t, bool> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of sentence-clusters and writes them to the sink (will be produced in-order; no output if string is empty)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(cp::Range): range of a single sentence */
	class SentenceRanges {
	private:
		template <class SinkType>
		class Impl {
		private:
			struct Lambda {
				Impl<SinkType>& self;
				constexpr Lambda(Impl<SinkType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, bool separate) {
					if (separate) {
						self.pSink(cp::Range(self.pStart, self.pLast));
						self.pStart = payload;
					}
					self.pLast = payload;
				}
			};

		private:
			detail::SentenceBreak<Lambda, size_t> pBreaker;
			SinkType pSink;
			size_t pStart = 0;
			size_t pLast = 0;
			bool pInitialized = false;

		public:
			constexpr Impl(SinkType&& sink) : pBreaker{ Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				if (pInitialized)
					pBreaker.next(cp, index);
				else {
					pInitialized = true;
					pStart = index;
					pLast = index;
					pBreaker.first(cp);
				}
			}
			constexpr void done() {
				if (pInitialized) {
					pBreaker.done();
					pSink(cp::Range(pStart, pLast));
				}
			}
		};

	public:
		template <cp::IsSink<cp::Range> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which receives the 'line-break-before' attribute for every codepoint except for the first codepoint (will be produced in-order)
	*	Additionally specify whether to not break grapheme-clusters, produce emergency-breaks (based on grapheme-clusters), or perform default line-breaking
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output
	*	OutSink(size_t, cp::LineMode): insert corresponding break before codepoint at given index */
	class LineBreak {
	private:
		template <class SinkType>
		class Impl {
		private:
			struct Lambda {
				Impl<SinkType>& self;
				constexpr Lambda(Impl<SinkType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, cp::LineMode mode) {
					self.pSink(payload, mode);
				}
			};

		private:
			detail::LineBreak<Lambda, size_t> pBreaker;
			SinkType pSink;
			bool pInitialized = false;

		public:
			constexpr Impl(bool graphemeAware, bool createEmergency, SinkType&& sink) : pBreaker{ graphemeAware, createEmergency, Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				if (pInitialized)
					pBreaker.next(cp, index);
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

	private:
		bool pGraphemeAware = false;
		bool pCreateEmergency = false;

	public:
		LineBreak(bool graphemeAware = true, bool createEmergency = true) : pGraphemeAware(graphemeAware), pCreateEmergency(createEmergency) {}

	public:
		template <cp::IsSink<size_t, cp::LineMode> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ pGraphemeAware, pCreateEmergency, std::forward<SinkType>(sink) };
		}
	};

	/* create a sink, which splits the stream into ranges of line-clusters and writes them to the sink (will be produced in-order; no output if string is empty)
	*	Additionally specify whether to not break grapheme-clusters, produce emergency-breaks (based on grapheme-clusters), or perform default line-breaking
	*	InSink(char32_t, size_t): code-point and index used to reference it in the output-ranges
	*	OutSink(cp::LRange): range of a single line-token and corresponding behavior before the range (cp::BreakMode::emergency for first range) */
	class LineRanges {
	private:
		template <class SinkType>
		class Impl {
		private:
			struct Lambda {
				Impl<SinkType>& self;
				constexpr Lambda(Impl<SinkType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, cp::LineMode mode) {
					if (mode != cp::LineMode::combine) {
						self.pSink(cp::LRange(self.pStart, self.pLast, self.pMode));
						self.pMode = (mode == cp::LineMode::emergency ? cp::BreakMode::emergency : (mode == cp::LineMode::mandatory ? cp::BreakMode::mandatory : cp::BreakMode::optional));
						self.pStart = payload;
					}
					self.pLast = payload;
				}
			};

		private:
			detail::LineBreak<Lambda, size_t> pBreaker;
			SinkType pSink;
			size_t pStart = 0;
			size_t pLast = 0;
			cp::BreakMode pMode = cp::BreakMode::emergency;
			bool pInitialized = false;

		public:
			constexpr Impl(bool graphemeAware, bool createEmergency, SinkType&& sink) : pBreaker{ graphemeAware, createEmergency, Lambda{ *this } }, pSink{ sink } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				if (pInitialized)
					pBreaker.next(cp, index);
				else {
					pInitialized = true;
					pStart = index;
					pLast = index;
					pBreaker.first(cp);
				}
			}
			constexpr void done() {
				if (pInitialized) {
					pBreaker.done();
					pSink(cp::LRange(pStart, pLast, pMode));
				}
			}
		};

	private:
		bool pGraphemeAware = false;
		bool pCreateEmergency = false;

	public:
		LineRanges(bool graphemeAware = true, bool createEmergency = true) : pGraphemeAware(graphemeAware), pCreateEmergency(createEmergency) {}

	public:
		template <cp::IsSink<cp::LRange> SinkType>
		constexpr Impl<SinkType> operator()(SinkType&& sink) {
			return Impl<SinkType>{ pGraphemeAware, pCreateEmergency, std::forward<SinkType>(sink) };
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
