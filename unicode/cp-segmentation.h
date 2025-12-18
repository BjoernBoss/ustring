/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2025 Bjoern Boss Henrichsen */
#pragma once

#include "../common/str-common.h"

#include "../generated/unicode-segmentation.h"

#include <utility>
#include <type_traits>

namespace cp {
	enum class BreakMode : uint8_t {
		none,
		optional,
		emergency,
		mandatory,
		edge
	};

	/* inclusive range */
	struct Range {
	public:
		size_t first = 0;
		size_t last = 0;
		cp::BreakMode breakBefore = cp::BreakMode::optional;

	public:
		constexpr Range() = default;
		explicit constexpr Range(size_t v, cp::BreakMode brkBefore) : first(v), last(v), breakBefore(brkBefore) {}
		explicit constexpr Range(size_t f, size_t l, cp::BreakMode brkBefore) : first(f), last(l), breakBefore(brkBefore) {}
	};

	namespace detail {
		template <class RecvType, template <class, class> class BreakType>
		class BreakSingle {
		private:
			struct Lambda {
				detail::BreakSingle<RecvType, BreakType>& self;
				constexpr Lambda(detail::BreakSingle<RecvType, BreakType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, cp::BreakMode mode) {
					self.pRecv(payload, mode);
				}
			};

		private:
			BreakType<Lambda, size_t> pBreaker;
			RecvType pRecv;
			bool pInitialized = false;

		public:
			template <class... Args>
			constexpr BreakSingle(RecvType&& recv, Args&&... args) : pBreaker{ Lambda{ *this }, std::forward<Args>(args)... }, pRecv{ std::forward<RecvType>(recv) } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				if (pInitialized)
					pBreaker.next(raw, index);
				else {
					pInitialized = true;
					pBreaker.first(raw);
				}
			}
			constexpr void done() {
				if (pInitialized)
					pBreaker.done();
			}
		};
		template <class RecvType, template <class, class> class BreakType>
		class BreakRanges {
		private:
			struct Lambda {
				detail::BreakRanges<RecvType, BreakType>& self;
				constexpr Lambda(detail::BreakRanges<RecvType, BreakType>& s) : self{ s } {}
				constexpr void operator()(size_t payload, cp::BreakMode mode) {
					if (mode != cp::BreakMode::none) {
						self.pRecv(cp::Range(self.pStart, self.pLast, self.pMode));
						self.pStart = payload;
						self.pMode = mode;
					}
					self.pLast = payload;
				}
			};

		private:
			BreakType<Lambda, size_t> pBreaker;
			RecvType pRecv;
			size_t pStart = 0;
			size_t pLast = 0;
			bool pInitialized = false;
			cp::BreakMode pMode = cp::BreakMode::edge;

		public:
			template <class... Args>
			constexpr BreakRanges(RecvType&& recv, Args&&... args) : pBreaker{ Lambda{ *this }, std::forward<Args>(args)... }, pRecv{ std::forward<RecvType>(recv) } {}

		public:
			constexpr void next(char32_t cp, size_t index) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				if (pInitialized)
					pBreaker.next(raw, index);
				else {
					pInitialized = true;
					pStart = index;
					pLast = index;
					pBreaker.first(raw);
				}
			}
			constexpr void done() {
				if (pInitialized) {
					pBreaker.done();
					pRecv(cp::Range(pStart, pLast, pMode));
				}
			}
		};
		template <class ItType, template <class> class BreakType>
		class BreakIterator {
		private:
			using State = typename BreakType<ItType>::State;
			static constexpr bool HasState = !std::is_void_v<State>;

		private:
			ItType pNextIt{};
			ItType pPrevIt{};
			uint32_t pNextRaw = 0;
			uint32_t pPrevRaw = 0;
			std::conditional_t<HasState, State, uint8_t> pState{};
			bool pPrevIsResult = false;

		public:
			constexpr BreakIterator(const ItType& it, std::conditional_t<HasState, State, uint8_t> state) : pNextIt{ it }, pPrevIt{ it } {
				pPrevRaw = detail::gen::GetSegmentation(it.valid() ? it.get() : str::Invalid);
				pNextRaw = pPrevRaw;
				if constexpr (HasState)
					pState = state;
			}

		private:
			constexpr cp::BreakMode fEval() {
				if constexpr (HasState)
					return BreakType<ItType>::Resolve(pPrevIt, pPrevRaw, pNextIt, pNextRaw, pState);
				else
					return BreakType<ItType>::Resolve(pPrevIt, pPrevRaw, pNextIt, pNextRaw);
			}

		public:
			constexpr cp::BreakMode next() {
				/* check if the iterator can be advanced */
				ItType it = pNextIt;
				if (!it.valid() || !it.advance()) {
					pPrevIsResult = false;
					return cp::BreakMode::edge;
				}

				/* setup the new set of iterators and evaluate the break-type between them */
				pPrevIt = pNextIt;
				pPrevRaw = pNextRaw;
				pNextIt = it;
				pNextRaw = detail::gen::GetSegmentation(pNextIt.get());
				pPrevIsResult = true;
				return fEval();
			}
			constexpr cp::BreakMode prev() {
				/* check if the iterator can be reverted */
				ItType it = pPrevIt;
				if (!it.valid() || !it.reverse()) {
					pPrevIsResult = true;
					return cp::BreakMode::edge;
				}

				/* setup the new set of iterators and evaluate the break-type between them */
				pNextIt = pPrevIt;
				pPrevIt = it;
				pNextRaw = pPrevRaw;
				pPrevRaw = detail::gen::GetSegmentation(pPrevIt.get());
				pPrevIsResult = false;
				return fEval();
			}
			constexpr const ItType& get() const {
				return (pPrevIsResult ? pPrevIt : pNextIt);
			}
		};

		struct GraphemeBreak {
			static_assert(size_t(detail::gen::GraphemeType::_end) == 21, "Only types 0-20 are known by the state-machine");
		public:
			using Type = detail::gen::GraphemeType;
			enum class Break : uint8_t {
				combine,
				separate
			};

		public:
			static constexpr Type GetRaw(uint32_t raw) {
				return static_cast<Type>((raw >> detail::gen::GraphemeSegmentationOff) & detail::gen::SegmentationMask);
			}
			static constexpr Type GetCP(char32_t cp) {
				return GetRaw(detail::gen::GetSegmentation(cp));
			}
			template <class SelfType>
			static constexpr Break Test(Type l, Type r, SelfType&& self) {
				switch (r) {
				case Type::lf:
					/* GB3 */
					if (l == Type::cr)
						return Break::combine;
					[[fallthrough]];
				case Type::control:
				case Type::cr:
					/* GB5 */
					return Break::separate;

				case Type::extendDef:
				case Type::extendInCBExtend:
				case Type::extendInCBLinker:
				case Type::zwjDef:
				case Type::zwjInCBExtend:
				case Type::spaceMarking:
					/* GB4 partial */
					if (l == Type::control || l == Type::cr || l == Type::lf)
						return Break::separate;
					/* GB9/GB9a */
					return Break::combine;

				case Type::inCBConsonant:
					/* GB9c */
					if (self.fGB9cHasConsonant())
						return Break::combine;
					break;
				default:
					break;
				}

				switch (l) {
				case Type::cr:
				case Type::lf:
				case Type::control:
					/* GB4 */
					return Break::separate;
				case Type::zwjDef:
				case Type::zwjInCBExtend:
					/* GB11 */
					if (self.fGB11HasExtPicto() && r == Type::extendedPictographic)
						return Break::combine;
					break;
				case Type::regionalIndicator:
					/* GB12/GB13 */
					if (r == Type::regionalIndicator && self.fIsRIChainEven())
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
				default:
					break;
				}

				/* GB999 */
				return Break::separate;
			}
		};
		template <class ItType>
		class GraphemeRandom {
			friend struct detail::GraphemeBreak;
			using Host = detail::GraphemeBreak;
		public:
			using State = void;

		private:
			const ItType& pLeft;

		private:
			constexpr GraphemeRandom(const ItType& l) : pLeft{ l } {}

		private:
			constexpr bool fGB9cHasConsonant() const {
				ItType t = pLeft;
				bool linkerFound = false;
				do {
					Host::Type type = Host::GetCP(t.get());
					if (type == Host::Type::inCBLinker || type == Host::Type::extendInCBLinker)
						linkerFound = true;
					else if (type == Host::Type::inCBConsonant)
						return linkerFound;
					else if (type != Host::Type::inCBExtend && type != Host::Type::extendInCBExtend && type != Host::Type::zwjInCBExtend)
						return false;
				} while (t.reverse());
				return false;
			}
			constexpr bool fGB11HasExtPicto() const {
				ItType t = pLeft;
				while (t.reverse()) {
					Host::Type type = Host::GetCP(t.get());
					if (type == Host::Type::extendedPictographic)
						return true;
					if (type != Host::Type::extendDef && type != Host::Type::extendInCBExtend && type != Host::Type::extendInCBLinker)
						return false;
				}
				return false;
			}
			constexpr bool fIsRIChainEven() const {
				ItType t = pLeft;
				size_t count = 0;
				while (t.reverse() && Host::GetCP(t.get()) == Host::Type::regionalIndicator)
					++count;
				return ((count & 0x01) == 0);
			}

		public:
			static constexpr cp::BreakMode Resolve(const ItType& lIt, uint32_t lRaw, const ItType& rIt, uint32_t rRaw) {
				Host::Type lType = Host::GetRaw(lRaw), rType = Host::GetRaw(rRaw);
				if (Host::Test(lType, rType, detail::GraphemeRandom<ItType>{ lIt }) == Host::Break::separate)
					return cp::BreakMode::optional;
				return cp::BreakMode::none;
			}
		};
		template <class RecvType, class PayloadType>
		class GraphemeForward {
			friend struct detail::GraphemeBreak;
			using Host = detail::GraphemeBreak;
		private:
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
			RecvType pRecv;
			GB9cState pGB9cState = GB9cState::none;
			GB11State pGB11State = GB11State::none;
			Host::Type pLast = Host::Type::other;
			bool pRICountOdd = false;

		public:
			constexpr GraphemeForward(RecvType&& recv) : pRecv{ std::forward<RecvType>(recv) } {}

		private:
			constexpr bool fGB9cHasConsonant() const {
				return (pGB9cState == GB9cState::match);
			}
			constexpr bool fGB11HasExtPicto() const {
				return (pGB11State == GB11State::match);
			}
			constexpr bool fIsRIChainEven() const {
				/* check if odd, as it already contains the trailing RI on the left side of GB12/GB13 */
				return pRICountOdd;
			}

		private:
			constexpr GB9cState fUpdateGB9cState(Host::Type l) const {
				if (l == Host::Type::inCBConsonant)
					return GB9cState::linker;

				if (pGB9cState == GB9cState::linker) {
					if (l == Host::Type::inCBExtend || l == Host::Type::extendInCBExtend || l == Host::Type::zwjInCBExtend)
						return GB9cState::linker;
					if (l == Host::Type::inCBLinker || l == Host::Type::extendInCBLinker)
						return GB9cState::match;
				}
				else if (pGB9cState == GB9cState::match) {
					if (l == Host::Type::inCBExtend || l == Host::Type::extendInCBExtend || l == Host::Type::zwjInCBExtend)
						return GB9cState::match;
					if (l == Host::Type::inCBLinker || l == Host::Type::extendInCBLinker)
						return GB9cState::match;
				}
				return GB9cState::none;
			}
			constexpr GB11State fUpdateGB11State(Host::Type l) const {
				if (l == Host::Type::extendedPictographic)
					return GB11State::zwj;

				if (pGB11State == GB11State::zwj) {
					if (l == Host::Type::extendDef || l == Host::Type::extendInCBExtend || l == Host::Type::extendInCBLinker)
						return GB11State::zwj;
					if (l == Host::Type::zwjDef || l == Host::Type::zwjInCBExtend)
						return GB11State::match;
				}
				return GB11State::none;
			}

		public:
			constexpr void first(uint32_t raw) {
				/* GB1: initialize the state */
				pLast = Host::GetRaw(raw);
			}
			constexpr void next(uint32_t raw, const PayloadType& payload) {
				/* GB2: no special handling for cleanup necessary */
				Host::Type left = pLast, right = Host::GetRaw(raw);

				/* update the states */
				pGB9cState = fUpdateGB9cState(left);
				pGB11State = fUpdateGB11State(left);
				pRICountOdd = (left == Host::Type::regionalIndicator ? !pRICountOdd : false);
				pLast = right;

				/* check if the two values should be separated and write the result to the receiver */
				bool separate = (Host::Test(left, right, *this) == Host::Break::separate);
				pRecv(payload, separate ? cp::BreakMode::optional : cp::BreakMode::none);
			}
			constexpr void done() {}
		};

		struct WordBreak {
			static_assert(size_t(detail::gen::WordType::_end) == 21, "Only types 0-20 are known by the state-machine");
		public:
			using Type = detail::gen::WordType;
			enum class Break : uint8_t {
				separate,
				combine,
				uncertain
			};

		public:
			static constexpr Type GetRaw(uint32_t raw) {
				return static_cast<Type>((raw >> detail::gen::WordSegmentationOff) & detail::gen::SegmentationMask);
			}
			static constexpr Type GetCP(char32_t cp) {
				return GetRaw(detail::gen::GetSegmentation(cp));
			}
			template <class SelfType>
			static constexpr Break Test(Type l, Type r, SelfType&& self) {
				switch (r) {
				case Type::lf:
					/* WB3 */
					if (l == Type::cr)
						return Break::combine;
					[[fallthrough]];
				case Type::newline:
				case Type::cr:
					/* WB3b */
					return Break::separate;
				case Type::extend:
				case Type::format:
				case Type::zwj:
					/* WB3c */
					if (l == Type::newline || l == Type::cr || l == Type::lf)
						return Break::separate;
					/* WB4 */
					return Break::combine;
				case Type::wSegSpace:
					/* Wb3d */
					if (l == Type::wSegSpace)
						return Break::combine;
					break;
				case Type::extendedPictographic:
				case Type::aLetterExtendedPictographic:
					/* WB3c */
					if (l == Type::zwj)
						return Break::combine;
					break;
				default:
					break;
				}

				l = self.fSkipWB4(l);
				switch (l) {
				case Type::cr:
				case Type::newline:
				case Type::lf:
					/* WB3a */
					return Break::separate;
				case Type::hebrewLetter:
					/* WB7b/WB7c */
					if (r == Type::doubleQuote)
						return self.fWB7bCheck();

					/* WB7a */
					if (r == Type::singleQuote)
						return self.fWB7aCheck();
					[[fallthrough]];
				case Type::aLetterDef:
				case Type::aLetterExtendedPictographic:
					/* WB5/WB9 */
					if (r == Type::aLetterDef || r == Type::aLetterExtendedPictographic || r == Type::hebrewLetter || r == Type::numeric)
						return Break::combine;

					/* WB6/WB7 */
					if (r == Type::midLetter || r == Type::midNumLetter || r == Type::singleQuote)
						return self.fWB6Check();

					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;
					break;
				case Type::numeric:
					/* WB8/WB10 */
					if (r == Type::numeric || r == Type::aLetterDef || r == Type::aLetterExtendedPictographic || r == Type::hebrewLetter)
						return Break::combine;

					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;

					/* WB11/WB12 */
					if (r == Type::midNum || r == Type::midNumLetter || r == Type::singleQuote)
						return self.fWB12Check();
					break;
				case Type::katakana:
					/* WB13 */
					if (r == Type::katakana)
						return Break::combine;

					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;
					break;
				case Type::midNum:
					/* WB11 */
					if (r == Type::numeric)
						return self.fWB11Check();
					break;
				case Type::midNumLetter:
				case Type::singleQuote:
					/* WB11 */
					if (r == Type::numeric)
						return self.fWB11Check();
					[[fallthrough]];
				case Type::midLetter:
					/* WB7 */
					if (r == Type::aLetterDef || r == Type::aLetterExtendedPictographic || r == Type::hebrewLetter)
						return self.fWB7Check();
					break;
				case Type::doubleQuote:
					/* WB7c */
					if (r == Type::hebrewLetter)
						return self.fWB7cCheck();
					break;
				case Type::extendNumLet:
					/* partial: WB13a */
					if (r == Type::extendNumLet)
						return Break::combine;

					/* WB13b */
					if (r == Type::aLetterDef || r == Type::aLetterExtendedPictographic || r == Type::hebrewLetter || r == Type::numeric || r == Type::katakana)
						return Break::combine;
					break;
				case Type::regionalIndicator:
					/* WB15/WB16 */
					if (r == Type::regionalIndicator && self.fIsLeftRIOdd())
						return Break::combine;
					break;
				default:
					break;
				}

				/* WB999 */
				return Break::separate;
			}
		};
		template <class ItType>
		class WordRandom {
			friend struct detail::WordBreak;
			using Host = detail::WordBreak;
		public:
			using State = void;

		private:
			const ItType& pLeft;
			const ItType& pRight;

		private:
			constexpr WordRandom(const ItType& l, const ItType& r) : pLeft{ l }, pRight{ r } {}

		private:
			Host::Type fGetPrev() const {
				ItType t = pLeft;
				bool firstReached = false;
				do {
					Host::Type type = Host::GetCP(t.get());
					if (type == Host::Type::extend || type == Host::Type::format || type == Host::Type::zwj)
						continue;
					if (firstReached)
						return type;
					firstReached = true;
				} while (t.reverse());
				return Host::Type::other;
			}
			Host::Type fGetNext() const {
				ItType t = pRight;
				while (t.advance()) {
					Host::Type type = Host::GetCP(t.get());
					if (type != Host::Type::extend && type != Host::Type::format && type != Host::Type::zwj)
						return type;
				}
				return Host::Type::other;
			}

		private:
			constexpr Host::Type fSkipWB4(Host::Type l) const {
				ItType t = pLeft;
				while ((l == Host::Type::extend || l == Host::Type::format || l == Host::Type::zwj) && t.reverse())
					l = Host::GetCP(t.get());
				return l;
			}
			constexpr Host::Break fWB6Check() const {
				Host::Type r = fGetNext();
				if (r == Host::Type::aLetterDef || r == Host::Type::aLetterExtendedPictographic || r == Host::Type::hebrewLetter)
					return Host::Break::combine;
				return Host::Break::separate;
			}
			constexpr Host::Break fWB7Check() const {
				Host::Type r = fGetPrev();
				if (r == Host::Type::aLetterDef || r == Host::Type::aLetterExtendedPictographic || r == Host::Type::hebrewLetter)
					return Host::Break::combine;
				return Host::Break::separate;
			}
			constexpr Host::Break fWB7aCheck() const {
				return Host::Break::combine;
			}
			constexpr Host::Break fWB7bCheck() const {
				if (fGetNext() == Host::Type::hebrewLetter)
					return Host::Break::combine;
				return Host::Break::separate;
			}
			constexpr Host::Break fWB7cCheck() const {
				if (fGetPrev() == Host::Type::hebrewLetter)
					return Host::Break::combine;
				return Host::Break::separate;
			}
			constexpr Host::Break fWB11Check() const {
				if (fGetPrev() == Host::Type::numeric)
					return Host::Break::combine;
				return Host::Break::separate;
			}
			constexpr Host::Break fWB12Check() const {
				if (fGetNext() == Host::Type::numeric)
					return Host::Break::combine;
				return Host::Break::separate;
			}
			constexpr bool fIsLeftRIOdd() const {
				ItType t = pLeft;
				size_t count = 0;
				do {
					Host::Type type = Host::GetCP(t.get());
					if (type == Host::Type::regionalIndicator)
						++count;
					else if (type != Host::Type::extend && type != Host::Type::format && type != Host::Type::zwj)
						break;
				} while (t.reverse());
				return ((count & 0x01) != 0);
			}

		public:
			static constexpr cp::BreakMode Resolve(const ItType& lIt, uint32_t lRaw, const ItType& rIt, uint32_t rRaw) {
				Host::Type lType = Host::GetRaw(lRaw), rType = Host::GetRaw(rRaw);
				if (Host::Test(lType, rType, detail::WordRandom<ItType>{ lIt, rIt }) == Host::Break::separate)
					return cp::BreakMode::optional;
				return cp::BreakMode::none;
			}
		};
		template <class RecvType, class PayloadType>
		class WordForward {
			friend struct detail::WordBreak;
			using Host = detail::WordBreak;
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

		private:
			str::detail::LocalBuffer<PayloadType> pCache;
			RecvType pRecv;
			PayloadType pUncertainPayload{};
			Host::Type pLast = Host::Type::other;
			Host::Type pLastActual = Host::Type::other;
			State pState = State::none;
			bool pRICountOdd = false;

		public:
			constexpr WordForward(RecvType&& recv) : pRecv{ std::forward<RecvType>(recv) } {}

		private:
			constexpr Host::Type fSkipWB4(Host::Type) const {
				return pLast;
			}
			constexpr Host::Break fWB6Check() {
				pState = State::wb6;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fWB7Check() const {
				/* never needs to be checked separately (implicitly handled by WB6) */
				return Host::Break::separate;
			}
			constexpr Host::Break fWB7aCheck() {
				pState = State::wb7a;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fWB7bCheck() {
				pState = State::wb7b;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fWB7cCheck() const {
				/* never needs to be checked separately (implicitly handled by WB7b) */
				return Host::Break::separate;
			}
			constexpr Host::Break fWB11Check() const {
				/* never needs to be checked separately (implicitly handled by WB12) */
				return Host::Break::separate;
			}
			constexpr Host::Break fWB12Check() {
				pState = State::wb11;
				return Host::Break::uncertain;
			}
			constexpr bool fIsLeftRIOdd() const {
				return pRICountOdd;
			}

		private:
			constexpr Continue fCheckState(Host::Type right) const {
				/* right will be 'other' for the final iteration and automatically clean it up properly */

				/* check if this is a silent reduction */
				if (right == Host::Type::extend || right == Host::Type::format || right == Host::Type::zwj)
					return Continue::uncertain;

				/* cleanup the state */
				switch (pState) {
				case State::wb6:
					if (right == Host::Type::aLetterDef || right == Host::Type::aLetterExtendedPictographic || right == Host::Type::hebrewLetter)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;
				case State::wb7a:
					if (right == Host::Type::aLetterDef || right == Host::Type::aLetterExtendedPictographic || right == Host::Type::hebrewLetter)
						return Continue::combineIncludingRight;
					return Continue::combineExcludingRight;
				case State::wb7b:
					if (right == Host::Type::hebrewLetter)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;
				case State::wb11:
					if (right == Host::Type::numeric)
						return Continue::combineIncludingRight;
					return Continue::breakBeforeCached;

				case State::none:
					/* to silence warning */
					break;
				}
				return Continue::combineExcludingRight;
			}
			constexpr void fUpdateLeft(Host::Type r) {
				/* update the regionalIndicator counter and update the last-state */
				if (r != Host::Type::extend && r != Host::Type::format && r != Host::Type::zwj) {
					pRICountOdd = (r == Host::Type::regionalIndicator ? !pRICountOdd : false);
					pLast = r;
				}
				pLastActual = r;
			}

		public:
			constexpr void first(uint32_t raw) {
				Host::Type right = Host::GetRaw(raw);

				/* WB1: initialize the last-state */
				pRICountOdd = (right == Host::Type::regionalIndicator ? !pRICountOdd : false);
				pLast = right;
				pLastActual = right;
			}
			constexpr void next(uint32_t raw, const PayloadType& payload) {
				Host::Type right = Host::GetRaw(raw);

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					/* update the state and check if the state can be resolved */
					Continue cont = fCheckState(right);
					if (cont == Continue::uncertain) {
						pCache.push(payload);
						fUpdateLeft(right);
						return;
					}
					pState = State::none;

					/* flush the cached characters */
					pRecv(pUncertainPayload, cont == Continue::breakBeforeCached ? cp::BreakMode::optional : cp::BreakMode::none);
					while (pCache.size() > 0)
						pRecv(pCache.pop(), cp::BreakMode::none);

					/* check if the upcoming element can be consumed as well */
					if (cont == Continue::combineIncludingRight) {
						pRecv(payload, cp::BreakMode::none);
						fUpdateLeft(right);
						return;
					}
				}

				/* check the current values and update the state */
				switch (Host::Test(pLastActual, right, *this)) {
				case Host::Break::combine:
					pRecv(payload, cp::BreakMode::none);
					break;
				case Host::Break::separate:
					pRecv(payload, cp::BreakMode::optional);
					break;
				case Host::Break::uncertain:
					pUncertainPayload = payload;
					break;
				}
				fUpdateLeft(right);
			}
			constexpr void done() {
				/* WB2: check if a cached state needs to be ended */
				if (pState == State::none)
					return;

				/* flush the cached characters */
				pRecv(pUncertainPayload, fCheckState(Host::Type::other) == Continue::breakBeforeCached ? cp::BreakMode::optional : cp::BreakMode::none);
				while (pCache.size() > 0)
					pRecv(pCache.pop(), cp::BreakMode::none);
			}
		};

		struct SentenceBreak {
			static_assert(size_t(detail::gen::SentenceType::_end) == 15, "Only types 0-14 are known by the state-machine");
		public:
			using Type = detail::gen::SentenceType;
			enum class Break : uint8_t {
				separate,
				combine,
				uncertain
			};
			enum class Chain : uint8_t {
				none,
				lowUp,
				aTermLowUp,
				aTerm,
				sTerm,
				aClose,
				sClose,
				aSpace,
				sSpace
			};

		public:
			static constexpr Type GetRaw(uint32_t raw) {
				return static_cast<Type>((raw >> detail::gen::SentenceSegmentationOff) & detail::gen::SegmentationMask);
			}
			static constexpr Type GetCP(char32_t cp) {
				return GetRaw(detail::gen::GetSegmentation(cp));
			}
			template <class SelfType>
			static constexpr Break Test(Type l, Type r, SelfType&& self) {
				/* SB3/SB4 */
				if (l == Type::cr)
					return (r == Type::lf ? Break::combine : Break::separate);
				if (l == Type::lf || l == Type::separator)
					return Break::separate;

				Chain c = self.fGetChainState();
				switch (r) {
				case Type::extend:
				case Type::format:
					/* SB4 */
					return Break::combine;

				case Type::numeric:
					/* SB6 */
					if (c == Chain::aTermLowUp || c == Chain::aTerm)
						return Break::combine;

					/* SB8 */
					if (c == Chain::aClose || c == Chain::aSpace)
						return self.fSB8Check();
					break;
				case Type::upper:
					/* SB7 */
					if (c == Chain::aTermLowUp)
						return Break::combine;
					break;
				case Type::close:
					/* SB8 */
					if (c == Chain::aSpace)
						return self.fSB8Check();

					/* SB9 */
					if (c != Chain::none && c != Chain::lowUp && c != Chain::sSpace)
						return Break::combine;
					break;
				case Type::sContinue:
				case Type::aTerm:
				case Type::sTerm:
					/* SB8a */
					if (c != Chain::none && c != Chain::lowUp)
						return Break::combine;
					break;
				case Type::space:
				case Type::separator:
				case Type::cr:
				case Type::lf:
					/* SB10 */
					if (c != Chain::none && c != Chain::lowUp)
						return Break::combine;
					break;
				case Type::lower:
					/* SB8 */
					if (c == Chain::aTermLowUp || c == Chain::aTerm || c == Chain::aClose || c == Chain::aSpace)
						return Break::combine;
					break;
				case Type::other:
					/* SB8 */
					if (c == Chain::aTermLowUp || c == Chain::aTerm || c == Chain::aClose || c == Chain::aSpace)
						return self.fSB8Check();
					break;
				default:
					break;
				}

				/* SB11 */
				if (c != Chain::none && c != Chain::lowUp)
					return Break::separate;

				/* SB998 */
				return Break::combine;
			}
		};
		template <class ItType>
		class SentenceRandom {
			friend struct detail::SentenceBreak;
			using Host = detail::SentenceBreak;
		public:
			using State = void;

		private:
			const ItType& pLeft;
			const ItType& pRight;

		private:
			constexpr SentenceRandom(const ItType& l, const ItType& r) : pLeft{ l }, pRight{ r } {}

		private:
			constexpr Host::Chain fGetChainState() const {
				ItType t = pLeft;
				bool hasSpace = false, hasClose = false, hasATerm = false;

				do {
					switch (Host::GetCP(t.get())) {
					case Host::Type::lower:
					case Host::Type::upper:
						return (hasATerm ? Host::Chain::aTermLowUp : Host::Chain::lowUp);
					case Host::Type::sTerm:
						if (hasATerm)
							return Host::Chain::aTerm;
						if (hasSpace)
							return Host::Chain::sSpace;
						return (hasClose ? Host::Chain::sClose : Host::Chain::sTerm);
					case Host::Type::aTerm:
						if (hasATerm)
							return Host::Chain::aTerm;
						if (hasSpace)
							return Host::Chain::aSpace;
						if (hasClose)
							return Host::Chain::aClose;
						hasATerm = true;
						break;
					case Host::Type::close:
						if (hasATerm)
							return Host::Chain::aTerm;
						hasClose = true;
						break;
					case Host::Type::space:
						if (hasATerm)
							return Host::Chain::aTerm;
						if (hasClose)
							return Host::Chain::none;
						hasSpace = true;
						break;
					case Host::Type::extend:
					case Host::Type::format:
						break;
					default:
						return (hasATerm ? Host::Chain::aTerm : Host::Chain::none);
					}
				} while (t.reverse());
				return (hasATerm ? Host::Chain::aTerm : Host::Chain::none);
			}
			constexpr Host::Break fSB8Check() const {
				ItType t = pRight;

				while (t.advance()) {
					Host::Type type = Host::GetCP(t.get());
					if (type == Host::Type::lower)
						return Host::Break::combine;
					if (type != Host::Type::other && type != Host::Type::extend && type != Host::Type::format && type != Host::Type::space &&
						type != Host::Type::numeric && type != Host::Type::sContinue && type != Host::Type::close)
						break;
				}
				return Host::Break::separate;
			}

		public:
			static constexpr cp::BreakMode Resolve(const ItType& lIt, uint32_t lRaw, const ItType& rIt, uint32_t rRaw) {
				Host::Type lType = Host::GetRaw(lRaw), rType = Host::GetRaw(rRaw);
				if (Host::Test(lType, rType, detail::SentenceRandom<ItType>{ lIt, rIt }) == Host::Break::separate)
					return cp::BreakMode::optional;
				return cp::BreakMode::none;
			}
		};
		template <class RecvType, class PayloadType>
		class SentenceForward {
			friend struct detail::SentenceBreak;
			using Host = detail::SentenceBreak;
		private:
			struct Cache {
				PayloadType payload{};
				Host::Type type = Host::Type::other;
			};

		private:
			str::detail::LocalBuffer<Cache> pCache;
			PayloadType pUncertainPayload{};
			RecvType pRecv;
			Host::Type pLast = Host::Type::other;
			Host::Chain pChain = Host::Chain::none;
			bool pUncertain = false;

		public:
			constexpr SentenceForward(RecvType&& recv) : pRecv{ std::forward<RecvType>(recv) } {}

		private:
			constexpr Host::Chain fGetChainState() const {
				return pChain;
			}
			constexpr Host::Break fSB8Check() const {
				return Host::Break::uncertain;
			}

		private:
			constexpr Host::Chain fUpdateChain(Host::Type r) const {
				/* check if the character is excluded from modifying the state-machine */
				if (r == Host::Type::extend || r == Host::Type::format)
					return pChain;

				/* update the state-machine based on the next character and current state */
				switch (r) {
				case Host::Type::lower:
				case Host::Type::upper:
					return Host::Chain::lowUp;
				case Host::Type::aTerm:
					if (pChain == Host::Chain::lowUp)
						return Host::Chain::aTermLowUp;
					return Host::Chain::aTerm;
				case Host::Type::sTerm:
					return Host::Chain::sTerm;
				case Host::Type::close:
					if (pChain == Host::Chain::aTerm || pChain == Host::Chain::aTermLowUp || pChain == Host::Chain::aClose)
						return Host::Chain::aClose;
					if (pChain == Host::Chain::sTerm || pChain == Host::Chain::sClose)
						return Host::Chain::sClose;
					break;
				case Host::Type::space:
					if (pChain == Host::Chain::sTerm || pChain == Host::Chain::sClose || pChain == Host::Chain::sSpace)
						return Host::Chain::sSpace;
					else if (pChain != Host::Chain::none)
						return Host::Chain::aSpace;
					break;
				default:
					break;
				}
				return Host::Chain::none;
			}
			constexpr Host::Break fCheckState(Host::Type r) const {
				/* check if a determined end has been encountered */
				if (r == Host::Type::lower)
					return Host::Break::combine;

				/* check if the chain remains uncertain */
				if (r == Host::Type::other || r == Host::Type::extend || r == Host::Type::format || r == Host::Type::space ||
					r == Host::Type::numeric || r == Host::Type::sContinue || r == Host::Type::close)
					return Host::Break::uncertain;
				return Host::Break::separate;
			}
			template <bool IsCleanup>
			constexpr void fProcessQueue(Host::Break _break) {
				/* process the cached characters */
				while (true) {
					/* check if the last state needs to be forcefully aborted or if the result is incomplete */
					if (_break == Host::Break::uncertain) {
						if constexpr (!IsCleanup)
							return;
						_break = Host::Break::separate;
					}
					pRecv(pUncertainPayload, _break == Host::Break::separate ? cp::BreakMode::optional : cp::BreakMode::none);

					/* process the next character */
					if (pCache.size() == 0) {
						pUncertain = false;
						return;
					}
					Host::Type right = pCache.front().type;
					pUncertainPayload = pCache.pop().payload;
					_break = Host::Test(pLast, right, *this);
					pLast = right;
					pChain = fUpdateChain(right);

					/* feed the cached characters to it to try to complete the state */
					for (size_t i = 0; _break == Host::Break::uncertain && i < pCache.size(); ++i)
						_break = fCheckState(pCache.get(i).type);
				}
			}

		public:
			constexpr void first(uint32_t raw) {
				/* SB1: initialize the state */
				pLast = Host::GetRaw(raw);
				pChain = fUpdateChain(pLast);
			}
			constexpr void next(uint32_t raw, const PayloadType& payload) {
				Host::Type right = Host::GetRaw(raw);

				/* fast-path of not being in a look-ahead state */
				if (!pUncertain) {
					Host::Break _break = Host::Test(pLast, right, *this);
					if (_break != Host::Break::uncertain)
						pRecv(payload, _break == Host::Break::separate ? cp::BreakMode::optional : cp::BreakMode::none);
					else {
						pUncertainPayload = payload;
						pUncertain = true;
					}
					pLast = right;
					pChain = fUpdateChain(right);
					return;
				}

				/* add the character to the cache and update the current state */
				pCache.push({ payload, right });
				fProcessQueue<false>(fCheckState(right));
			}
			constexpr void done() {
				/* SB2: clear the cache and abort the current cached state */
				if (pUncertain)
					fProcessQueue<true>(Host::Break::separate);
			}
		};

		struct LineBreak {
			static_assert(size_t(detail::gen::LineType::_end) == 49, "Only types 0-48 are known by the state-machine");
		public:
			using Type = detail::gen::LineType;
			enum class Break : uint8_t {
				optional,
				mandatory,
				combine,
				uncertain
			};

		public:
			static constexpr std::pair<Type, bool> GetRaw(uint32_t raw) {
				Type type = static_cast<Type>((raw >> detail::gen::LineSegmentationOff) & detail::gen::SegmentationMask);
				bool fwh = ((raw & (1 << detail::gen::LineFWHAsianWidthTest)) != 0);
				return { type, fwh };
			}
			static constexpr std::pair<Type, bool> GetCP(char32_t cp) {
				return GetRaw(detail::gen::GetSegmentation(cp));
			}
			template <class SelfType>
			static constexpr Break Test(Type l, bool lFWH, Type r, bool rFWH, SelfType&& self) {
				/* LB4/LB5 */
				if (l == Type::cr)
					return (r == Type::lf ? Break::combine : Break::mandatory);
				if (l == Type::bk || l == Type::lf || l == Type::nl)
					return Break::mandatory;

				/* LB6/LB7 */
				if (r == Type::bk || r == Type::cr || r == Type::lf || r == Type::nl || r == Type::zw || r == Type::sp)
					return Break::combine;

				/* handle all space-related operations */
				if (l == Type::sp) {
					/* LB7 */
					if (r == Type::sp || r == Type::zw)
						return Break::combine;

					std::pair<Type, bool> _l = self.fSkipSpaceAndLB9(l, lFWH);
					l = _l.first; lFWH = _l.second;

					/* LB8 */
					if (l == Type::zw)
						return Break::optional;

					/* LB15c/LB15d */
					if (r == Type::is)
						return self.fLB15cCheck();

					/* LB11/LB13/LB14 */
					if (r == Type::wj || r == Type::cl || r == Type::cp || r == Type::ex || r == Type::sy)
						return Break::combine;

					/* LB14 */
					if (l == Type::op)
						return Break::combine;

					/* LB15a */
					if (l == Type::quPi)
						return self.fLB15aCheck();

					/* LB15b */
					if (r == Type::quPf)
						return self.fLB15bCheck();

					/* LB16 */
					if (r == Type::ns && (l == Type::cl || l == Type::cp))
						return Break::combine;

					/* LB17 */
					if (l == Type::b2 && r == Type::b2)
						return Break::combine;

					/* LB18 */
					return Break::optional;
				}

				/* LB8 */
				if (l == Type::zw)
					return Break::optional;

				/* LB8a */
				if (l == Type::zwj)
					return Break::combine;

				std::pair<Type, bool> _l = self.fSkipSpaceAndLB9(l, lFWH);
				l = _l.first; lFWH = _l.second;
				switch (r) {
				case Type::cm:
				case Type::zwj:
					/* LB9 */
					return Break::combine;
				case Type::wj:
					/* LB11 */
					return Break::combine;
				case Type::gl:
					/* LB12a */
					if (l != Type::ba && l != Type::hh && l != Type::hy)
						return Break::combine;
					break;
				case Type::cl:
				case Type::cp:
				case Type::ex:
				case Type::is:
				case Type::sy:
					/* LB13 */
					return Break::combine;
				case Type::b2:
					/* LB17 */
					if (l == Type::b2)
						return Break::combine;
					break;
				case Type::nu:
					/* LB25 */
					if (l == Type::po || l == Type::pr || l == Type::hy || l == Type::is)
						return Break::combine;
					break;
				case Type::quNoPiPf:
				case Type::quPf:
					/* LB19 */
					return Break::combine;
				case Type::quPi:
					/* LB19a */
					if (!lFWH)
						return Break::combine;
					return self.fLB19aCheckRight();
				case Type::cb:
					/* LB11/LB12/LB14 */
					if (l == Type::wj || l == Type::gl || l == Type::op)
						return Break::combine;

					/* LB19 */
					if (l == Type::quNoPiPf || l == Type::quPi)
						return Break::combine;

					/* LB19a */
					if (l == Type::quPf) {
						if (!rFWH)
							return Break::combine;
						return self.fLB19aCheckLeft();
					}

					/* LB20 */
					return Break::optional;
				case Type::ns:
					/* LB16 */
					if (l == Type::cl || l == Type::cp)
						return Break::combine;
					[[fallthrough]];
				case Type::ba:
				case Type::hh:
				case Type::hy:
				case Type::in:
					/* LB11/LB12/LB14 */
					if (l == Type::wj || l == Type::gl || l == Type::op)
						return Break::combine;

					/* LB20 */
					if (l == Type::cb)
						return Break::optional;

					/* LB21/LB22 */
					return Break::combine;
				case Type::hl:
				case Type::alDef:
				case Type::alCnPict:
				case Type::alDotCircle:
					/* LB20a */
					if (l == Type::hy || l == Type::hh)
						return self.fLB20a21aCheck(true, r != Type::hl);
					break;
				default:
					break;
				}

				switch (l) {
				case Type::wj:
					/* LB11 */
					return Break::combine;
				case Type::gl:
					/* LB12 */
					return Break::combine;
				case Type::op:
					/* LB14 */
					return Break::combine;
				case Type::quNoPiPf:
				case Type::quPi:
					/* LB19 */
					return Break::combine;
				case Type::quPf:
					/* LB19a */
					if (!rFWH)
						return Break::combine;
					return self.fLB19aCheckLeft();
				case Type::cb:
					/* LB20 */
					return Break::optional;
				case Type::bb:
					/* LB21 */
					return Break::combine;
				case Type::hy:
					/* LB25 */
					if (r == Type::nu)
						return Break::combine;
					[[fallthrough]];
				case Type::hh:
					/* LB21a */
					if (r != Type::hl)
						return self.fLB21aCheck();
					break;
				case Type::is:
					/* LB29 */
					if (r == Type::alDef || r == Type::alCnPict || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;
					[[fallthrough]];
				case Type::sy:
					/* LB21b */
					if (l == Type::sy && r == Type::hl)
						return Break::combine;

					/* LB25 */
					if (r == Type::nu || r == Type::pr || r == Type::po)
						return self.fLB25CheckLeft();
					break;
				case Type::alDotCircle:
					/* LB28a */
					if (r == Type::vf || r == Type::vi)
						return Break::combine;

					/* LB28a */
					if (r == Type::ak || r == Type::as)
						return self.fLB28a3Check();
					[[fallthrough]];
				case Type::alCnPict:
					/* LB30b */
					if (l == Type::alCnPict && r == Type::em)
						return Break::combine;
					[[fallthrough]];
				case Type::alDef:
				case Type::cm:
				case Type::zwj:
				case Type::hl:
					/* LB23 */
					if (r == Type::nu)
						return Break::combine;

					/* LB24 */
					if (r == Type::pr || r == Type::po)
						return Break::combine;

					/* LB28 */
					if (r == Type::alDef || r == Type::alCnPict || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;

					/* LB30 */
					if (r == Type::op && !rFWH)
						return Break::combine;
					break;
				case Type::nu:
					/* LB23 */
					if (r == Type::alDef || r == Type::alCnPict || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;

					/* LB25 */
					if (r == Type::nu || r == Type::pr || r == Type::po)
						return Break::combine;

					/* LB30 */
					if (r == Type::op && !rFWH)
						return Break::combine;
					break;
				case Type::pr:
					/* LB23a */
					if (r == Type::idDef || r == Type::idCnPict || r == Type::eb || r == Type::em)
						return Break::combine;

					/* LB27 */
					if (r == Type::jl || r == Type::jv || r == Type::jt || r == Type::h2 || r == Type::h3)
						return Break::combine;
					[[fallthrough]];
				case Type::po:
					/* LB24 */
					if (r == Type::alDef || r == Type::alCnPict || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;

					/* LB25 */
					if (r == Type::nu)
						return Break::combine;
					if (r == Type::op || r == Type::hy)
						return self.fLB25CheckRight();
					break;
				case Type::jl:
					/* LB26 */
					if (r == Type::jl || r == Type::jv || r == Type::h2 || r == Type::h3)
						return Break::combine;

					/* LB27 */
					if (r == Type::po)
						return Break::combine;
					break;
				case Type::jv:
				case Type::h2:
					/* LB26 */
					if (r == Type::jv)
						return Break::combine;
					[[fallthrough]];
				case Type::jt:
				case Type::h3:
					/* LB26 */
					if (r == Type::jt)
						return Break::combine;

					/* LB27 */
					if (r == Type::po)
						return Break::combine;
					break;
				case Type::eb:
				case Type::idCnPict:
					/* LB30b */
					if (r == Type::em)
						return Break::combine;
					[[fallthrough]];
				case Type::em:
				case Type::idDef:
					/* LB23a */
					if (r == Type::po)
						return Break::combine;
					break;
				case Type::ap:
					/* LB28a */
					if (r == Type::ak || r == Type::alDotCircle || r == Type::as)
						return Break::combine;
					break;
				case Type::ak:
				case Type::as:
					/* LB28a */
					if (r == Type::vf || r == Type::vi)
						return Break::combine;

					/* LB28a */
					if (r == Type::ak || r == Type::alDotCircle || r == Type::as)
						return self.fLB28a3Check();
					break;
				case Type::vi:
					/* LB28a */
					if (r == Type::ak || r == Type::alDotCircle)
						return self.fLB28a2Check();
					break;
				case Type::cp:
					/* LB30 */
					if (!lFWH && (r == Type::nu || r == Type::alDef || r == Type::alCnPict || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl))
						return Break::combine;
					[[fallthrough]];
				case Type::cl:
					/* LB25 */
					if (r == Type::pr || r == Type::po)
						return self.fLB25CheckLeft();
					break;
				case Type::ri:
					/* LB30a */
					if (r == Type::ri && self.fIsLeftRIOdd())
						return Break::combine;
					break;
				default:
					break;
				}

				/* LB31 */
				return Break::optional;
			}
		};
		template <class ItType>
		class PrimitiveLineRandom {
			friend struct detail::LineBreak;
			using Host = detail::LineBreak;
		public:
			using State = void;

		private:
			const ItType& pLeft;
			const ItType& pRight;

		private:
			constexpr PrimitiveLineRandom(const ItType& l, const ItType& r) : pLeft{ l }, pRight{ r } {}

		private:
			constexpr std::pair<ItType, std::pair<Host::Type, bool>> fSkipIntermediate(const ItType& it, std::pair<Host::Type, bool> type, bool skipSpaces) const {
				ItType t = it;

				while (true) {
					if (type.first == Host::Type::sp && skipSpaces) {
						if (!t.reverse())
							break;
						type = Host::GetCP(t.get());
					}
					else if (type.first != Host::Type::cm && type.first != Host::Type::zwj)
						break;
					else {
						ItType tIt = t;
						if (!tIt.reverse())
							break;
						std::pair<Host::Type, bool> temp = Host::GetCP(tIt.get());
						if (temp.first == Host::Type::bk || temp.first == Host::Type::cr || temp.first == Host::Type::lf || temp.first == Host::Type::nl || temp.first == Host::Type::sp || temp.first == Host::Type::zw)
							break;
						type = temp;
						t = tIt;
					}
				}
				return { t, type };
			}
			constexpr std::pair<std::pair<Host::Type, bool>, bool> fGetPrev() const {
				auto [it, type] = fSkipIntermediate(pLeft, Host::GetCP(pLeft.get()), true);
				if (!it.reverse())
					return { type, false };
				return { fSkipIntermediate(it, Host::GetCP(it.get()), false).second, true };
			}
			constexpr std::pair<std::pair<Host::Type, bool>, bool> fGetNext() const {
				ItType t = pRight;
				while (t.advance()) {
					std::pair<Host::Type, bool> type = Host::GetCP(t.get());
					if (type.first != Host::Type::cm && type.first != Host::Type::zwj)
						return { type, true };
				}
				return { { Host::Type::_end, false }, false };
			}

		private:
			constexpr std::pair<Host::Type, bool> fSkipSpaceAndLB9(Host::Type l, bool lFWH) const {
				return fSkipIntermediate(pLeft, { l, lFWH }, true).second;
			}
			constexpr Host::Break fLB15aCheck() const {
				auto [type, exists] = fGetPrev();
				if (!exists || type.first == Host::Type::bk || type.first == Host::Type::cr || type.first == Host::Type::lf || type.first == Host::Type::nl ||
					type.first == Host::Type::op || type.first == Host::Type::quNoPiPf || type.first == Host::Type::quPf ||
					type.first == Host::Type::quPi || type.first == Host::Type::gl || type.first == Host::Type::sp || type.first == Host::Type::zw)
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB15bCheck() const {
				auto [type, exists] = fGetNext();
				if (type.first == Host::Type::sp || type.first == Host::Type::gl || type.first == Host::Type::wj || type.first == Host::Type::cl || type.first == Host::Type::quNoPiPf ||
					type.first == Host::Type::quPf || type.first == Host::Type::quPi || type.first == Host::Type::cp || type.first == Host::Type::ex ||
					type.first == Host::Type::is || type.first == Host::Type::sy || type.first == Host::Type::bk || type.first == Host::Type::cr || type.first == Host::Type::lf ||
					type.first == Host::Type::nl || type.first == Host::Type::zw || !exists)
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB15cCheck() const {
				auto [type, exists] = fGetNext();
				return ((exists && type.first == Host::Type::nu) ? Host::Break::optional : Host::Break::combine);
			}
			constexpr Host::Break fLB19aCheckLeft() const {
				auto [type, exists] = fGetPrev();
				return ((exists && type.second) ? Host::Break::optional : Host::Break::combine);
			}
			constexpr Host::Break fLB19aCheckRight() const {
				auto [type, exists] = fGetNext();
				return ((!exists || !type.second) ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB20a21aCheck(bool lb20a, bool lb21a) const {
				auto [type, exists] = fGetPrev();
				if (lb20a) {
					if (!exists || type.first == Host::Type::bk || type.first == Host::Type::cr || type.first == Host::Type::lf || type.first == Host::Type::nl ||
						type.first == Host::Type::sp || type.first == Host::Type::zw || type.first == Host::Type::cb || type.first == Host::Type::gl)
						return Host::Break::combine;
				}
				if (lb21a && type.first == Host::Type::hl)
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB21aCheck() const {
				auto [type, exists] = fGetPrev();
				return ((exists && type.first == Host::Type::hl) ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB25CheckLeft() const {
				auto [type, exists] = fGetPrev();
				return ((exists && type.first == Host::Type::nu) ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB25CheckRight() const {
				return (fGetNext().first.first == Host::Type::nu ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB28a2Check() const {
				auto [type, exists] = fGetPrev();
				if (exists && (type.first == Host::Type::ak || type.first == Host::Type::alDotCircle || type.first == Host::Type::as))
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB28a3Check() const {
				return (fGetNext().first.first == Host::Type::vf ? Host::Break::combine : Host::Break::optional);
			}
			constexpr bool fIsLeftRIOdd() const {
				ItType t = pLeft;
				size_t count = 0;
				do {
					std::pair<Host::Type, bool> type = Host::GetCP(t.get());
					if (type.first == Host::Type::ri)
						++count;
					else if (type.first != Host::Type::cm && type.first != Host::Type::zwj)
						break;
				} while (t.reverse());
				return ((count & 0x01) != 0);
			}

		public:
			static constexpr cp::BreakMode Resolve(const ItType& lIt, uint32_t lRaw, const ItType& rIt, uint32_t rRaw) {
				std::pair<Host::Type, bool> lType = Host::GetRaw(lRaw), rType = Host::GetRaw(rRaw);
				Host::Break _break = Host::Test(lType.first, lType.second, rType.first, rType.second, detail::PrimitiveLineRandom<ItType>{ lIt, rIt });
				if (_break == Host::Break::combine)
					return cp::BreakMode::none;
				else if (_break == Host::Break::mandatory)
					return cp::BreakMode::mandatory;
				return cp::BreakMode::optional;
			}
		};
		template <class RecvType, class PayloadType>
		class PrimitiveLineForward {
			friend struct detail::LineBreak;
			using Host = detail::LineBreak;
		private:
			enum class State : uint8_t {
				none,
				lb15b,
				lb15c,
				lb19a,
				lb25,
				lb28a3
			};
			enum class Chain : uint8_t {
				none,
				lb15aMatch,
				lb20aMatch,
				lb21aMatch,
				lb25Nu,
				lb25PrPo,
				lb28a2Match
			};
			struct Cache {
				PayloadType payload{};
				cp::BreakMode mode = cp::BreakMode::none;
			};

		private:
			str::detail::LocalBuffer<Cache> pCache;
			RecvType pRecv;
			PayloadType pUncertainPayload{};
			std::pair<Host::Type, bool> pLast = { Host::Type::_end, false };
			std::pair<Host::Type, bool> pActual = { Host::Type::_end, false };
			Chain pChain = Chain::none;
			State pState = State::none;
			bool pRICountOdd = false;
			bool pNotFWH = false;
			cp::BreakMode pCombineValue = cp::BreakMode::none;

		public:
			constexpr PrimitiveLineForward(RecvType&& recv, bool emergencyAware) : pRecv{ std::forward<RecvType>(recv) } {
				if (emergencyAware)
					pCombineValue = cp::BreakMode::emergency;
			}

		private:
			constexpr std::pair<Host::Type, bool> fSkipSpaceAndLB9(Host::Type, bool) const {
				return pLast;
			}
			constexpr Host::Break fLB15aCheck() const {
				return (pChain == Chain::lb15aMatch ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB15bCheck() {
				pState = State::lb15b;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fLB15cCheck() {
				pState = State::lb15c;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fLB19aCheckLeft() const {
				return (pNotFWH ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB19aCheckRight() {
				pState = State::lb19a;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fLB20a21aCheck(bool lb20a, bool lb21a) const {
				if (pChain == Chain::lb20aMatch && lb20a)
					return Host::Break::combine;
				if (pChain == Chain::lb21aMatch && lb21a)
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB21aCheck() const {
				return (pChain == Chain::lb21aMatch ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB25CheckLeft() const {
				return ((pChain == Chain::lb25Nu || pChain == Chain::lb25PrPo) ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB25CheckRight() {
				pState = State::lb25;
				return Host::Break::uncertain;
			}
			constexpr Host::Break fLB28a2Check() const {
				return (pChain == Chain::lb28a2Match ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB28a3Check() {
				pState = State::lb28a3;
				return Host::Break::uncertain;
			}
			constexpr bool fIsLeftRIOdd() const {
				return pRICountOdd;
			}

		private:
			constexpr void fUpdateLeft(std::pair<Host::Type, bool> r) {
				/* update the chain state-machine */
				switch (r.first) {
				case Host::Type::quPi:
					if (pLast.first == Host::Type::bk || pLast.first == Host::Type::cr || pLast.first == Host::Type::lf || pLast.first == Host::Type::nl ||
						pLast.first == Host::Type::op || pLast.first == Host::Type::quNoPiPf || pLast.first == Host::Type::quPf ||
						pLast.first == Host::Type::quPi || pLast.first == Host::Type::gl || pLast.first == Host::Type::zw || pActual.first == Host::Type::sp)
						pChain = Chain::lb15aMatch;
					else
						pChain = Chain::none;
					break;
				case Host::Type::nu:
					pChain = Chain::lb25Nu;
					break;
				case Host::Type::sy:
				case Host::Type::is:
					if (pChain != Chain::lb25Nu)
						pChain = Chain::none;
					break;
				case Host::Type::cl:
				case Host::Type::cp:
					pChain = (pChain == Chain::lb25Nu ? Chain::lb25PrPo : Chain::none);
					break;
				case Host::Type::hy:
				case Host::Type::ba:
				case Host::Type::hh:
					if (r.first != Host::Type::ba && (pLast.first == Host::Type::bk || pLast.first == Host::Type::cr || pLast.first == Host::Type::lf || pLast.first == Host::Type::nl ||
						pLast.first == Host::Type::zw || pLast.first == Host::Type::cb || pLast.first == Host::Type::gl || pActual.first == Host::Type::sp))
						pChain = Chain::lb20aMatch;
					else if ((r.first == Host::Type::hy || !r.second) && pLast.first == Host::Type::hl)
						pChain = Chain::lb21aMatch;
					else
						pChain = Chain::none;
					break;
				case Host::Type::vi:
					if (pLast.first == Host::Type::ak || pLast.first == Host::Type::alDotCircle || pLast.first == Host::Type::as)
						pChain = Chain::lb28a2Match;
					else
						pChain = Chain::none;
					break;
				case Host::Type::cm:
				case Host::Type::zwj:
					break;
				case Host::Type::sp:
					if (pChain != Chain::lb15aMatch)
						pChain = Chain::none;
					break;
				default:
					pChain = Chain::none;
					break;
				}

				/* check if the value before the last space can be updated */
				pActual = r;
				if (r.first == Host::Type::sp) {
					pRICountOdd = false;
					return;
				}

				/* update the LB19a fwh-state */
				pNotFWH = !pLast.second;

				/* skip the invisible types and update the last value and ri-parity */
				if ((r.first == Host::Type::cm || r.first == Host::Type::zwj) && pLast.first != Host::Type::bk && pLast.first != Host::Type::cr &&
					pLast.first != Host::Type::lf && pLast.first != Host::Type::nl && pLast.first != Host::Type::sp && pLast.first != Host::Type::zw)
					return;
				pRICountOdd = (r.first == Host::Type::ri ? !pRICountOdd : false);
				pLast = r;
			}
			constexpr Host::Break fCheckState(std::pair<Host::Type, bool> r) const {
				/* check if the character does not close the state */
				if (r.first == Host::Type::cm || r.first == Host::Type::zwj)
					return Host::Break::uncertain;

				/* LB15b with certainty that it was followed by space, and would therefore otherwise be an optional break */
				if (pState == State::lb15b) {
					if (r.first == Host::Type::sp || r.first == Host::Type::gl || r.first == Host::Type::wj || r.first == Host::Type::cl || r.first == Host::Type::quNoPiPf ||
						r.first == Host::Type::quPf || r.first == Host::Type::quPi || r.first == Host::Type::cp || r.first == Host::Type::ex || r.first == Host::Type::is ||
						r.first == Host::Type::sy || r.first == Host::Type::bk || r.first == Host::Type::cr || r.first == Host::Type::lf || r.first == Host::Type::nl || r.first == Host::Type::zw)
						return Host::Break::combine;
					return Host::Break::optional;
				}

				/* LB15c */
				if (pState == State::lb15c) {
					if (r.first == Host::Type::nu)
						return Host::Break::optional;
					return Host::Break::combine;
				}

				/* LB19a */
				if (pState == State::lb19a) {
					if (!r.second)
						return Host::Break::combine;
					return Host::Break::optional;
				}

				/* LB25 */
				if (pState == State::lb25) {
					if (r.first == Host::Type::nu)
						return Host::Break::combine;
					return Host::Break::optional;
				}

				/* LB28a */
				if (r.first == Host::Type::vf)
					return Host::Break::combine;
				return Host::Break::optional;
			}

		private:
			constexpr void fPopQueue(bool combine) {
				/* post all cached characters */
				pRecv(pUncertainPayload, (combine ? pCombineValue : cp::BreakMode::optional));
				while (pCache.size() > 0) {
					auto [payload, mode] = pCache.pop();
					pRecv(payload, mode);
				}
			}

		public:
			constexpr void first(uint32_t raw) {
				std::pair<Host::Type, bool> right = Host::GetRaw(raw);

				/* LB2: initialize the state */
				pLast = (pActual = right);
				pRICountOdd = (right.first == Host::Type::ri);
				if (right.first == Host::Type::nu)
					pChain = Chain::lb25Nu;
				else if (right.first == Host::Type::quPi)
					pChain = Chain::lb15aMatch;
				else if (right.first == Host::Type::hh || right.first == Host::Type::hy)
					pChain = Chain::lb20aMatch;
				pNotFWH = true;
			}
			constexpr void next(uint32_t raw, const PayloadType& payload, bool withinGrapheme) {
				std::pair<Host::Type, bool> right = Host::GetRaw(raw);

				/* check if this lies within a grapheme and is therefore a true combine-value and either
				*	write it out directly, or cache it to be processed once the cache is popped */
				if (withinGrapheme) {
					if (pState == State::none)
						pRecv(payload, cp::BreakMode::none);
					else
						pCache.push({ payload, cp::BreakMode::none });
					return;
				}

				/* check if a current state for longer chains has been entered and handle it */
				if (pState != State::none) {
					Host::Break _break = fCheckState(right);
					if (_break == Host::Break::uncertain) {
						pCache.push({ payload, pCombineValue });
						fUpdateLeft(right);
						return;
					}
					pState = State::none;

					/* clear all cached characters */
					fPopQueue(_break == Host::Break::combine);
				}

				/* check the current values and update the state */
				switch (Host::Test(pActual.first, pActual.second, right.first, right.second, *this)) {
				case Host::Break::combine:
					pRecv(payload, pCombineValue);
					break;
				case Host::Break::optional:
					pRecv(payload, cp::BreakMode::optional);
					break;
				case Host::Break::mandatory:
					pRecv(payload, cp::BreakMode::mandatory);
					break;
				case Host::Break::uncertain:
					pUncertainPayload = payload;
					break;
				}
				fUpdateLeft(right);
			}
			constexpr void done() {
				/* LB3: cleanup the final cached state */
				if (pState != State::none)
					fPopQueue(pState == State::lb15b || pState == State::lb15c || pState == State::lb19a);
			}
		};
		template <class ItType>
		class LineRandom {
		private:
			struct Iterator {
				detail::BreakIterator<ItType, detail::GraphemeRandom> iterator;
				constexpr Iterator(const ItType& it) : iterator{ it, 0 } {}
				constexpr char32_t next() {
					char32_t cp = iterator.get().get();
					iterator.next();
					return cp;
				}
				constexpr char32_t prev() {
					char32_t cp = iterator.get().get();
					iterator.prev();
					return cp;
				}
				constexpr bool advance() {
					return (iterator.next() != cp::BreakMode::edge);
				}
				constexpr bool reverse() {
					return (iterator.prev() != cp::BreakMode::edge);
				}
				constexpr char32_t get() const {
					return iterator.get().get();
				}
			};

		public:
			struct State {
				bool emergency = false;
				bool graphemes = false;
				constexpr State(bool emergencyBreak = true, bool graphemeAware = true) : emergency{ graphemeAware && emergencyBreak }, graphemes{ graphemeAware } {}
			};

		public:
			static constexpr cp::BreakMode Resolve(ItType lIt, uint32_t lRaw, const ItType& rIt, uint32_t rRaw, const State& state) {
				/* check if this can immediately be passed to the line-resolver */
				if (!state.graphemes)
					return detail::PrimitiveLineRandom<ItType>::Resolve(lIt, lRaw, rIt, rRaw);

				/* check if the two iterators lie directly in a single grapheme cluster, in which case no line-checking needs to be done */
				if (detail::GraphemeRandom<ItType>::Resolve(lIt, lRaw, rIt, rRaw) == cp::BreakMode::none)
					return cp::BreakMode::none;

				/* find the start of the left grapheme cluster */
				while (true) {
					ItType it = lIt;
					if (!it.reverse())
						break;
					uint32_t raw = detail::gen::GetSegmentation(it.get());
					if (detail::GraphemeRandom<ItType>::Resolve(it, raw, lIt, lRaw) != cp::BreakMode::none)
						break;
					lIt = it;
					lRaw = raw;
				}

				/* evaluate the result of the two grapheme clusters using the separate grapheme-iterators */
				cp::BreakMode mode = detail::PrimitiveLineRandom<Iterator>::Resolve(Iterator{ lIt }, lRaw, Iterator{ rIt }, rRaw);
				if (state.emergency && mode == cp::BreakMode::none)
					return cp::BreakMode::emergency;
				return mode;
			}
		};
		template <class RecvType, class PayloadType>
		class LineForward {
		private:
			struct GrPayload {
				uint32_t raw = 0;
				PayloadType payload{};
			};
			struct GrLambda {
				LineForward<RecvType, PayloadType>& self;
				constexpr GrLambda(LineForward<RecvType, PayloadType>& s) : self{ s } {}
				constexpr void operator()(const GrPayload& payload, cp::BreakMode mode) {
					self.pLine.next(payload.raw, payload.payload, (mode == cp::BreakMode::none));
				}
			};
			struct LnLambda {
				LineForward<RecvType, PayloadType>& self;
				constexpr LnLambda(LineForward<RecvType, PayloadType>& s) : self{ s } {}
				constexpr void operator()(const PayloadType& payload, cp::BreakMode mode) {
					self.pRecv(payload, mode);
				}
			};

		private:
			detail::GraphemeForward<GrLambda, GrPayload> pGrapheme;
			detail::PrimitiveLineForward<LnLambda, PayloadType> pLine;
			RecvType pRecv;
			bool pGraphemes = false;

		public:
			constexpr LineForward(RecvType&& recv, bool emergencyBreak, bool graphemeAware) : pGrapheme{ GrLambda{ *this } },
				pLine{ LnLambda{ *this }, graphemeAware && emergencyBreak }, pRecv{ std::forward<RecvType>(recv) }, pGraphemes{ graphemeAware } {
			}

		public:
			constexpr void first(uint32_t raw) {
				if (pGraphemes)
					pGrapheme.first(raw);
				pLine.first(raw);
			}
			constexpr void next(uint32_t raw, const PayloadType& payload) {
				if (pGraphemes)
					pGrapheme.next(raw, { raw, payload });
				else
					pLine.next(raw, payload, false);
			}
			constexpr void done() {
				if (pGraphemes)
					pGrapheme.done();
				pLine.done();
			}
		};
	}

	/* create a [str::IsCollector<char32_t, size_t>], which feeds the 'grapheme-break-before' attribute for every
	*		codepoint except for the first codepoint to the receiver (will be produced in-order)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	For receiver(size_t, cp::BreakMode): insert corresponding break before codepoint at given index (only none/optional) */
	class GraphemeBreak {
	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		using Type = detail::BreakSingle<RecvType, detail::GraphemeForward>;

	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv) };
		}
	};

	/* create a [str::IsCollector<char32_t, size_t>], which splits the stream into ranges of grapheme-clusters
	*		and writes them to the receiver (will be produced in-order; no output if string is empty)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	For receiver(cp::Range): range of a single grapheme-cluster (edge for first range, else optional) */
	class GraphemeRanges {
	public:
		template <str::IsReceiver<cp::Range> RecvType>
		using Type = detail::BreakRanges<RecvType, detail::GraphemeForward>;

	public:
		template <str::IsReceiver<cp::Range> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv) };
		}
	};

	/* iterator which allows iteration over codepoints and finding the corresponding grapheme breaks between two codepoints, as well as query the corresponding codepoint-iterators
	*	Less efficient than cp::GraphemeBreak/cp::GraphemeRanges; Guaranteed by Unicode to not break grapheme-clusters
	*	Will advance the codepoint (except for direction-changes and construction) and return the break-type between it and the next codepoint in the given direction (will result in edge, optional, none)
	*	For invalid iterators, BreakMode::edge will be returned at all times */
	template <str::IsIterator ItType>
	class GraphemeIterator : public detail::BreakIterator<ItType, detail::GraphemeRandom> {
	public:
		constexpr GraphemeIterator(const ItType& it) : detail::BreakIterator<ItType, detail::GraphemeRandom>{ it, 0 } {}
	};


	/* create a [str::IsCollector<char32_t, size_t>], which feeds the 'word-break-before' attribute for every
	*		codepoint except for the first codepoint to the receiver (will be produced in-order)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	For receiver(size_t, cp::BreakMode): insert corresponding break before codepoint at given index (only none/optional) */
	class WordBreak {
	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		using Type = detail::BreakSingle<RecvType, detail::WordForward>;

	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv) };
		}
	};

	/* create a [str::IsCollector<char32_t, size_t>], which splits the stream into ranges of words
	*		and writes them to the receiver (will be produced in-order; no output if string is empty)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	For receiver(cp::Range): range of a single word (edge for first range, else optional) */
	class WordRanges {
	public:
		template <str::IsReceiver<cp::Range> RecvType>
		using Type = detail::BreakRanges<RecvType, detail::WordForward>;

	public:
		template <str::IsReceiver<cp::Range> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv) };
		}
	};

	/* iterator which allows iteration over codepoints and finding the corresponding word breaks between two codepoints, as well as query the corresponding codepoint-iterators
	*	Less efficient than cp::WordBreak/cp::WordRanges; Guaranteed by Unicode to not break grapheme-clusters
	*	Will advance the codepoint (except for direction-changes and construction) and return the break-type between it and the next codepoint in the given direction (will result in edge, optional, none)
	*	For invalid iterators, BreakMode::edge will be returned at all times */
	template <str::IsIterator ItType>
	class WordIterator : public detail::BreakIterator<ItType, detail::WordRandom> {
	public:
		constexpr WordIterator(const ItType& it) : detail::BreakIterator<ItType, detail::WordRandom>{ it, 0 } {}
	};


	/* create a [str::IsCollector<char32_t, size_t>], which feeds the 'sentence-break-before' attribute for every
	*		codepoint except for the first codepoint to the receiver (will be produced in-order)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	For receiver(size_t, cp::BreakMode): insert corresponding break before codepoint at given index (only none/optional) */
	class SentenceBreak {
	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		using Type = detail::BreakSingle<RecvType, detail::SentenceForward>;

	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv) };
		}
	};

	/* create a [str::IsCollector<char32_t, size_t>], which splits the stream into ranges of sentence-clusters
	*		and writes them to the receiver (will be produced in-order; no output if string is empty)
	*	Guaranteed by Unicode to not break grapheme-clusters
	*	For receiver(cp::Range): range of a single sentence (edge for first range, else optional) */
	class SentenceRanges {
	public:
		template <str::IsReceiver<cp::Range> RecvType>
		using Type = detail::BreakRanges<RecvType, detail::SentenceForward>;

	public:
		template <str::IsReceiver<cp::Range> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv) };
		}
	};

	/* iterator which allows iteration over codepoints and finding the corresponding sentence breaks between two codepoints, as well as query the corresponding codepoint-iterators
	*	Less efficient than cp::SentenceBreak/cp::SentenceRanges; Guaranteed by Unicode to not break grapheme-clusters
	*	Will advance the codepoint (except for direction-changes and construction) and return the break-type between it and the next codepoint in the given direction (will result in edge, optional, none)
	*	For invalid iterators, BreakMode::edge will be returned at all times */
	template <str::IsIterator ItType>
	class SentenceIterator : public detail::BreakIterator<ItType, detail::SentenceRandom> {
	public:
		constexpr SentenceIterator(const ItType& it) : detail::BreakIterator<ItType, detail::SentenceRandom>{ it, 0 } {}
	};


	/* create a [str::IsCollector<char32_t, size_t>], which feeds the 'line-break-before' attribute for every
	*		codepoint except for the first codepoint to the receiver (will be produced in-order)
	*	Additionally specify whether to produce emergency-breaks (based on grapheme-clusters), or ignore grapheme-boundaries entirely and perform default line-breaking
	*	For receiver(size_t, cp::BreakMode): insert corresponding break before codepoint at given index (all except edge) */
	class LineBreak {
	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		using Type = detail::BreakSingle<RecvType, detail::LineForward>;

	private:
		bool pEmergency = false;
		bool pGraphemes = false;

	public:
		constexpr LineBreak(bool emergencyBreak = true, bool graphemeAware = true) : pEmergency{ emergencyBreak }, pGraphemes{ graphemeAware } {}

	public:
		template <str::IsReceiver<size_t, cp::BreakMode> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv), pEmergency, pGraphemes };
		}
	};

	/* create a [str::IsCollector<char32_t, size_t>], which splits the stream into ranges of line-clusters
	*		and writes them to the receiver (will be produced in-order; no output if string is empty)
	*	Additionally specify whether to produce emergency-breaks (based on grapheme-clusters), or ignore grapheme-boundaries entirely and perform default line-breaking
	*	For receiver(cp::Range): range of a single line and corresponding behavior before the range (edge for first range, else emergency/optional/mandatory, none if single step) */
	class LineRanges {
	public:
		template <str::IsReceiver<cp::Range> RecvType>
		using Type = detail::BreakRanges<RecvType, detail::LineForward>;

	private:
		bool pEmergency = false;
		bool pGraphemes = false;

	public:
		constexpr LineRanges(bool emergencyBreak = true, bool graphemeAware = true) : pEmergency{ emergencyBreak }, pGraphemes{ graphemeAware } {}

	public:
		template <str::IsReceiver<cp::Range> RecvType>
		constexpr Type<RecvType> operator()(RecvType&& recv) const {
			return Type<RecvType>{ std::forward<RecvType>(recv), pEmergency, pGraphemes };
		}
	};

	/* iterator which allows iteration over codepoints and finding the corresponding line breaks between two codepoints, as well as query the corresponding codepoint-iterators
	*	Additionally specify whether to produce emergency-breaks (based on grapheme-clusters), or ignore grapheme-boundaries entirely and perform default line-breaking
	*	Less efficient than cp::LineBreak/cp::LineRanges; Specify whether to produce emergency-breaks (based on grapheme-clusters), or ignore grapheme-boundaries and perform default line-breaking
	*	Will advance the codepoint (except for direction-changes and construction) and return the break-type between it and the next codepoint in the given direction (will result in all values)
	*	For invalid iterators, BreakMode::edge will be returned at all times */
	template <str::IsIterator ItType>
	class LineIterator : public detail::BreakIterator<ItType, detail::LineRandom> {
	public:
		constexpr LineIterator(const ItType& it, bool emergencyBreak = true, bool graphemeAware = true) : detail::BreakIterator<ItType, detail::LineRandom>{ it, { emergencyBreak, graphemeAware } } {}
	};
}
