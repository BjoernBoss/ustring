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

	/* codepoint-iterator must move itself upon prev()/next() and return true, or return false and stay
	*	and must return the currently pointed to codepoint on get() */
	template <class Type>
	concept IsCPIterator = std::copyable<Type> && requires(Type t, const Type ct) {
		{ t.prev() } -> std::same_as<bool>;
		{ t.next() } -> std::same_as<bool>;
		{ ct.get() } -> std::same_as<char32_t>;
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

		struct GraphemeBreak {
			static_assert(size_t(detail::gen::GraphemeType::_last) == 21, "Only types 0-20 are known by the state-machine");
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
				return static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::GraphemeSegmentationOff) & detail::gen::SegmentationMask);
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
				}

				/* GB999 */
				return Break::separate;
			}
		};
		template <class ItType>
		struct GraphemeIterator {
			friend struct detail::GraphemeBreak;
			using Host = detail::GraphemeBreak;
		private:
			ItType& pLeft;

		private:
			constexpr GraphemeIterator(ItType& l) : pLeft{ l } {}

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
				} while (t.prev());
				return false;
			}
			constexpr bool fGB11HasExtPicto() const {
				ItType t = pLeft;
				while (t.prev()) {
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
				while (t.prev() && Host::GetCP(t.get()) == Host::Type::regionalIndicator)
					++count;
				return ((count & 0x01) == 0);
			}

		public:
			static constexpr ItType Forwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.next())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(fVal, sVal, GraphemeIterator<ItType>{ fIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
			static constexpr ItType Backwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.prev())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(sVal, fVal, GraphemeIterator<ItType>{ sIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
		};
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
			GB9cState pGB9cState = GB9cState::none;
			GB11State pGB11State = GB11State::none;
			Host::Type pLast = Host::Type::other;
			bool pRICountOdd = false;

		public:
			constexpr GraphemeForward() {}

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
			constexpr bool next(uint32_t raw) {
				/* GB2: no special handling for cleanup necessary */
				Host::Type left = pLast, right = Host::GetRaw(raw);

				/* update the states */
				pGB9cState = fUpdateGB9cState(left);
				pGB11State = fUpdateGB11State(left);
				pRICountOdd = (left == Host::Type::regionalIndicator ? !pRICountOdd : false);
				pLast = right;

				/* check if the two values should be separated */
				return (Host::Test(left, right, *this) == Host::Break::separate);
			}
		};

		struct WordBreak {
			static_assert(size_t(detail::gen::WordType::_last) == 21, "Only types 0-20 are known by the state-machine");
		public:
			using Type = detail::gen::WordType;
			enum class Break : uint8_t {
				separate,
				combine,
				uncertain
			};

		public:
			static constexpr Type GetCP(char32_t cp) {
				return static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::WordSegmentationOff) & detail::gen::SegmentationMask);
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
				}

				switch (self.fWB4SkipIgnorables(l)) {
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
				}

				/* WB999 */
				return Break::separate;
			}
		};
		template <class ItType>
		class WordIterator {
			friend struct detail::WordBreak;
			using Host = detail::WordBreak;
		private:
			ItType& pLeft;
			ItType& pRight;

		private:
			constexpr WordIterator(ItType& l, ItType& r) : pLeft{ l }, pRight{ r } {}

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
				} while (t.prev());
				return Host::Type::other;
			}
			Host::Type fGetNext() const {
				ItType t = pRight;
				while (t.next()) {
					Host::Type type = Host::GetCP(t.get());
					if (type != Host::Type::extend && type != Host::Type::format && type != Host::Type::zwj)
						return type;
				}
				return Host::Type::other;
			}

		private:
			constexpr Host::Type fWB4SkipIgnorables(Host::Type l) const {
				ItType t = pLeft;
				while ((l == Host::Type::extend || l == Host::Type::format || l == Host::Type::zwj) && t.prev())
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
				} while (t.prev());
				return ((count & 0x01) != 0);
			}

		public:
			static constexpr ItType Forwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.next())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(fVal, sVal, WordIterator<ItType>{ fIt, sIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
			static constexpr ItType Backwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.prev())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(sVal, fVal, WordIterator<ItType>{ sIt, fIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
		};
		template <class SinkType, class PayloadType>
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
			detail::LocalBuffer<PayloadType, 2> pCache;
			SinkType pSink;
			PayloadType pUncertainPayload{};
			Host::Type pLast = Host::Type::other;
			Host::Type pLastActual = Host::Type::other;
			State pState = State::none;
			bool pRICountOdd = false;

		public:
			constexpr WordForward(SinkType&& sink) : pSink{ sink } {}

		private:
			constexpr Host::Type fWB4SkipIgnorables(Host::Type) const {
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
			constexpr void first(char32_t cp) {
				Host::Type right = Host::GetCP(cp);

				/* WB1: initialize the last-state */
				pRICountOdd = (right == Host::Type::regionalIndicator ? !pRICountOdd : false);
				pLast = right;
				pLastActual = right;
			}
			constexpr void next(char32_t cp, const PayloadType& payload) {
				Host::Type right = Host::GetCP(cp);

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
					pSink(pUncertainPayload, cont == Continue::breakBeforeCached);
					while (pCache.size() > 0)
						pSink(pCache.pop(), false);

					/* check if the upcoming element can be consumed as well */
					if (cont == Continue::combineIncludingRight) {
						pSink(payload, false);
						fUpdateLeft(right);
						return;
					}
				}

				/* check the current values and update the state */
				switch (Host::Test(pLastActual, right, *this)) {
				case Host::Break::combine:
					pSink(payload, false);
					break;
				case Host::Break::separate:
					pSink(payload, true);
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
				pSink(pUncertainPayload, fCheckState(Host::Type::other) == Continue::breakBeforeCached);
				while (pCache.size() > 0)
					pSink(pCache.pop(), false);
			}
		};

		struct SentenceBreak {
			static_assert(size_t(detail::gen::SentenceType::_last) == 15, "Only types 0-14 are known by the state-machine");
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
			static constexpr Type GetCP(char32_t cp) {
				return static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::SentenceSegmentationOff) & detail::gen::SegmentationMask);
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
				}

				/* SB11 */
				if (c != Chain::none && c != Chain::lowUp)
					return Break::separate;

				/* SB998 */
				return Break::combine;
			}
		};
		template <class ItType>
		struct SentenceIterator {
			friend struct detail::SentenceBreak;
			using Host = detail::SentenceBreak;
		private:
			ItType& pLeft;
			ItType& pRight;

		private:
			constexpr SentenceIterator(ItType& l, ItType& r) : pLeft{ l }, pRight{ r } {}

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
				} while (t.prev());
				return (hasATerm ? Host::Chain::aTerm : Host::Chain::none);
			}
			constexpr Host::Break fSB8Check() const {
				ItType t = pRight;

				while (t.next()) {
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
			static constexpr ItType Forwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.next())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(fVal, sVal, SentenceIterator<ItType>{ fIt, sIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
			static constexpr ItType Backwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.prev())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(sVal, fVal, SentenceIterator<ItType>{ sIt, fIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
		};
		template <class SinkType, class PayloadType>
		class SentenceForward {
			friend struct detail::SentenceBreak;
			using Host = detail::SentenceBreak;
		private:
			struct Cache {
				PayloadType payload{};
				Host::Type type = Host::Type::other;
			};

		private:
			detail::LocalBuffer<Cache, 2> pCache;
			PayloadType pUncertainPayload{};
			SinkType pSink;
			Host::Type pLast = Host::Type::other;
			Host::Chain pChain = Host::Chain::none;
			bool pUncertain = false;

		public:
			constexpr SentenceForward(SinkType&& sink) : pSink{ sink } {}

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
					pSink(pUncertainPayload, _break == Host::Break::separate);

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
			constexpr void first(char32_t cp) {
				/* SB1: initialize the state */
				pLast = Host::GetCP(cp);
				pChain = fUpdateChain(pLast);
			}
			constexpr void next(char32_t cp, const PayloadType& payload) {
				Host::Type right = Host::GetCP(cp);

				/* fast-path of not being in a look-ahead state */
				if (!pUncertain) {
					Host::Break _break = Host::Test(pLast, right, *this);
					if (_break != Host::Break::uncertain)
						pSink(payload, _break == Host::Break::separate);
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
			static_assert(size_t(detail::gen::LineType::_last) == 50, "Only types 0-49 are known by the state-machine");
		public:
			using Type = detail::gen::LineType;
			enum class Break : uint8_t {
				optional,
				mandatory,
				combine,
				uncertain
			};

		public:
			static constexpr Type GetRaw(uint32_t raw) {
				return static_cast<Type>((raw >> detail::gen::LineSegmentationOff) & detail::gen::SegmentationMask);
			}
			static constexpr Type GetCP(char32_t cp) {
				return static_cast<Type>((detail::gen::GetSegmentation(cp) >> detail::gen::LineSegmentationOff) & detail::gen::SegmentationMask);
			}
			template <class SelfType>
			static constexpr Break Test(Type l, Type r, SelfType&& self) {
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
					l = self.fSkipSpaceAndLB9(l);

					/* LB8 */
					if (l == Type::zw)
						return Break::optional;

					/* LB11/LB13/LB14 */
					if (r == Type::wj || r == Type::cl || r == Type::cpDef || r == Type::cpNoFWH || r == Type::ex || r == Type::is || r == Type::sy)
						return Break::combine;

					/* LB14 */
					if (l == Type::opDef || l == Type::opNoFWH)
						return Break::combine;

					/* LB15a */
					if (l == Type::quPi)
						return self.fLB15aCheck();

					/* LB15b */
					if (r == Type::quPf)
						return self.fLB15bCheck();

					/* LB16 */
					if (r == Type::ns && (l == Type::cl || l == Type::cpDef || l == Type::cpNoFWH))
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

				l = self.fSkipSpaceAndLB9(l);
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
					if (l != Type::ba && l != Type::hy)
						return Break::combine;
					break;
				case Type::cl:
				case Type::cpDef:
				case Type::cpNoFWH:
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
				case Type::quDef:
				case Type::quPf:
				case Type::quPi:
					/* LB19 */
					return Break::combine;
				case Type::cb:
					/* LB11/LB12/LB14 */
					if (l == Type::wj || l == Type::gl || l == Type::opDef || l == Type::opNoFWH)
						return Break::combine;

					/* LB19/LB20 */
					if (l == Type::quDef || l == Type::quPf || l == Type::quPi)
						return Break::combine;
					return Break::optional;
				case Type::ns:
					/* LB16 */
					if (l == Type::cl || l == Type::cpDef || l == Type::cpNoFWH)
						return Break::combine;
					[[fallthrough]];
				case Type::ba:
				case Type::hy:
				case Type::in:
					/* LB11/LB12/LB14 */
					if (l == Type::wj || l == Type::gl || l == Type::opDef || l == Type::opNoFWH)
						return Break::combine;

					/* LB20 */
					if (l == Type::cb)
						return Break::optional;

					/* LB21/LB22 */
					return Break::combine;
				}

				switch (l) {
				case Type::wj:
					/* LB11 */
					return Break::combine;
				case Type::gl:
					/* LB12 */
					return Break::combine;
				case Type::opDef:
				case Type::opNoFWH:
					/* LB14 */
					return Break::combine;
				case Type::quDef:
				case Type::quPf:
				case Type::quPi:
					/* LB19 */
					return Break::combine;
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
				case Type::ba:
					/* LB21a */
					return self.fLB21aCheck();
				case Type::is:
					/* LB29 */
					if (r == Type::alDef || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
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
					if (r == Type::alDef || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;

					/* LB30 */
					if (r == Type::opNoFWH)
						return Break::combine;
					break;
				case Type::nu:
					/* LB23 */
					if (r == Type::alDef || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;

					/* LB25 */
					if (r == Type::nu || r == Type::pr || r == Type::po)
						return Break::combine;

					/* LB30 */
					if (r == Type::opNoFWH)
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
					if (r == Type::alDef || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;

					/* LB25 */
					if (r == Type::nu)
						return Break::combine;
					if (r == Type::opDef || r == Type::opNoFWH || r == Type::hy)
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
				case Type::cpNoFWH:
					/* LB30 */
					if (r == Type::nu || r == Type::alDef || r == Type::alDotCircle || r == Type::cm || r == Type::zwj || r == Type::hl)
						return Break::combine;
					[[fallthrough]];
				case Type::cl:
				case Type::cpDef:
					/* LB25 */
					if (r == Type::pr || r == Type::po)
						return self.fLB25CheckLeft();
					break;
				case Type::ri:
					/* LB30a */
					if (r == Type::ri && self.fIsLeftRIOdd())
						return Break::combine;
					break;
				}

				/* LB31 */
				return Break::optional;
			}
		};
		template <class ItType>
		struct LineIterator {
			friend struct detail::LineBreak;
			using Host = detail::LineBreak;
		private:
			ItType& pLeft;
			ItType& pRight;

		private:
			constexpr LineIterator(ItType& l, ItType& r) : pLeft{ l }, pRight{ r } {}

		private:
			std::pair<ItType, Host::Type> fSkipIntermediate(const ItType& it, Host::Type type, bool skipSpaces) const {
				ItType t = it;

				while (true) {
					if (type == Host::Type::sp && skipSpaces) {
						if (!t.prev())
							break;
						type = Host::GetCP(t.get());
					}
					else if (type != Host::Type::cm && type != Host::Type::zwj)
						break;
					else {
						ItType tIt = t;
						if (!tIt.prev())
							break;
						Host::Type temp = Host::GetCP(tIt.get());
						if (temp == Host::Type::bk || temp == Host::Type::cr || temp == Host::Type::lf || temp == Host::Type::nl || temp == Host::Type::sp || temp == Host::Type::zw)
							break;
						type = temp;
						t = tIt;
					}
				}
				return { t, type };
			}
			std::pair<Host::Type, bool> fGetPrev() const {
				auto [it, type] = fSkipIntermediate(pLeft, Host::GetCP(pLeft.get()), true);
				if (!it.prev())
					return { type, false };
				return { fSkipIntermediate(it, Host::GetCP(it.get()), false).second, true };
			}
			Host::Type fGetNext() const {
				ItType t = pRight;
				while (t.next()) {
					Host::Type type = Host::GetCP(t.get());
					if (type != Host::Type::cm && type != Host::Type::zwj)
						return type;
				}
				return Host::Type::_last;
			}

		private:
			constexpr Host::Type fSkipSpaceAndLB9(Host::Type l) const {
				return fSkipIntermediate(pLeft, l, true).second;
			}
			constexpr Host::Break fLB15aCheck() const {
				auto [type, exists] = fGetPrev();
				if (!exists || type == Host::Type::bk || type == Host::Type::cr || type == Host::Type::lf || type == Host::Type::nl ||
					type == Host::Type::opDef || type == Host::Type::opNoFWH || type == Host::Type::quDef || type == Host::Type::quPf ||
					type == Host::Type::quPi || type == Host::Type::gl || type == Host::Type::sp || type == Host::Type::zw)
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB15bCheck() const {
				Host::Type type = fGetNext();
				if (type == Host::Type::sp || type == Host::Type::gl || type == Host::Type::wj || type == Host::Type::cl || type == Host::Type::quDef ||
					type == Host::Type::quPf || type == Host::Type::quPi || type == Host::Type::cpDef || type == Host::Type::cpNoFWH || type == Host::Type::ex ||
					type == Host::Type::is || type == Host::Type::sy || type == Host::Type::bk || type == Host::Type::cr || type == Host::Type::lf ||
					type == Host::Type::nl || type == Host::Type::zw || type == Host::Type::_last)
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB21aCheck() const {
				auto [type, exists] = fGetPrev();
				return ((exists && type == Host::Type::hl) ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB25CheckLeft() const {
				auto [type, exists] = fGetPrev();
				return ((exists && type == Host::Type::nu) ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB25CheckRight() const {
				return (fGetNext() == Host::Type::nu ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB28a2Check() const {
				auto [type, exists] = fGetPrev();
				if (exists && (type == Host::Type::ak || type == Host::Type::alDotCircle || type == Host::Type::as))
					return Host::Break::combine;
				return Host::Break::optional;
			}
			constexpr Host::Break fLB28a3Check() const {
				return (fGetNext() == Host::Type::vf ? Host::Break::combine : Host::Break::optional);
			}
			constexpr bool fIsLeftRIOdd() const {
				ItType t = pLeft;
				size_t count = 0;
				do {
					Host::Type type = Host::GetCP(t.get());
					if (type == Host::Type::ri)
						++count;
					else if (type != Host::Type::cm && type != Host::Type::zwj)
						break;
				} while (t.prev());
				return ((count & 0x01) != 0);
			}

		public:
			static constexpr ItType Forwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.next())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(fVal, sVal, LineIterator<ItType>{ fIt, sIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
			static constexpr ItType Backwards(ItType fIt) {
				/* fetch the initial type */
				ItType sIt = fIt;
				Host::Type fVal = Host::GetCP(fIt.get());
				while (true) {
					/* check if the next token exists, and otherwise this is the last character before the break
					*	(this also ensures that a fully invalid iterator will immediately be returned as-is) */
					if (!sIt.prev())
						return fIt;
					Host::Type sVal = Host::GetCP(sIt.get());

					/* check if the edge exists between the two slots and otherwise step once to the right */
					if (Host::Test(sVal, fVal, LineIterator<ItType>{ sIt, fIt }) != Host::Break::combine)
						return fIt;
					fIt = sIt;
					fVal = sVal;
				}
			}
		};
		template <class SinkType, class PayloadType>
		class LineForward {
			friend struct detail::LineBreak;
			using Host = detail::LineBreak;
		private:
			enum class Emergency : uint8_t {
				emergency,
				graphemeAware,
				none
			};
			enum class State : uint8_t {
				none,
				lb15b,
				lb25,
				lb28a3
			};
			enum class Chain : uint8_t {
				none,
				lb15aMatch,
				lb21aMatch,
				lb25Nu,
				lb25PrPo,
				lb28a2Match
			};
			struct Cache {
				PayloadType payload{};
				bool combine = false;
			};

		private:
			detail::GraphemeForward pGrapheme;
			detail::LocalBuffer<Cache, 2> pCache;
			SinkType pSink;
			PayloadType pUncertainPayload{};
			Emergency pCheckEmergency = Emergency::none;
			Host::Type pLast = Host::Type::_last;
			Host::Type pActual = Host::Type::_last;
			Chain pChain = Chain::none;
			State pState = State::none;
			bool pRICountOdd = false;

		public:
			constexpr LineForward(bool checkGrapheme, bool createEmergency, SinkType&& sink) : pSink{ sink } {
				if (checkGrapheme)
					pCheckEmergency = (createEmergency ? Emergency::emergency : Emergency::graphemeAware);
			}

		private:
			constexpr Host::Type fSkipSpaceAndLB9(Host::Type) const {
				return pLast;
			}
			constexpr Host::Break fLB15aCheck() const {
				return (pChain == Chain::lb15aMatch ? Host::Break::combine : Host::Break::optional);
			}
			constexpr Host::Break fLB15bCheck() {
				pState = State::lb15b;
				return Host::Break::uncertain;
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
			constexpr void fUpdateLeft(Host::Type r) {
				/* update the chain state-machine */
				switch (r) {
				case Host::Type::quPi:
					if (pLast == Host::Type::bk || pLast == Host::Type::cr || pLast == Host::Type::lf || pLast == Host::Type::nl ||
						pLast == Host::Type::opDef || pLast == Host::Type::opNoFWH || pLast == Host::Type::quDef || pLast == Host::Type::quPf ||
						pLast == Host::Type::quPi || pLast == Host::Type::gl || pLast == Host::Type::zw || pActual == Host::Type::sp)
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
				case Host::Type::cpDef:
				case Host::Type::cpNoFWH:
					pChain = (pChain == Chain::lb25Nu ? Chain::lb25PrPo : Chain::none);
					break;
				case Host::Type::hy:
				case Host::Type::ba:
					pChain = (pLast == Host::Type::hl ? Chain::lb21aMatch : Chain::none);
					break;
				case Host::Type::vi:
					if (pLast == Host::Type::ak || pLast == Host::Type::alDotCircle || pLast == Host::Type::as)
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
				if (r == Host::Type::sp) {
					pRICountOdd = false;
					return;
				}

				/* skip the invisible types and update the last value and ri-parity */
				if ((r == Host::Type::cm || r == Host::Type::zwj) && pLast != Host::Type::bk && pLast != Host::Type::cr &&
					pLast != Host::Type::lf && pLast != Host::Type::nl && pLast != Host::Type::sp && pLast != Host::Type::zw)
					return;
				pRICountOdd = (r == Host::Type::ri ? !pRICountOdd : false);
				pLast = r;
			}
			constexpr Host::Break fCheckState(Host::Type r) const {
				/* check if the character does not close the state */
				if (r == Host::Type::cm || r == Host::Type::zwj)
					return Host::Break::uncertain;

				/* LB15b with certainty that it was followed by space, and would therefore otherwise be an optional break */
				if (pState == State::lb15b) {
					if (r == Host::Type::sp || r == Host::Type::gl || r == Host::Type::wj || r == Host::Type::cl || r == Host::Type::quDef || r == Host::Type::quPf ||
						r == Host::Type::quPi || r == Host::Type::cpDef || r == Host::Type::cpNoFWH || r == Host::Type::ex || r == Host::Type::is ||
						r == Host::Type::sy || r == Host::Type::bk || r == Host::Type::cr || r == Host::Type::lf || r == Host::Type::nl || r == Host::Type::zw)
						return Host::Break::combine;
					return Host::Break::optional;
				}

				/* LB25 */
				if (pState == State::lb25) {
					if (r == Host::Type::nu)
						return Host::Break::combine;
					return Host::Break::optional;
				}

				/* LB28a */
				if (r == Host::Type::vf)
					return Host::Break::combine;
				return Host::Break::optional;
			}

		private:
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
				Host::Type right = Host::GetRaw(raw);

				/* LB2: initialize the state */
				if (pCheckEmergency != Emergency::none)
					pGrapheme.first(raw);
				pLast = (pActual = right);
				pRICountOdd = (right == Host::Type::ri);
				if (right == Host::Type::nu)
					pChain = Chain::lb25Nu;
				else if (right == Host::Type::quPi)
					pChain = Chain::lb15aMatch;
			}
			constexpr void next(char32_t cp, const PayloadType& payload) {
				uint32_t raw = detail::gen::GetSegmentation(cp);
				Host::Type right = Host::GetRaw(raw);

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
					Host::Break _break = fCheckState(right);
					if (_break == Host::Break::uncertain) {
						pCache.push({ payload, (pCheckEmergency != Emergency::emergency) });
						fUpdateLeft(right);
						return;
					}
					pState = State::none;

					/* clear all cached characters */
					fPopQueue(_break == Host::Break::combine);
				}

				/* check the current values and update the state */
				switch (Host::Test(pActual, right, *this)) {
				case Host::Break::combine:
					pSink(payload, (pCheckEmergency == Emergency::emergency ? cp::LineMode::emergency : cp::LineMode::combine));
					break;
				case Host::Break::optional:
					pSink(payload, cp::LineMode::optional);
					break;
				case Host::Break::mandatory:
					pSink(payload, cp::LineMode::mandatory);
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

	/* create a sink, which receives the 'grapheme-break-before' attribute for every codepoint except for the first codepoint (will be produced in-order)
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output
	*	OutSink(size_t, bool): insert break before codepoint at given index */
	class GraphemeBreak {
	private:
		template <class SinkType>
		class Impl {
		private:
			detail::GraphemeForward pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output-ranges
	*	OutSink(cp::Range): range of a single grapheme-cluster */
	class GraphemeRanges {
	private:
		template <class SinkType>
		class Impl {
		private:
			detail::GraphemeForward pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output
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
			detail::WordForward<Lambda, size_t> pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output-ranges
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
			detail::WordForward<Lambda, size_t> pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output
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
			detail::SentenceForward<Lambda, size_t> pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output-ranges
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
			detail::SentenceForward<Lambda, size_t> pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output
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
			detail::LineForward<Lambda, size_t> pBreaker;
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
	*	InSink(char32_t, size_t): codepoint and index used to reference it in the output-ranges
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
			detail::LineForward<Lambda, size_t> pBreaker;
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



	class GraphemeNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::GraphemeIterator<ItType>::Forwards(it);
		}
	};
	class GraphemePrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::GraphemeIterator<ItType>::Backwards(it);
		}
	};

	class WordNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::WordIterator<ItType>::Forwards(it);
		}
	};
	class WordPrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::WordIterator<ItType>::Backwards(it);
		}
	};

	class SentenceNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::SentenceIterator<ItType>::Forwards(it);
		}
	};
	class SentencePrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::SentenceIterator<ItType>::Backwards(it);
		}
	};

	class LineNext {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::LineIterator<ItType>::Forwards(it);
		}
	};
	class LinePrev {
	public:
		template <cp::IsCPIterator ItType>
		constexpr ItType operator()(ItType it) {
			return detail::LineIterator<ItType>::Backwards(it);
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
