#pragma once

#include "../str-common.h"
#include "../str-codepoint.h"

#include "../generated/unicode-normalization.h"

#include <algorithm>
#include <type_traits>

namespace cp {
	namespace detail {
		template <class SinkType, bool WithData>
		class DecompMapper {
		private:
			struct CharData {
				const uint32_t* data = 0;
				char32_t cp = 0;
				uint32_t ccc = 0;
			};
			struct Char {
				char32_t cp = 0;
				uint32_t ccc = 0;
			};

		private:
			detail::LocalBuffer<std::conditional_t<WithData, CharData, Char>, 2> pChars;
			SinkType pSink;

		public:
			constexpr DecompMapper(SinkType&& sink) : pSink{ sink } {}

		private:
			constexpr void fFlush() {
				if (pChars.size() == 0)
					return;

				/* sort the cached chars with ccc in ascending order (sorting is guaranteed to preserve order between equal ccc-values) */
				std::stable_sort(pChars.begin(), pChars.end(), [](const auto& a, const auto& b) -> bool { return a.ccc < b.ccc; });

				/* write the sorted codepoints out */
				while (pChars.size() > 0) {
					if constexpr (WithData) {
						auto [data, cp, _] = pChars.pop();
						pSink(data, cp);
					}
					else
						pSink(pChars.pop().cp);
				}
			}
			constexpr void fDecomposed(char32_t cp, const uint32_t* data) {
				uint32_t ccc = (data[0] & detail::gen::NormCCCMask);

				/* check if the character needs to be queued for sorting or if it is a starter and can immediately be written out */
				if (ccc > 0) {
					if constexpr (WithData)
						pChars.push({ data, cp, ccc });
					else
						pChars.push({ cp, ccc });
				}
				else {
					fFlush();
					if constexpr (WithData)
						pSink(data, cp);
					else
						pSink(cp);
				}
			}
			constexpr void fNext(char32_t cp) {
				const uint32_t* data = detail::gen::GetNormalization(cp);

				/* check if the character is a hangul-syllable composition and decompose it programmatically */
				if (data[0] & detail::gen::NormIsHSComposition) {
					uint32_t sIndex = (cp - detail::gen::NormHSSBase);
					uint32_t lIndex = (sIndex / detail::gen::NormHSNCount);
					uint32_t vIndex = (sIndex % detail::gen::NormHSNCount) / detail::gen::NormHSTCount;
					uint32_t tIndex = (sIndex % detail::gen::NormHSTCount);
					fNext(char32_t(detail::gen::NormHSLBase + lIndex));
					fNext(char32_t(detail::gen::NormHSVBase + vIndex));
					if (tIndex > 0)
						fNext(char32_t(detail::gen::NormHSTBase + tIndex));
					return;
				}

				/* check if a decomposition exists for the character and either recursively decompose them further or handle them as fully decomposed */
				uint32_t decomp = ((data[0] >> detail::gen::NormDecompShift) & detail::gen::NormDecompMask);
				if (decomp == detail::gen::NormDecompNone)
					fDecomposed(cp, data);
				else for (uint32_t i = 0; i < decomp; ++i)
					fNext(data[1 + i]);
			}

		public:
			constexpr void next(char32_t cp) {
				fNext(cp);
			}
			constexpr void done() {
				/* flush any remaining cached characters */
				fFlush();
			}
		};

		template <class SinkType>
		class CompMapper {
		private:
			struct Lambda {
				detail::CompMapper<SinkType>& self;
				constexpr Lambda(detail::CompMapper<SinkType>& s) : self{ s } {}
				constexpr void operator()(const uint32_t* data, char32_t cp) {
					self.fNext(data, cp);
				}
			};
			enum class HSState : uint8_t {
				none,
				lAdded,
				vAdded
			};

		private:
			detail::LocalBuffer<char32_t, 2> pChars;
			detail::DecompMapper<Lambda, true> pDecompose;
			SinkType pSink;
			struct {
				const uint32_t* data = 0;
				size_t count = 0;
				uint32_t ccc = 0;
			} pStarter;
			HSState pHSState = HSState::none;

		public:
			constexpr CompMapper(SinkType&& sink) : pDecompose{ Lambda{ *this } }, pSink{ sink } {}

		private:
			constexpr void fFlush() {
				while (pChars.size() > 0)
					pSink(pChars.pop());
			}
			constexpr bool fCompleteHS(uint32_t tData, char32_t tCp) {
				/* check if a valid hangul-syllable can be composed */
				if (pHSState != HSState::vAdded) {
					/* flush the cached codepoints as they cannot be composed */
					fFlush();
					pHSState = HSState::none;
					return false;
				}

				/* compute the new composed hangul-syllable codepoint */
				bool tType = (tData & detail::gen::NormIsHSTypeT);
				uint32_t lIndex = pChars.get(0) - detail::gen::NormHSLBase;
				uint32_t vIndex = pChars.get(1) - detail::gen::NormHSVBase;
				uint32_t lvIndex = lIndex * detail::gen::NormHSNCount + vIndex * detail::gen::NormHSTCount;
				uint32_t tIndex = (tType ? tCp - detail::gen::NormHSTBase : 0);

				/* write the codepoint out and clear the cached characters and return whether or not the input-codepoint was consumed */
				pSink(char32_t(detail::gen::NormHSSBase + lvIndex + tIndex));
				pChars.clear();
				pHSState = HSState::none;
				return tType;
			}
			constexpr void fNext(const uint32_t* data, char32_t cp) {
				/* check if the first character is a hangul-syllable and can be merged with this codepoint */
				if (pHSState != HSState::none) {
					/* check if the next character can be added */
					if (pHSState == HSState::lAdded && (data[0] & detail::gen::NormIsHSTypeV) != 0) {
						pChars.push(cp);
						pHSState = HSState::vAdded;
						return;
					}

					/* try to complete the hangul-syllable (returns whether or not the incoming codepoint has also been consumed,
					otherwise feed it to the normal procedure; starter will already be set to null and does not need to be reset) */
					if (fCompleteHS(data[0], cp))
						return;
				}

				/* check if a composition with the first character exists and apply it */
				uint32_t ccc = (data[0] & detail::gen::NormCCCMask);
				for (size_t i = 0; i < pStarter.count; ++i) {
					/* check if the mapping does not match or the codepoint is blocked by a higher ccc */
					if (pStarter.data[i * 2] != cp || (pStarter.ccc > 0 && pStarter.ccc >= ccc))
						continue;

					/* update the starter and also the starter state (must be a starter, but may potentially not contain any compositions anymore) */
					pChars.front() = pStarter.data[i * 2 + 1];
					data = detail::gen::GetNormalization(pChars.front());
					size_t count = ((data[0] >> detail::gen::NormCompShift) & detail::gen::NormCompMask);
					if (count == 0) {
						fFlush();
						pStarter = { 0, 0, 0 };
					}
					else
						pStarter = { data + 1 + ((data[0] >> detail::gen::NormDecompShift) & detail::gen::NormDecompMask), count, pStarter.ccc };
					return;
				}

				/* check if the codepoint is not a starter and not excluded, in which case it can either immediately be
				*	written out, if no starter exists, or it needs to be cached and the current blocking-ccc updated */
				bool hsStarter = (data[0] & detail::gen::NormIsHSTypeL);
				if (ccc != 0 && (data[0] & detail::gen::NormIsExcluded) == 0 && !hsStarter) {
					if (pStarter.count == 0)
						pSink(cp);
					else {
						pStarter.ccc = ccc;
						pChars.push(cp);
					}
					return;
				}

				/* flush the cached characters as a new starter has been found */
				fFlush();

				/* check if its a hangul-syllable */
				if (hsStarter) {
					pStarter = { 0, 0, 0 };
					pChars.push(cp);
					pHSState = HSState::lAdded;
					return;
				}

				/* check if the codepoint can potentially be composed with other codepoints and otherwise flush it immediately */
				size_t count = ((data[0] >> detail::gen::NormCompShift) & detail::gen::NormCompMask);
				if (count > 0) {
					pStarter = { data + 1 + ((data[0] >> detail::gen::NormDecompShift) & detail::gen::NormDecompMask), count, 0 };
					pChars.push(cp);
				}
				else {
					pStarter = { 0, 0, 0 };
					pSink(cp);
				}
			}

		public:
			constexpr void next(char32_t cp) {
				pDecompose.next(cp);
			}
			constexpr void done() {
				pDecompose.done();

				/* will automatically flush, if no hangul-syllable state exists */
				fCompleteHS(0, 0);
			}
		};

		template <class SinkType>
		using NoDataDecompMapper = detail::DecompMapper<SinkType, false>;

		template <template<class> class MapType>
		class TestNormalization {
		private:
			struct Lambda {
				detail::TestNormalization<MapType>& self;
				constexpr Lambda(detail::TestNormalization<MapType>& s) : self{ s } {}
				constexpr void operator()(char32_t cp) {
					self.fNext(cp);
				}
			};

		private:
			MapType<Lambda> pMapper;
			detail::LocalBuffer<char32_t, 2> pChars;
			bool pMatches = true;

		public:
			constexpr TestNormalization() : pMapper{ Lambda{ *this } } {}

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

	/* [cp::IsMapper] create a sink, which writes the decomposed-normalized (NFD) stream to the given sink
	*	InSink(char32_t): source codepoint
	*	OutSink(char32_t): decomposed and normalized codepoint(s) */
	class Decompose {
	public:
		template <cp::IsSink<char32_t> SinkType>
		using Type = detail::DecompMapper<SinkType, false>;

	public:
		constexpr Decompose() {}

	public:
		template <cp::IsSink<char32_t> SinkType>
		constexpr Type<SinkType> operator()(SinkType&& sink) {
			return Type<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* [cp::IsMapper] create a sink, which writes the composed-normalized (NFC) stream to the given sink
	*	InSink(char32_t): source codepoint
	*	OutSink(char32_t): composed and normalized codepoint(s) */
	class Compose {
	public:
		template <cp::IsSink<char32_t> SinkType>
		using Type = detail::CompMapper<SinkType>;

	public:
		constexpr Compose() {}

	public:
		template <cp::IsSink<char32_t> SinkType>
		constexpr Type<SinkType> operator()(SinkType&& sink) {
			return Type<SinkType>{ std::forward<SinkType>(sink) };
		}
	};

	/* [cp::IsTester<bool>] check if the entire stream of codepoints is decomposed-normalized (NFD) (i.e. cp::Decompose(...) would result in the same codepoints) */
	class TestDecompose : public detail::TestNormalization<detail::NoDataDecompMapper> {
	public:
		constexpr TestDecompose() = default;
	};

	/* [cp::IsTester<bool>] check if the entire stream of codepoints is composed-normalized (NFC) (i.e. cp::Compose(...) would result in the same codepoints) */
	class TestCompose : public detail::TestNormalization<detail::CompMapper> {
	public:
		constexpr TestCompose() = default;
	};
}
