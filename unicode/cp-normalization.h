/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "../str-common.h"
#include "cp-casing.h"

#include "../generated/unicode-normalization.h"

#include <algorithm>
#include <type_traits>

namespace cp {
	namespace detail {
		template <class CollType, bool WithData>
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
			str::detail::LocalBuffer<std::conditional_t<WithData, CharData, Char>> pChars;
			CollType pCollector;

		public:
			constexpr DecompMapper(CollType&& collector) : pCollector{ collector } {}

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
						pCollector.next(data, cp);
					}
					else
						pCollector.next(pChars.pop().cp);
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
						pCollector.next(data, cp);
					else
						pCollector.next(cp);
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
				pCollector.done();
			}
		};

		template <class CollType>
		class CompMapper {
		private:
			struct Lambda {
				detail::CompMapper<CollType>& self;
				constexpr Lambda(detail::CompMapper<CollType>& s) : self{ s } {}
				constexpr void next(const uint32_t* data, char32_t cp) {
					self.fNext(data, cp);
				}
				constexpr void done() {}
			};
			enum class HSState : uint8_t {
				none,
				lAdded,
				vAdded
			};

		private:
			str::detail::LocalBuffer<char32_t> pChars;
			detail::DecompMapper<Lambda, true> pDecompose;
			CollType pCollector;
			struct {
				const uint32_t* data = 0;
				size_t count = 0;
				uint32_t ccc = 0;
			} pStarter;
			HSState pHSState = HSState::none;

		public:
			constexpr CompMapper(CollType&& collector) : pDecompose{ Lambda{ *this } }, pCollector{ collector } {}

		private:
			constexpr void fFlush() {
				while (pChars.size() > 0)
					pCollector.next(pChars.pop());
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
				pCollector.next(char32_t(detail::gen::NormHSSBase + lvIndex + tIndex));
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
						pCollector.next(cp);
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
					pCollector.next(cp);
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
				pCollector.done();
			}
		};

		template <class CollType>
		class NormFoldMapper {
		private:
			struct Lambda {
				detail::NormFoldMapper<CollType>& self;
				constexpr Lambda(detail::NormFoldMapper<CollType>& s) : self{ s } {}
				constexpr void next(char32_t cp) {
					self.pDecompose.next(cp);
				}
				constexpr void done() {
					self.pDecompose.done();
				}
			};

		private:
			cp::FoldCase::Type<Lambda> pCaseFold;
			detail::DecompMapper<CollType, false> pDecompose;

		public:
			constexpr NormFoldMapper(CollType&& collector, const wchar_t* locale) : pCaseFold{ cp::FoldCase{ locale }(Lambda{ *this }) }, pDecompose{ std::forward<CollType>(collector) } {}

		public:
			constexpr void next(char32_t cp) {
				pCaseFold.next(cp);
			}
			constexpr void done() {
				pCaseFold.done();
			}
		};

		template <class CollType>
		using NoDataDecompMapper = detail::DecompMapper<CollType, false>;

		template <template<class> class MapType, class... Args>
		class TestNormalization {
		private:
			struct Lambda {
				detail::TestNormalization<MapType, Args...>& self;
				constexpr Lambda(detail::TestNormalization<MapType, Args...>& s) : self{ s } {}
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
			constexpr TestNormalization(Args... args) : pMapper{ Lambda{ *this }, args... } {}

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

	/* [str::IsMapper] create a collector, which writes the decomposed-normalized (NFD) stream to the given collector */
	class Decompose {
	public:
		template <str::IsCollector CollType>
		using Type = detail::DecompMapper<CollType, false>;

	public:
		constexpr Decompose() {}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector) };
		}
	};

	/* [str::IsMapper] create a collector, which writes the composed-normalized (NFC) stream to the given collector */
	class Compose {
	public:
		template <str::IsCollector CollType>
		using Type = detail::CompMapper<CollType>;

	public:
		constexpr Compose() {}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector) };
		}
	};

	/* [str::IsMapper] create a collector, which writes the casefolded and decomposed-normalized (NFD) stream to the given collector */
	class NormFold {
	public:
		template <str::IsCollector CollType>
		using Type = detail::NormFoldMapper<CollType>;

	private:
		const wchar_t* pLocale = 0;

	public:
		constexpr NormFold(const wchar_t* locale = 0) : pLocale{ locale } {}

	public:
		template <str::IsCollector CollType>
		constexpr Type<std::remove_cvref_t<CollType>> operator()(CollType&& collector) const {
			return Type<std::remove_cvref_t<CollType>>{ std::forward<CollType>(collector), pLocale };
		}
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is decomposed-normalized (NFD) (i.e. cp::Decompose(...) would result in the same codepoints) */
	class TestDecompose : public detail::TestNormalization<detail::NoDataDecompMapper> {
	public:
		constexpr TestDecompose() = default;
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is composed-normalized (NFC) (i.e. cp::Compose(...) would result in the same codepoints) */
	class TestCompose : public detail::TestNormalization<detail::CompMapper> {
	public:
		constexpr TestCompose() = default;
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is casefolded and decomposed-normalized (NFD) (i.e. cp::NormFold(...) would result in the same codepoints) */
	class TestNormFold : public detail::TestNormalization<detail::NormFoldMapper, const wchar_t*> {
	public:
		constexpr TestNormFold(const wchar_t* locale = 0) : detail::TestNormalization<detail::NormFoldMapper, const wchar_t*>{ locale } {}
	};
}
