/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2026 Bjoern Boss Henrichsen */
#pragma once

#include "../common/str-common.h"
#include "../common/str-local.h"
#include "cp-casing.h"

#include "../generated/unicode-normalization.h"

#include <algorithm>
#include <type_traits>

namespace cp {
	namespace detail {
		template <bool WithData>
		class Decomposer {
		private:
			struct CharData {
				const uint32_t* data = nullptr;
				char32_t cp = 0;
				uint32_t ccc = 0;
			};
			struct Char {
				char32_t cp = 0;
				uint32_t ccc = 0;
			};

		private:
			str::detail::LocalQueue<std::conditional_t<WithData, CharData, Char>> pChars;

		public:
			constexpr Decomposer() = default;

		private:
			constexpr void fFlush(auto&& recv) {
				if (pChars.size() == 0)
					return;

				/* sort the cached chars with ccc in ascending order (sorting is guaranteed to preserve order between equal ccc-values) */
				std::stable_sort(pChars.begin(), pChars.end(), [](const auto& a, const auto& b) -> bool { return a.ccc < b.ccc; });

				/* write the sorted codepoints out */
				while (pChars.size() > 0) {
					if constexpr (WithData) {
						auto [data, cp, _] = pChars.pop();
						recv(data, cp);
					}
					else
						recv(pChars.pop().cp);
				}
			}
			constexpr void fDecomposed(char32_t cp, const uint32_t* data, auto&& recv) {
				uint32_t ccc = (data[0] & detail::gen::NormCCCMask);

				/* check if the character needs to be queued for sorting or if it is a starter and can immediately be written out */
				if (ccc > 0) {
					if constexpr (WithData)
						pChars.push({ data, cp, ccc });
					else
						pChars.push({ cp, ccc });
				}
				else {
					fFlush(recv);
					if constexpr (WithData)
						recv(data, cp);
					else
						recv(cp);
				}
			}
			constexpr void fNext(char32_t cp, auto&& recv) {
				const uint32_t* data = detail::gen::GetNormalization(cp);

				/* check if the character is a hangul-syllable composition and decompose it programmatically */
				if (data[0] & detail::gen::NormIsHSComposition) {
					uint32_t sIndex = (cp - detail::gen::NormHSSBase);
					uint32_t lIndex = (sIndex / detail::gen::NormHSNCount);
					uint32_t vIndex = (sIndex % detail::gen::NormHSNCount) / detail::gen::NormHSTCount;
					uint32_t tIndex = (sIndex % detail::gen::NormHSTCount);
					fNext(char32_t(detail::gen::NormHSLBase + lIndex), recv);
					fNext(char32_t(detail::gen::NormHSVBase + vIndex), recv);
					if (tIndex > 0)
						fNext(char32_t(detail::gen::NormHSTBase + tIndex), recv);
					return;
				}

				/* check if a decomposition exists for the character and either recursively decompose them further or handle them as fully decomposed */
				uint32_t decomp = ((data[0] >> detail::gen::NormDecompShift) & detail::gen::NormDecompMask);
				if (decomp == detail::gen::NormDecompNone)
					fDecomposed(cp, data, recv);
				else for (uint32_t i = 0; i < decomp; ++i)
					fNext(char32_t(data[1 + i]), recv);
			}

		public:
			constexpr void next(char32_t cp, auto&& recv) {
				fNext(cp, recv);
			}
			constexpr void done(auto&& recv) {
				/* flush any remaining cached characters */
				fFlush(recv);
			}
		};

		template <class CollType>
		class DecompMapper {
		private:
			detail::Decomposer<false> pDecompose;
			CollType pCollector;

		public:
			constexpr DecompMapper(CollType&& collector) : pCollector{ std::forward<CollType>(collector) } {}

		public:
			constexpr std::remove_reference_t<CollType>& collector() {
				return pCollector;
			}
			constexpr void next(char32_t cp) {
				pDecompose.next(cp, [this](char32_t c) { pCollector.next(c); });
			}
			constexpr void done() {
				pDecompose.done([this](char32_t c) { pCollector.next(c); });
				pCollector.done();
			}
		};

		template <class CollType>
		class CompMapper {
		private:
			enum class HSState : uint8_t {
				none,
				lAdded,
				vAdded
			};

		private:
			str::detail::LocalQueue<char32_t> pChars;
			detail::Decomposer<true> pDecompose;
			CollType pCollector;
			struct {
				const uint32_t* data = nullptr;
				size_t count = 0;
				uint32_t ccc = 0;
			} pStarter;
			HSState pHSState = HSState::none;

		public:
			constexpr CompMapper(CollType&& collector) : pCollector{ std::forward<CollType>(collector) } {}

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
					pChars.front() = char32_t(pStarter.data[i * 2 + 1]);
					data = detail::gen::GetNormalization(pChars.front());
					size_t count = ((data[0] >> detail::gen::NormCompShift) & detail::gen::NormCompMask);
					if (count == 0) {
						fFlush();
						pStarter = { nullptr, 0, 0 };
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
					pStarter = { nullptr, 0, 0 };
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
					pStarter = { nullptr, 0, 0 };
					pCollector.next(cp);
				}
			}

		public:
			constexpr std::remove_reference_t<CollType>& collector() {
				return pCollector;
			}
			constexpr void next(char32_t cp) {
				pDecompose.next(cp, [this](const uint32_t* data, char32_t c) { fNext(data, c); });
			}
			constexpr void done() {
				pDecompose.done([this](const uint32_t* data, char32_t c) { fNext(data, c); });

				/* will automatically flush, if no hangul-syllable state exists */
				fCompleteHS(0, 0);
				pCollector.done();
			}
		};

		/* canonical caseless mapping is NFD(casefold(NFD(x))) */
		template <class CollType>
		class NormFoldMapper {
		private:
			detail::DecompMapper<detail::FoldingMapper<detail::DecompMapper<CollType>>> pMapper;

		public:
			constexpr NormFoldMapper(CollType&& collector, detail::CaseLocale locale) : pMapper{ detail::FoldingMapper<detail::DecompMapper<CollType>>{ detail::DecompMapper<CollType>{ std::forward<CollType>(collector) }, locale } } {}

		public:
			constexpr std::remove_reference_t<CollType>& collector() {
				return pMapper.collector().collector().collector();
			}
			constexpr void next(char32_t cp) {
				pMapper.next(cp);
			}
			constexpr void done() {
				pMapper.done();
			}
		};

		template <template<class> class MapType, class... Args>
		class TestNormalization {
		private:
			MapType<str::detail::MatchCollector> pMapper;

		public:
			constexpr TestNormalization(Args... args) : pMapper{ str::detail::MatchCollector{}, args... } {}

		public:
			constexpr void next(char32_t cp) {
				str::detail::MatchCollector& match = pMapper.collector();
				if (match.matches) {
					match.expected.push(cp);
					pMapper.next(cp);
				}
			}
			constexpr bool done() {
				str::detail::MatchCollector& match = pMapper.collector();
				if (match.matches)
					pMapper.done();
				return match.completed();
			}
		};
	}

	/* [str::IsMapper] create a collector, which writes the decomposed-normalized (NFD) stream to the given collector
	*	Note: For rvalues, a local move-constructed value of the collector is held by the mapper,
	*	otherwise a reference is held and the mapper must not outlive the collector */
	class Decompose {
	public:
		template <str::IsCollector<char32_t> CollType>
		using Type = detail::DecompMapper<CollType>;

	public:
		constexpr Decompose() {}

	public:
		template <str::IsCollector<char32_t> CollType>
		constexpr Type<CollType> operator()(CollType&& collector) const {
			return Type<CollType>{ std::forward<CollType>(collector) };
		}
	};

	/* [str::IsMapper] create a collector, which writes the composed-normalized (NFC) stream to the given collector
	*	Note: For rvalues, a local move-constructed value of the collector is held by the mapper,
	*	otherwise a reference is held and the mapper must not outlive the collector */
	class Compose {
	public:
		template <str::IsCollector<char32_t> CollType>
		using Type = detail::CompMapper<CollType>;

	public:
		constexpr Compose() {}

	public:
		template <str::IsCollector<char32_t> CollType>
		constexpr Type<CollType> operator()(CollType&& collector) const {
			return Type<CollType>{ std::forward<CollType>(collector) };
		}
	};

	/* [str::IsMapper] create a collector, which writes the casefolded and decomposed-normalized (NFD) stream to the given collector
	*	(Creates canonical caseless mapping, which produces identical streams for canonically equivalent strings)
	*	Note: For rvalues, a local move-constructed value of the collector is held by the mapper,
	*	otherwise a reference is held and the mapper must not outlive the collector */
	class NormFold {
	public:
		template <str::IsCollector<char32_t> CollType>
		using Type = detail::NormFoldMapper<CollType>;

	private:
		detail::CaseLocale pLocale = detail::CaseLocale::none;

	public:
		constexpr NormFold(std::string_view locale = {}) {
			pLocale = detail::ParseCaseLocale(locale);
		}

	public:
		template <str::IsCollector<char32_t> CollType>
		constexpr Type<CollType> operator()(CollType&& collector) const {
			return Type<CollType>{ std::forward<CollType>(collector), pLocale };
		}
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is decomposed-normalized (NFD) (i.e. cp::Decompose(...) would result in the same codepoints) */
	class TestDecompose : public detail::TestNormalization<detail::DecompMapper> {
	public:
		constexpr TestDecompose() = default;
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is composed-normalized (NFC) (i.e. cp::Compose(...) would result in the same codepoints) */
	class TestCompose : public detail::TestNormalization<detail::CompMapper> {
	public:
		constexpr TestCompose() = default;
	};

	/* [str::IsAnalysis] check if the entire stream of codepoints is casefolded and decomposed-normalized (NFD) (i.e. cp::NormFold(...) would result in the same codepoints) */
	class TestNormFold : public detail::TestNormalization<detail::NormFoldMapper, detail::CaseLocale> {
	public:
		constexpr TestNormFold(std::string_view locale = {}) : detail::TestNormalization<detail::NormFoldMapper, detail::CaseLocale>{ detail::ParseCaseLocale(locale) } {}
	};
}
