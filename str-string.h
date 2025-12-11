/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2025 Bjoern Boss Henrichsen */
#pragma once

#include "unicode/cp-casing.h"
#include "unicode/cp-normalization.h"
#include "unicode/cp-property.h"
#include "unicode/cp-segmentation.h"
#include "coding/str-coding.h"
#include "format/str-escape.h"

/*
*	Coding-Rules:
*	 - Uses str::FastcodeAll<Error> by default and otherwise uses str::TranscodeAll<Error> on explicit selection
*/
namespace str {
	/* [str::IsCollector] collect the sequence of codepoints into the corresponding sink
	*	Note: Must not outlive the sink object as it may store a reference to it
	*	Note: For rvalues, a local move-constructed value of Type is held, otherwise a reference is held */
	template <str::IsSink SinkType>
	struct Collect {
	private:
		SinkType pSink;

	public:
		constexpr Collect(SinkType&& sink) : pSink{ std::forward<SinkType>(sink) } {}

	public:
		constexpr void next(char32_t cp) {
			str::CodepointTo(pSink, cp, 1);
		}
		constexpr void done() {}
	};
	template <class Type> Collect(Type&) -> Collect<Type&>;
	template <class Type> Collect(Type&&) -> Collect<Type>;

	/* [str::IsCollector] collect the sequence of codepoints and pass them to the corresponding callable object
	*	Note: Must not outlive the sink object as it may store a reference to it
	*	Note: For rvalues, a local move-constructed value of Type is held, otherwise a reference is held */
	template <str::IsReceiver<char32_t> CallType>
	struct ForEach {
	private:
		CallType pSink;

	public:
		constexpr ForEach(CallType&& sink) : pSink{ std::forward<CallType>(sink) } {}

	public:
		constexpr void next(char32_t cp) {
			pSink(cp);
		}
		constexpr void done() {}
	};
	template <class Type> ForEach(Type&) -> ForEach<Type&>;
	template <class Type> ForEach(Type&&) -> ForEach<Type>;

	template <str::IsChar ChType, str::CodeError>
	struct String;

	namespace detail {
		template <class ChType, class BaseType, str::CodeError, class SelfType>
		struct UWrapper;
	}

	/* [str::IsStr] wrap std::string_view to support the extended unicode-operations */
	template <str::IsChar ChType, str::CodeError Error = str::CodeError::replace>
	struct View : public detail::UWrapper<ChType, std::basic_string_view<ChType>, Error, str::View<ChType, Error>> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string_view<ChType>, Error, str::View<ChType, Error>>;

	public:
		using Super::Super;
		View(const std::basic_string_view<ChType>& v) : Super{ v } {}
		View(std::basic_string_view<ChType>&& v) : Super{ std::move(v) } {}

	public:
		template <str::CodeError OError>
		View(const str::View<ChType, OError>& v) : Super{ v } {}
		template <str::CodeError OError>
		View(str::View<ChType, OError>&& v) : Super{ std::move(v) } {}

	public:
		template <str::CodeError OError>
		View(const str::String<ChType, OError>& v) : Super{ std::basic_string_view<ChType>{ v } } {}
		template <str::CodeError OError>
		View(str::String<ChType, OError>&& v) : Super{ std::basic_string_view<ChType>{ v } } {}
	};
	template <str::IsChStr<char> Type> View(Type) -> View<char, str::CodeError::replace>;
	template <str::IsChStr<wchar_t> Type> View(Type) -> View<wchar_t, str::CodeError::replace>;
	template <str::IsChStr<char8_t> Type> View(Type) -> View<char8_t, str::CodeError::replace>;
	template <str::IsChStr<char16_t> Type> View(Type) -> View<char16_t, str::CodeError::replace>;
	template <str::IsChStr<char32_t> Type> View(Type) -> View<char32_t, str::CodeError::replace>;

	/* [str::IsStr/str::IsSink] wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, str::CodeError Error = str::CodeError::replace>
	struct String : public detail::UWrapper<ChType, std::basic_string<ChType>, Error, str::String<ChType, Error>> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string<ChType>, Error, str::String<ChType, Error>>;

	public:
		using Super::Super;
		explicit String(const std::basic_string_view<ChType>& v) : Super{ v } {}
		explicit String(std::basic_string_view<ChType>&& v) : Super{ std::move(v) } {}

	public:
		template <str::CodeError OError>
		String(const str::String<ChType, OError>& v) : Super{ v } {}
		template <str::CodeError OError>
		String(str::String<ChType, OError>&& v) : Super{ std::move(v) } {}

	public:
		constexpr operator str::View<ChType, Error>() const {
			return str::View<ChType, Error>{ *static_cast<const std::basic_string<ChType>*>(this) };
		}
	};
	template <str::IsChStr<char> Type> String(Type) -> String<char, str::CodeError::replace>;
	template <str::IsChStr<wchar_t> Type> String(Type) -> String<wchar_t, str::CodeError::replace>;
	template <str::IsChStr<char8_t> Type> String(Type) -> String<char8_t, str::CodeError::replace>;
	template <str::IsChStr<char16_t> Type> String(Type) -> String<char16_t, str::CodeError::replace>;
	template <str::IsChStr<char32_t> Type> String(Type) -> String<char32_t, str::CodeError::replace>;

	namespace detail {
		template <class ChType, class BaseType, str::CodeError Error, class SelfType>
		struct UWrapper : public BaseType {
			template <class, class, str::CodeError, class> friend struct detail::UWrapper;
		public:
			/* character type of the current object */
			using CharType = ChType;

			/* string-view type of the current object */
			using ViewType = str::View<ChType, Error>;

			/* string type of the current object */
			using StrType = str::String<ChType, Error>;

			/* codepoint-iterator type of the current type */
			using ItType = str::Iterator<ChType, Error>;

		public:
			using BaseType::BaseType;
			UWrapper(const BaseType& v) : BaseType{ v } {}
			UWrapper(BaseType&& v) : BaseType{ std::move(v) } {}

		private:
			constexpr const BaseType& fBase() const {
				return *static_cast<const BaseType*>(this);
			}
			constexpr BaseType& fBase() {
				return *static_cast<BaseType*>(this);
			}

		private:
			template <class CollType, class... Transforms>
			struct TransTypeRec;
			template <class CollType, class Transform, class... Transforms>
			struct TransTypeRec<CollType, Transform, Transforms...> { using type = typename Transform::template Type<typename TransTypeRec<CollType, Transforms...>::type>; };
			template <class CollType>
			struct TransTypeRec<CollType> { using type = CollType; };

			/* final type of lambda for all transforms applied to the final collector */
			template <class CollType, class... Transforms>
			using TransType = typename TransTypeRec<CollType, std::remove_cvref_t<Transforms>...>::type;

			/* construct the final transform to apply all transformations and write it to the final collector */
			template <class CollType>
			static constexpr TransType<CollType> fTransform(CollType&& collector) {
				return std::forward<CollType>(collector);
			}
			template <class CollType, class Transform, class... Transforms>
			static constexpr TransType<CollType, Transform, Transforms...> fTransform(CollType&& collector, const Transform& transform, const Transforms&... transforms) {
				if constexpr (sizeof...(Transforms) == 0)
					return transform(std::forward<CollType>(collector));
				else
					return transform(fTransform<CollType, Transforms...>(std::forward<CollType>(collector), transforms...));
			}

			/* iterate over all codepoints of the current object and apply the given transformation and collector to it */
			template <class CollType, class... Transforms>
			constexpr void fApply(CollType&& collector, const Transforms&... transforms) const {
				TransType<CollType, Transforms...> transform = fTransform<CollType, Transforms...>(std::forward<CollType>(collector), transforms...);
				ItType it{ fBase() };

				/* pass all codepoints into the transformation */
				while (it.valid())
					transform.next(it.next());
				transform.done();
			}

			/* iterate over all codepoints of the current object and pass them to the analysis */
			template <class AnType>
			constexpr bool fAnalyze(AnType&& analysis) const {
				if (fBase().empty())
					return false;
				ItType it{ fBase() };

				/* pass all codepoints into the analysis */
				while (it.valid())
					analysis.next(it.next());
				return analysis.done();
			}

			/* iterate over all codepoints of the current object and check if they all fulfill the tester-requirements */
			template <class TsType>
			constexpr bool fTest(const TsType& tester) const {
				if (fBase().empty())
					return false;
				ItType it{ fBase() };

				/* pass all codepoints to the tester */
				while (it.valid()) {
					if (!tester(it.next()))
						return false;
				}
				return true;
			}

			/* iterate over all codepoints of the two strings, apply the transformations, and check if the strings match */
			template <class AChType, class BChType, class... Transforms>
			static bool fCompare(const std::basic_string_view<AChType>& a, const std::basic_string_view<BChType>& b, const Transforms&... transforms) {
				detail::LocalBuffer<char32_t> buffer;
				int8_t state = 0;
				bool valid = true;

				/* construct the two iterators to iterate across the two strings */
				str::Iterator<AChType, Error> aIt{ a };
				str::Iterator<BChType, Error> bIt{ b };

				/* instantiate the two transformations, which compare their produced codepoints to the cached last codepoints */
				auto aTrans = fTransform(str::ForEach([&](char32_t cp) {
					if (state >= 0) {
						buffer.push(cp);
						state = 1;
					}
					else if (buffer.front() != cp)
						valid = false;
					else {
						buffer.pop();
						if (buffer.size() == 0)
							state = 0;
					}
					}), transforms...);
				auto bTrans = fTransform(str::ForEach([&](char32_t cp) {
					if (state <= 0) {
						buffer.push(cp);
						state = -1;
					}
					else if (buffer.front() != cp)
						valid = false;
					else {
						buffer.pop();
						if (buffer.size() == 0)
							state = 0;
					}
					}), transforms...);

				/* iterate over the codepoints and feed them to the transformation and compare the outputs */
				while (aIt.valid()) {
					if (!bIt.valid())
						return false;
					aTrans.next(aIt.next());
					bTrans.next(bIt.next());
					if (!valid)
						return false;
				}

				/* ensure that the other iterator has also reached its end and flush the transformations */
				if (bIt.valid())
					return false;
				aTrans.done();
				bTrans.done();
				return valid;
			}

			/* skip the first codepoints until the tester returns false */
			template <class TsType>
			constexpr ViewType fLeftStrip(const TsType& tester) const {
				ItType it{ fBase(), 0, false };

				/* iterate over the codepoints and look for the first fail */
				while (it.valid()) {
					if (!tester(it.get()))
						return ViewType{ fBase() }.substr(it.base());
					it.advance();
				}
				return ViewType{};
			}

			/* skip the last codepoints until the tester returns false */
			template <class TsType>
			constexpr ViewType fRightStrip(const TsType& tester) const {
				size_t lastEnd = fBase().size();
				ItType it{ fBase(), lastEnd, true };

				/* iterate over the codepoints and look for the first fail */
				while (it.valid()) {
					if (!tester(it.get()))
						return ViewType{ fBase() }.substr(0, lastEnd);
					lastEnd = it.base();
					it.reverse();
				}
				return ViewType{};
			}

		public:
			/* fetch the underlying string-object */
			constexpr BaseType& base() {
				return fBase();
			}

			/* fetch the underlying string-object */
			constexpr const BaseType& base() const {
				return fBase();
			}

			/* fetch the codepoint iterator for the string */
			constexpr ItType it(size_t index = 0, bool previous = false) const {
				return ItType{ fBase(), index, previous };
			}

			/* convert the string to the corrsponding string-type as fast as possible (either fast but potentially incorrect, or slower but correct) */
			template <str::IsSink SinkType>
			constexpr SinkType to(bool fast = true) const {
				if (fast)
					return str::FastcodeAll<SinkType, Error>(fBase());
				return str::TranscodeAll<SinkType, Error>(fBase());
			}

			/* convert the string to a string of the corresponding character type (either fast but potentially incorrect, or slower but correct) */
			template <str::IsChar OChType = ChType, str::CodeError OError = Error>
			constexpr str::String<OChType, OError> str(bool fast = true) const {
				if (fast)
					return str::FastcodeAll<str::String<OChType, OError>, Error>(fBase());
				return str::TranscodeAll<str::String<OChType, OError>, Error>(fBase());
			}

			/* convert the string to a string of the corresponding char-type or return a view, if this string and the destination type are effectively using the same encoding [such as char and char8_t] */
			template <str::IsChar OChType, str::CodeError OError = Error>
			constexpr std::conditional_t<str::EffSame<ChType, OChType>, str::View<OChType, OError>, str::String<OChType, OError>> as() const {
				if constexpr (str::EffSame<ChType, OChType>) {
					std::basic_string_view<ChType> _this = fBase();
					return str::View<OChType, OError>{ std::basic_string_view<OChType>{ reinterpret_cast<const OChType*>(_this.data()), _this.size() } };
				}
				else
					return str::FastcodeAll<str::String<OChType, OError>, Error>(fBase());
			}

			/* convert the string to the corresponding string-type but as an escaped string */
			template <str::IsSink SinkType>
			constexpr SinkType escape(bool compact = false) const {
				return str::EscapeAll<SinkType, Error>(fBase(), compact);
			}

			/* overwrite sub-string of base-type */
			constexpr SelfType substr(size_t pos = 0, size_t count = BaseType::npos) const {
				return SelfType{ fBase().substr(pos, count) };
			}

		public:
			/* strip any leading characters, which fulfill [prop::IsSpace] */
			constexpr SelfType lstrip() const {
				return SelfType{ fLeftStrip(cp::prop::IsSpace) };
			}

			/* strip any leading characters, which fulfill the tester */
			constexpr SelfType lstrip(const str::IsTester auto& tester) const {
				return SelfType{ fLeftStrip(tester) };
			}

			/* strip any trailing characters, which fulfill [prop::IsSpace] */
			constexpr SelfType rstrip() const {
				return SelfType{ fRightStrip(cp::prop::IsSpace) };
			}

			/* strip any trailing characters, which fulfill the tester */
			constexpr SelfType rstrip(const str::IsTester auto& tester) const {
				return SelfType{ fRightStrip(tester) };
			}

			/* strip any leading or trailing characters, which fulfill [prop::IsSpace] */
			constexpr SelfType strip() const {
				return SelfType{ fLeftStrip(cp::prop::IsSpace).fRightStrip(cp::prop::IsSpace) };
			}

			/* strip any leading or trailing characters, which fulfill the tester */
			constexpr SelfType strip(const str::IsTester auto& tester) const {
				return SelfType{ fLeftStrip(tester).fRightStrip(tester) };
			}

		public:
			/* convert the string to upper-case using cp::UpperCase */
			constexpr StrType upper(std::wstring_view locale = {}) const {
				StrType out;
				fApply(str::Collect{ out }, cp::UpperCase{ locale });
				return out;
			}

			/* convert the string to lower-case using cp::LowerCase */
			constexpr StrType lower(std::wstring_view locale = {}) const {
				StrType out;
				fApply(str::Collect{ out }, cp::LowerCase{ locale });
				return out;
			}

			/* convert the string to title-case using cp::TitleCase */
			StrType title(std::wstring_view locale = {}) const {
				StrType out;
				fApply(str::Collect{ out }, cp::TitleCase{ locale });
				return out;
			}

			/* convert the string to case-folded using cp::FoldCase */
			constexpr StrType fold(std::wstring_view locale = {}) const {
				StrType out;
				fApply(str::Collect{ out }, cp::FoldCase{ locale });
				return out;
			}

			/* convert the string to its composed normalization form (NFC) using cp::Compose */
			constexpr StrType compose() const {
				StrType out;
				fApply(str::Collect{ out }, cp::Compose{});
				return out;
			}

			/* convert the string to its decomposed normalization form (NFD) using cp::Decompose */
			constexpr StrType decompose() const {
				StrType out;
				fApply(str::Collect{ out }, cp::Decompose{});
				return out;
			}

			/* convert the string to its normalized form (NFD) using cp::Decompose */
			constexpr StrType norm() const {
				StrType out;
				fApply(str::Collect{ out }, cp::Decompose{});
				return out;
			}

			/* convert the string to its case-folded normalized form (NFD) using cp::NormFold */
			constexpr StrType inorm(std::wstring_view locale = {}) const {
				StrType out;
				fApply(str::Collect{ out }, cp::NormFold{ locale });
				return out;
			}

			/* convert the decimal digits into the string from any format to ascii 0-9 and leave the remaining characters unchanged */
			constexpr StrType asciiDecimals() const {
				StrType out;
				ItType it{ fBase() };

				/* iterate over the codepoints and either transform all decimal digits or simply forward the codepoints */
				while (it.valid()) {
					char32_t cp = it.next();
					size_t digit = cp::prop::GetDecimal(cp);
					if (digit == cp::prop::ErrDecimal)
						str::CodepointTo<Error>(out, cp);
					else
						str::CodepointTo<Error>(out, cp::ascii::GetRadixLower(digit));
				}
				return out;
			}

		public:
			/* test if the string is non-empty and upper-case using cp::TestUpperCase */
			constexpr bool isUpper(std::wstring_view locale = {}) const {
				return fAnalyze(cp::TestUpperCase{ locale });
			}

			/* test if the string is non-empty and lower-case using cp::TestLowerCase */
			constexpr bool isLower(std::wstring_view locale = {}) const {
				return fAnalyze(cp::TestLowerCase{ locale });
			}

			/* test if the string is non-empty and title-case using cp::TestTitleCase */
			constexpr bool isTitle(std::wstring_view locale = {}) const {
				return fAnalyze(cp::TestTitleCase{ locale });
			}

			/* test if the string is non-empty and case-folded using cp::TestFoldCase */
			constexpr bool isFold(std::wstring_view locale = {}) const {
				return fAnalyze(cp::TestFoldCase{ locale });
			}

			/* test if the string is non-empty and in composed normalization form (NFC) using cp::TestCompose */
			constexpr bool isComposed() const {
				return fAnalyze(cp::TestCompose{});
			}

			/* test if the string is non-empty and in decomposed normalization form (NFD) using cp::TestDecompose */
			constexpr bool isDecomposed() const {
				return fAnalyze(cp::TestDecompose{});
			}

			/* test if the string is non-empty and is normalized (NFD) using cp::TestDecompose */
			constexpr bool isNorm() const {
				return fAnalyze(cp::TestDecompose{});
			}

			/* test if the string is non-empty and is case-folded normalized (NFD) using cp::TestNormFold */
			constexpr bool isINorm(std::wstring_view locale = {}) const {
				return fAnalyze(cp::TestNormFold{ locale });
			}

			/* test if the string is non-empty and represents an emoji using cp::TestEmoji */
			constexpr bool isEmoji(bool graphical = true, bool text = false) const {
				return fAnalyze(cp::TestEmoji{ graphical, text });
			}

		public:
			/* test if every codepoint in the string can be decoded and is considered valid (can be empty) */
			constexpr bool isValid() const {
				str::Iterator<ChType, str::CodeError::nothing> it{ fBase() };
				while (it.valid()) {
					if (it.next() == str::Invalid)
						return false;
				}
				return true;
			}

			/* test if the given index is an aligned codepoint edge (start of a valid codepoint) */
			constexpr bool isAligned(size_t index) const {
				if (index >= fBase().size())
					return false;
				return str::IsCodepoint(std::basic_string_view<ChType>{ fBase() }.substr(index));
			}

			/* test if the string is non-empty and every codepoint in the string is ascii using prop::IsAscii */
			constexpr bool isAscii() const {
				return fTest(cp::prop::IsAscii);
			}

			/* test if the string is non-empty and every codepoint in the string is assigned using prop::IsAssigned */
			constexpr bool isAssigned() const {
				return fTest(cp::prop::IsAssigned);
			}

			/* test if the string is non-empty and every codepoint in the string is space using prop::IsSpace */
			constexpr bool isSpace() const {
				return fTest(cp::prop::IsSpace);
			}

			/* test if the string is non-empty and every codepoint in the string is a control character using prop::IsControl */
			constexpr bool isControl() const {
				return fTest(cp::prop::IsControl);
			}

			/* test if the string is non-empty and every codepoint in the string is an alphabetical character using prop::IsAlpha */
			constexpr bool isAlpha() const {
				return fTest(cp::prop::IsAlpha);
			}

			/* test if the string is non-empty and every codepoint in the string is a numeric character using prop::IsNumeric */
			constexpr bool isNumeric() const {
				return fTest(cp::prop::IsNumeric);
			}

			/* test if the string is non-empty and every codepoint in the string is an alphabetical or numeric character using prop::IsAlNum */
			constexpr bool isAlNum() const {
				return fTest(cp::prop::IsAlNum);
			}

			/* test if the string is non-empty and every codepoint in the string is a printable or space using prop::IsPrint */
			constexpr bool isPrint(bool anySpace = true) const {
				return fTest([=](char32_t cp) { return cp::prop::IsPrint(cp, anySpace); });
			}

			/* test if the string is non-empty and every codepoint in the string is graphical using prop::IsGraphic */
			constexpr bool isGraphic() const {
				return fTest(cp::prop::IsGraphic);
			}

		public:
			/* perform a normalized comparison of this string and the other string */
			constexpr bool ucompare(const str::IsStr auto& str) const {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }, std::basic_string_view<OChType>{ str }, cp::Decompose{});
			}

			/* perform a normalized comparison of this string and the other string */
			constexpr bool ucompare(size_t pos1, size_t count1, const str::IsStr auto& str) const {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }, cp::Decompose{});
			}

			/* perform a normalized comparison of this string and the other string */
			constexpr bool ucompare(size_t pos1, size_t count1, const str::IsStr auto& str, size_t pos2, size_t count2) const {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }.substr(pos2, count2), cp::Decompose{});
			}

			/* perform a case-insensitive normalized comparison of this string and the other string */
			constexpr bool icompare(const str::IsStr auto& str, std::wstring_view locale = {}) const {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }, std::basic_string_view<OChType>{ str }, cp::NormFold{ locale });
			}

			/* perform a case-insensitive normalized comparison of this string and the other string */
			constexpr bool icompare(size_t pos1, size_t count1, const str::IsStr auto& str, std::wstring_view locale = {}) const {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }, cp::NormFold{ locale });
			}

			/* perform a case-insensitive normalized comparison of this string and the other string */
			constexpr bool icompare(size_t pos1, size_t count1, const str::IsStr auto& str, size_t pos2, size_t count2, std::wstring_view locale = {}) const {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }.substr(pos2, count2), cp::NormFold{ locale });
			}

		public:
			/* apply all of the transformations in nested order and write the result to the collector */
			template <str::IsCollector CollType>
			constexpr void transformTo(CollType&& collector, const str::IsMapper auto&... mapper) {
				fApply(std::forward<CollType>(collector), mapper...);
			}

			/* apply all of the transformations in nested order and write the result to an object of the given type and return it */
			template <str::IsSink SinkType>
			constexpr SinkType transform(const str::IsMapper auto&... mapper) {
				SinkType out{};
				fApply(str::Collect{ out }, mapper...);
				return out;
			}

			/* test if the string is non-empty and fulfills the analysis after having the transformations applied in nested order */
			constexpr bool analyze(str::IsAnalysis auto&& analysis, const str::IsMapper auto&... mapper) {
				if (fBase().empty())
					return false;
				fApply(str::ForEach([&](char32_t cp) { analysis.next(cp); }), mapper...);
				return analysis.done();
			}

			/* test if the string is non-empty and fulfills the tester after having the transformations applied in nested order */
			constexpr bool test(const str::IsTester auto& tester, const str::IsMapper auto&... mapper) {
				if (fBase().empty())
					return false;
				bool valid = true;
				fApply(str::ForEach([&](char32_t cp) { valid && (valid = tester(cp)); }), mapper...);
				return valid;
			}

			/* apply all of the transformations in nested order to this string and the other string and compare the two transformed outputs */
			constexpr bool transformEqual(const str::IsStr auto& str, const str::IsMapper auto&... mapper) {
				using OChType = str::StringChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }, std::basic_string_view<OChType>{ str }, mapper...);
			}

		public:
			/* setup a grapheme-iterator on the codepoint beneath the index */
			constexpr cp::GraphemeIterator<ItType> graphemes(size_t index = 0) const {
				ItType it{ fBase(), index };
				return cp::GraphemeIterator<ItType>{ it };
			}

			/* setup a word-iterator on the codepoint beneath the index */
			constexpr cp::WordIterator<ItType> words(size_t index = 0) const {
				ItType it{ fBase(), index };
				return cp::WordIterator<ItType>{ it };
			}

			/* setup a sentence-iterator on the codepoint beneath the index */
			constexpr cp::SentenceIterator<ItType> sentences(size_t index = 0) const {
				ItType it{ fBase(), index };
				return cp::SentenceIterator<ItType>{ it };
			}

			/* setup a line-iterator on the codepoint beneath the index */
			constexpr cp::LineIterator<ItType> lines(bool emergencyBreak = true, bool graphemeAware = true, size_t index = 0) const {
				ItType it{ fBase(), index };
				return cp::LineIterator<ItType>{ it, emergencyBreak, graphemeAware };
			}
		};
	}
}
