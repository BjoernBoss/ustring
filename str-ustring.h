#pragma once

#include "unicode/cp-casing.h"
#include "unicode/cp-normalization.h"
#include "unicode/cp-property.h"
#include "unicode/cp-segmentation.h"
#include "str-encode.h"
#include "str-helper.h"

namespace str {
	namespace detail {
		template <class ChType, class BaseType, char32_t CodeError, class SelfType>
		struct UWrapper;
	}

	/* wrap std::string_view to support the extended unicode-operations */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	struct UView : public detail::UWrapper<ChType, std::basic_string_view<ChType>, CodeError, str::UView<ChType, CodeError>> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string_view<ChType>, CodeError, str::UView<ChType, CodeError>>;

	public:
		using Super::Super;
		UView(const std::basic_string_view<ChType>& v) : Super{ v } {}
		UView(std::basic_string_view<ChType>&& v) : Super{ v } {}
		template <char32_t OCodeError>
		UView(const str::UView<ChType, OCodeError>& v) : Super{ v } {}
		template <char32_t OCodeError>
		UView(str::UView<ChType, OCodeError>&& v) : Super{ v } {}
	};
	template <str::IsStr<char> Type>
	UView(Type) -> UView<char, err::DefChar>;
	template <str::IsStr<wchar_t> Type>
	UView(Type) -> UView<wchar_t, err::DefChar>;
	template <str::IsStr<char8_t> Type>
	UView(Type) -> UView<char8_t, err::DefChar>;
	template <str::IsStr<char16_t> Type>
	UView(Type) -> UView<char16_t, err::DefChar>;
	template <str::IsStr<char32_t> Type>
	UView(Type) -> UView<char32_t, err::DefChar>;

	/* wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	struct UStr : public detail::UWrapper<ChType, std::basic_string<ChType>, CodeError, str::UStr<ChType, CodeError>> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string<ChType>, CodeError, str::UStr<ChType, CodeError>>;

	public:
		using Super::Super;
		UStr(const std::basic_string_view<ChType>& v) : Super{ v } {}
		UStr(std::basic_string_view<ChType>&& v) : Super{ v } {}
		template <char32_t OCodeError>
		UStr(const str::UStr<ChType, OCodeError>& v) : Super{ v } {}
		template <char32_t OCodeError>
		UStr(str::UStr<ChType, OCodeError>&& v) : Super{ v } {}
	};
	template <str::IsStr<char> Type>
	UStr(Type) -> UStr<char, err::DefChar>;
	template <str::IsStr<wchar_t> Type>
	UStr(Type) -> UStr<wchar_t, err::DefChar>;
	template <str::IsStr<char8_t> Type>
	UStr(Type) -> UStr<char8_t, err::DefChar>;
	template <str::IsStr<char16_t> Type>
	UStr(Type) -> UStr<char16_t, err::DefChar>;
	template <str::IsStr<char32_t> Type>
	UStr(Type) -> UStr<char32_t, err::DefChar>;

	/* default string-type to be used [utf-16] */
	using UString = str::UStr<char16_t, err::DefChar>;

	namespace detail {
		template <class ChType, class BaseType, char32_t CodeError, class SelfType>
		struct UWrapper : public BaseType {
		public:
			/* character type of the current object */
			using CharType = ChType;

			/* string-view type of the current object */
			using UViewType = str::UView<ChType, CodeError>;

			/* string type of the current object */
			using UStrType = str::UStr<ChType, CodeError>;

			/* codepoint-iterator type of the current type */
			using ItType = str::Iterator<ChType, CodeError>;

		public:
			using BaseType::BaseType;
			UWrapper(const BaseType& v) : BaseType{ v } {}
			UWrapper(BaseType&& v) : BaseType{ v } {}

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
			struct TransTypeRec<CollType, Transform, Transforms...> { using type = Transform::template Type<typename TransTypeRec<CollType, Transforms...>::type>; };
			template <class CollType>
			struct TransTypeRec<CollType> { using type = CollType; };

			/* final type of lambda for all transforms applied to the final collector */
			template <class CollType, class... Transforms>
			using TransType = typename TransTypeRec<std::remove_cvref_t<CollType>, std::remove_cvref_t<Transforms>...>::type;

			/* construct the final transform to apply all transformations and write it to the final collector */
			template <class CollType>
			static constexpr TransType<CollType> fTransform(CollType&& collector) {
				return collector;
			}
			template <class CollType, class Transform, class... Transforms>
			static constexpr TransType<CollType, Transform, Transforms...> fTransform(CollType&& collector, Transform&& transform, Transforms&&... transforms) {
				if constexpr (sizeof...(Transforms) == 0)
					return transform(std::forward<CollType>(collector));
				else
					return transform(fTransform<CollType, Transforms...>(std::forward<CollType>(collector), std::forward<Transforms>(transforms)...));
			}

			/* iterate over all codepoints of the current object and apply the given transformation and collector to it */
			template <class CollType, class... Transforms>
			constexpr void fApply(CollType&& collector, Transforms&&... transforms) const {
				TransType<CollType, Transforms...> transform = fTransform<CollType, Transforms...>(std::forward<CollType>(collector), std::forward<Transforms>(transforms)...);
				ItType it{ fBase() };

				/* pass all codepoints into the transformation */
				while (it.next())
					transform.next(it.get());
				transform.done();
			}

			/* iterate over all codepoints of the current object and pass them to the analysis */
			template <class AnType>
			constexpr bool fAnalyze(AnType&& analysis) const {
				if (fBase().empty())
					return false;
				ItType it{ fBase() };

				/* pass all codepoints into the analysis */
				while (it.next())
					analysis.next(it.get());
				return analysis.done();
			}

			/* iterate over all codepoints of the current object and check if they all fulfill the tester-requirements */
			template <class TsType>
			constexpr bool fTest(TsType tester) const {
				if (fBase().empty())
					return false;
				ItType it{ fBase() };

				/* pass all codepoints to the tester */
				while (it.next()) {
					if (!tester(it.get()))
						return false;
				}
				return true;
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
			constexpr ItType it(size_t index = 0) const {
				return ItType{ fBase(), index };
			}

			/* convert the string to the corrsponding string-type */
			template <str::AnySink SinkType>
			constexpr SinkType to() const {
				return str::Transcode<SinkType>(fBase());
			}

			/* overwrite sub-string of base-type */
			constexpr SelfType substr(size_t pos = 0, size_t count = BaseType::npos) const {
				return SelfType{ fBase().substr(pos, count) };
			}

		public:
			/* convert the string to upper-case using cp::UpperCase */
			constexpr UStrType upper(const char8_t* locale = 0) const {
				UStrType out;
				fApply(str::Collect(out), cp::UpperCase{ locale });
				return out;
			}

			/* convert the string to lower-case using cp::LowerCase */
			constexpr UStrType lower(const char8_t* locale = 0) const {
				UStrType out;
				fApply(str::Collect(out), cp::LowerCase{ locale });
				return out;
			}

			/* convert the string to title-case using cp::TitleCase */
			UStrType title(const char8_t* locale = 0) const {
				UStrType out;
				fApply(str::Collect(out), cp::TitleCase{ locale });
				return out;
			}

			/* convert the string to case-folded using cp::FoldCase */
			constexpr UStrType fold(const char8_t* locale = 0) const {
				UStrType out;
				fApply(str::Collect(out), cp::FoldCase{ locale });
				return out;
			}

			/* convert the string to its composed normalization form (NFC) using cp::Compose */
			constexpr UStrType compose() const {
				UStrType out;
				fApply(str::Collect(out), cp::Compose{});
				return out;
			}

			/* convert the string to its decomposed normalization form (NFD) using cp::Decompose */
			constexpr UStrType decompose() const {
				UStrType out;
				fApply(str::Collect(out), cp::Decompose{});
				return out;
			}

			/* convert the string to its normalized form (NFD) using cp::Decompose */
			constexpr UStrType norm() const {
				UStrType out;
				fApply(str::Collect(out), cp::Decompose{});
				return out;
			}

			/* convert the string to its case-folded normalized form (NFD) using cp::NormFold */
			constexpr UStrType inorm(const char8_t* locale = 0) const {
				UStrType out;
				fApply(str::Collect(out), cp::NormFold{ locale });
				return out;
			}

		public:
			/* test if the string is non-empty and upper-case using cp::TestUpperCase */
			constexpr bool isUpper(const char8_t* locale = 0) const {
				return fAnalyze(cp::TestUpperCase{ locale });
			}

			/* test if the string is non-empty and lower-case using cp::TestLowerCase */
			constexpr bool isLower(const char8_t* locale = 0) const {
				return fAnalyze(cp::TestLowerCase{ locale });
			}

			/* test if the string is non-empty and title-case using cp::TestTitleCase */
			constexpr bool isTitle(const char8_t* locale = 0) const {
				return fAnalyze(cp::TestTitleCase{ locale });
			}

			/* test if the string is non-empty and case-folded using cp::TestFoldCase */
			constexpr bool isFold(const char8_t* locale = 0) const {
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
			constexpr bool isINorm(const char8_t* locale = 0) const {
				return fAnalyze(cp::TestNormFold{ locale });
			}

			/* test if the string is non-empty and represents an emoji using cp::TestEmoji */
			constexpr bool isEmoji(bool graphical = true, bool text = false) const {
				return fAnalyze(cp::TestEmoji{ graphical, text });
			}

		public:
			/* test if every codepoint in the string can be decoded and is considered valid (can be empty) */
			constexpr bool isValid() const {
				str::Iterator<ChType, err::Nothing> it{ fBase() };
				while (it.next()) {
					if (it.get() == str::Invalid)
						return false;
				}
				return true;
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
			/* apply all of the transformations in nested order and write the result to the collector */
			constexpr void transformTo(str::IsCollector auto&& collector, const str::IsMapper auto&... mapper) {
				fApply(collector, mapper...);
			}

			/* apply all of the transformations in nested order and write the result to an object of the given type and return it */
			template <str::AnySink SinkType>
			constexpr SinkType transform(const str::IsMapper auto&... mapper) {
				SinkType out{};
				fApply(str::Collect(out), mapper...);
				return out;
			}

			/* test if the string is non-empty and fulfills the analysis after having the transformations applied in nested order */
			constexpr bool analyze(str::IsAnalysis auto&& analysis, const str::IsMapper auto&... mapper) {
				if (fBase().empty())
					return false;
				fApply(str::ForEach([&](char32_t cp) { analysis.next(); }), mapper...);
				return analysis.done();
			}

			/* test if the string is non-empty and fulfills the tester after having the transformations applied in nested order */
			constexpr bool test(str::IsTester auto&& tester, const str::IsMapper auto&... mapper) {
				if (fBase().empty())
					return false;
				bool valid = true;
				fApply(str::ForEach([&](char32_t cp) { valid = valid && tester(cp); }), mapper...);
				return valid;
			}

		public:
			/* incomplete... */
			constexpr bool ucompare(const str::AnyStr auto& str) const {
				return false;
			}
			constexpr bool icompare(const str::AnyStr auto& str) const {
				return false;
			}
			constexpr cp::GraphemeIterator<ItType> graphemes(size_t index = 0) const {
				return cp::GraphemeIterator<ItType>{ ItType{ fBase(), index } };
			}
		};
	}

	/* specializations for char-writers to support UStr */
	template <class ChType, char32_t CodeError>
	struct CharWriter<str::UStr<ChType, CodeError>, ChType> {
		constexpr void operator()(str::UStr<ChType, CodeError>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(str::UStr<ChType, CodeError>& sink, const ChType* str, size_t size) const {
			sink.append(str, size);
		}
	};
}
