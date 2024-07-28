/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "unicode/cp-casing.h"
#include "unicode/cp-normalization.h"
#include "unicode/cp-property.h"
#include "unicode/cp-segmentation.h"
#include "str-coding.h"
#include "str-escape.h"
#include "str-helper.h"

namespace str {
	template <str::IsChar ChType, char32_t CodeError>
	struct String;

	namespace detail {
		template <class ChType, class BaseType, char32_t CodeError, class SelfType>
		struct UWrapper;
	}

	/* wrap std::string_view to support the extended unicode-operations */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	struct View : public detail::UWrapper<ChType, std::basic_string_view<ChType>, CodeError, str::View<ChType, CodeError>> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string_view<ChType>, CodeError, str::View<ChType, CodeError>>;

	public:
		using Super::Super;
		View(const std::basic_string_view<ChType>& v) : Super{ v } {}
		View(std::basic_string_view<ChType>&& v) : Super{ v } {}

	public:
		template <char32_t OCodeError>
		View(const str::View<ChType, OCodeError>& v) : Super{ v } {}
		template <char32_t OCodeError>
		View(str::View<ChType, OCodeError>&& v) : Super{ v } {}

	public:
		template <char32_t OCodeError>
		View(const str::String<ChType, OCodeError>& v) : Super{ std::basic_string_view<ChType>{ v } } {}
		template <char32_t OCodeError>
		View(str::String<ChType, OCodeError>&& v) : Super{ std::basic_string_view<ChType>{ v } } {}
	};
	template <str::IsStr<char> Type>
	View(Type) -> View<char, err::DefChar>;
	template <str::IsStr<wchar_t> Type>
	View(Type) -> View<wchar_t, err::DefChar>;
	template <str::IsStr<char8_t> Type>
	View(Type) -> View<char8_t, err::DefChar>;
	template <str::IsStr<char16_t> Type>
	View(Type) -> View<char16_t, err::DefChar>;
	template <str::IsStr<char32_t> Type>
	View(Type) -> View<char32_t, err::DefChar>;

	/* wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, char32_t CodeError = err::DefChar>
	struct String : public detail::UWrapper<ChType, std::basic_string<ChType>, CodeError, str::String<ChType, CodeError>> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string<ChType>, CodeError, str::String<ChType, CodeError>>;

	public:
		using Super::Super;
		explicit String(const std::basic_string_view<ChType>& v) : Super{ v } {}
		explicit String(std::basic_string_view<ChType>&& v) : Super{ v } {}

	public:
		template <char32_t OCodeError>
		String(const str::String<ChType, OCodeError>& v) : Super{ v } {}
		template <char32_t OCodeError>
		String(str::String<ChType, OCodeError>&& v) : Super{ v } {}

	public:
		constexpr operator str::View<ChType, CodeError>() const {
			return str::View<ChType, CodeError>{ *static_cast<const std::basic_string<ChType>*>(this) };
		}
	};
	template <str::IsStr<char> Type>
	String(Type) -> String<char, err::DefChar>;
	template <str::IsStr<wchar_t> Type>
	String(Type) -> String<wchar_t, err::DefChar>;
	template <str::IsStr<char8_t> Type>
	String(Type) -> String<char8_t, err::DefChar>;
	template <str::IsStr<char16_t> Type>
	String(Type) -> String<char16_t, err::DefChar>;
	template <str::IsStr<char32_t> Type>
	String(Type) -> String<char32_t, err::DefChar>;

	namespace detail {
		template <class ChType, class BaseType, char32_t CodeError, class SelfType>
		struct UWrapper : public BaseType {
		public:
			/* character type of the current object */
			using CharType = ChType;

			/* string-view type of the current object */
			using ViewType = str::View<ChType, CodeError>;

			/* string type of the current object */
			using StrType = str::String<ChType, CodeError>;

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

			template <class AChType, class BChType, class... Transforms>
			static constexpr bool fCompare(const std::basic_string_view<AChType>& a, const std::basic_string_view<BChType>& b, Transforms&&... transforms) {
				bool valid = true;
				detail::LocalBuffer<char32_t> buffer;
				int8_t state = 0;

				/* construct the two iterators to iterate across the two strings */
				str::Iterator<AChType, CodeError> aIt{ a };
				str::Iterator<BChType, CodeError> bIt{ b };

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
					}), std::forward<Transforms>(transforms)...);
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
					}), std::forward<Transforms>(transforms)...);

				/* iterate over the codepoints and feed them to the transformation and compare the outputs */
				while (aIt.next()) {
					if (!bIt.next())
						return false;
					aTrans.next(aIt.get());
					bTrans.next(bIt.get());
					if (!valid)
						return false;
				}

				/* ensure that the other iterator has also reached its end and flush the transformations */
				if (bIt.next())
					return false;
				aTrans.done();
				bTrans.done();
				return valid;
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
				return str::TranscodeAll<SinkType, CodeError>(fBase());
			}

			/* convert the string to a string of the corresponding char-type or return a view, if this string and the destination type are effectively using the same encoding [such as char and char8_t] */
			template <str::IsChar OChType, char32_t OCodeError = CodeError>
			constexpr std::conditional_t<str::EffSame<ChType, OChType>, str::View<OChType, OCodeError>, str::String<OChType, OCodeError>> as() const {
				if constexpr (str::EffSame<ChType, OChType>) {
					std::basic_string_view<ChType> _this = fBase();
					return str::View<OChType, OCodeError>{ std::basic_string_view<OChType>{ reinterpret_cast<const OChType*>(_this.data()), _this.size() } };
				}
				else
					return str::TranscodeAll<str::String<OChType, OCodeError>, CodeError>(fBase());
			}

			/* convert the string to the corresponding string-type but as an escaped string */
			template <str::AnySink SinkType>
			constexpr SinkType escape(bool compact = false) const {
				return str::EscapeAll<SinkType, CodeError>(fBase(), compact);
			}

			/* overwrite sub-string of base-type */
			constexpr SelfType substr(size_t pos = 0, size_t count = BaseType::npos) const {
				return SelfType{ fBase().substr(pos, count) };
			}

		public:
			/* convert the string to upper-case using cp::UpperCase */
			constexpr StrType upper(const std::wstring_view& locale = {}) const {
				StrType out;
				fApply(str::Collect(out), cp::UpperCase{ locale });
				return out;
			}

			/* convert the string to lower-case using cp::LowerCase */
			constexpr StrType lower(const std::wstring_view& locale = {}) const {
				StrType out;
				fApply(str::Collect(out), cp::LowerCase{ locale });
				return out;
			}

			/* convert the string to title-case using cp::TitleCase */
			StrType title(const std::wstring_view& locale = {}) const {
				StrType out;
				fApply(str::Collect(out), cp::TitleCase{ locale });
				return out;
			}

			/* convert the string to case-folded using cp::FoldCase */
			constexpr StrType fold(const std::wstring_view& locale = {}) const {
				StrType out;
				fApply(str::Collect(out), cp::FoldCase{ locale });
				return out;
			}

			/* convert the string to its composed normalization form (NFC) using cp::Compose */
			constexpr StrType compose() const {
				StrType out;
				fApply(str::Collect(out), cp::Compose{});
				return out;
			}

			/* convert the string to its decomposed normalization form (NFD) using cp::Decompose */
			constexpr StrType decompose() const {
				StrType out;
				fApply(str::Collect(out), cp::Decompose{});
				return out;
			}

			/* convert the string to its normalized form (NFD) using cp::Decompose */
			constexpr StrType norm() const {
				StrType out;
				fApply(str::Collect(out), cp::Decompose{});
				return out;
			}

			/* convert the string to its case-folded normalized form (NFD) using cp::NormFold */
			constexpr StrType inorm(const std::wstring_view& locale = {}) const {
				StrType out;
				fApply(str::Collect(out), cp::NormFold{ locale });
				return out;
			}

			/* convert the decimal digits into the string from any format to ascii 0-9 and leave the remaining characters unchanged */
			constexpr StrType asciiDecimals() const {
				StrType out;
				ItType it{ fBase() };

				/* iterate over the codepoints and either transform all decimal digits or simply forward the codepoints */
				while (it.next()) {
					size_t digit = cp::prop::GetDecimal(it.get());
					if (digit == cp::prop::ErrDecimal)
						str::CodepointTo<CodeError>(out, it.get());
					else
						str::CodepointTo<CodeError>(out, cp::ascii::GetRadixLower(digit));
				}
				return out;
			}

		public:
			/* test if the string is non-empty and upper-case using cp::TestUpperCase */
			constexpr bool isUpper(const std::wstring_view& locale = {}) const {
				return fAnalyze(cp::TestUpperCase{ locale });
			}

			/* test if the string is non-empty and lower-case using cp::TestLowerCase */
			constexpr bool isLower(const std::wstring_view& locale = {}) const {
				return fAnalyze(cp::TestLowerCase{ locale });
			}

			/* test if the string is non-empty and title-case using cp::TestTitleCase */
			constexpr bool isTitle(const std::wstring_view& locale = {}) const {
				return fAnalyze(cp::TestTitleCase{ locale });
			}

			/* test if the string is non-empty and case-folded using cp::TestFoldCase */
			constexpr bool isFold(const std::wstring_view& locale = {}) const {
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
			constexpr bool isINorm(const std::wstring_view& locale = {}) const {
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
			/* perform a normalized comparison of this string and the other string */
			constexpr bool ucompare(const str::AnyStr auto& str) const {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }, std::basic_string_view<OChType>{ str }, cp::Decompose{});
			}

			/* perform a normalized comparison of this string and the other string */
			constexpr bool ucompare(size_t pos1, size_t count1, const str::AnyStr auto& str) const {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }, cp::Decompose{});
			}

			/* perform a normalized comparison of this string and the other string */
			constexpr bool ucompare(size_t pos1, size_t count1, const str::AnyStr auto& str, size_t pos2, size_t count2) const {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }.substr(pos2, count2), cp::Decompose{});
			}

			/* perform a case-insensitive normalized comparison of this string and the other string */
			constexpr bool icompare(const str::AnyStr auto& str, const std::wstring_view& locale = {}) const {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }, std::basic_string_view<OChType>{ str }, cp::NormFold{ locale });
			}

			/* perform a case-insensitive normalized comparison of this string and the other string */
			constexpr bool icompare(size_t pos1, size_t count1, const str::AnyStr auto& str, const std::wstring_view& locale = {}) const {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }, cp::NormFold{ locale });
			}

			/* perform a case-insensitive normalized comparison of this string and the other string */
			constexpr bool icompare(size_t pos1, size_t count1, const str::AnyStr auto& str, size_t pos2, size_t count2, const std::wstring_view& locale = {}) const {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }.substr(pos1, count1), std::basic_string_view<OChType>{ str }.substr(pos2, count2), cp::NormFold{ locale });
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

			/* apply all of the transformations in nested order to this string and the other string and compare the two transformed outputs */
			constexpr bool transformEqual(const str::AnyStr auto& str, const str::IsMapper auto&... mapper) {
				using OChType = str::StrChar<decltype(str)>;
				return fCompare<ChType, OChType>(std::basic_string_view<ChType>{ fBase() }, std::basic_string_view<OChType>{ str }, mapper...);
			}

		public:
			/* setup a grapheme-iterator on the codepoint beneath the index */
			constexpr cp::GraphemeIterator<ItType> graphemes(size_t index = 0) const {
				ItType it{ fBase(), index };
				it.next();
				return cp::GraphemeIterator<ItType>{ it };
			}

			/* setup a word-iterator on the codepoint beneath the index */
			constexpr cp::WordIterator<ItType> words(size_t index = 0) const {
				ItType it{ fBase(), index };
				it.next();
				return cp::WordIterator<ItType>{ it };
			}

			/* setup a sentence-iterator on the codepoint beneath the index */
			constexpr cp::SentenceIterator<ItType> sentences(size_t index = 0) const {
				ItType it{ fBase(), index };
				it.next();
				return cp::SentenceIterator<ItType>{ it };
			}

			/* setup a line-iterator on the codepoint beneath the index */
			constexpr cp::LineIterator<ItType> lines(bool emergencyBreak = true, bool graphemeAware = true, size_t index = 0) const {
				ItType it{ fBase(), index };
				it.next();
				return cp::LineIterator<ItType>{ it, emergencyBreak, graphemeAware };
			}
		};
	}

	/* specializations for char-writers to support String */
	template <class ChType, char32_t CodeError>
	struct CharWriter<str::String<ChType, CodeError>, ChType> {
		constexpr void operator()(str::String<ChType, CodeError>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(str::String<ChType, CodeError>& sink, const ChType* str, size_t size) const {
			sink.append(str, size);
		}
	};
}
