#pragma once

#include "str-encode.h"

namespace str {
	namespace detail {
		template <class Type, class BaseType, uint64_t ItHandling>
		struct UWrapper : public BaseType {
		public:
			/* character type of the current object */
			using Char = Type;

			/* string-view type of the current object */
			using View = std::basic_string_view<Type>;
			using UView = detail::UWrapper<Type, std::basic_string_view<Type>, ItHandling>;

			/* string type of the current object */
			using String = std::basic_string<Type>;
			using UString = detail::UWrapper<Type, std::basic_string<Type>, ItHandling>;

			/* codepoint-iterator type of the current type */
			using Iter = str::Iterator<Type, ItHandling>;

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
			template <class SinkType, class... Transforms> struct TransTypeRec;
			template <class SinkType, class Transform, class... Transforms>
			struct TransTypeRec<SinkType, Transform, Transforms...> { using type = Transform::template Type<SinkType>; };
			template <class SinkType>
			struct TransTypeRec<SinkType> { using type = SinkType; };

			/* final type of lambda for all transforms applied to the final sink */
			template <class SinkType, class... Transforms>
			using TransType = typename TransTypeRec<SinkType, Transforms...>::type;

			/* construct the final transform to apply all transformations and write it to the final sink */
			template <class SinkType, class Transform, class... Transforms>
			static constexpr TransType<SinkType, Transform, Transforms...> fTransform(SinkType&& sink, Transform&& transform, Transforms&&... transforms) {
				if constexpr (sizeof...(Transforms) == 0)
					return transform.operator() < SinkType > (std::forward<SinkType>(sink));
				else
					return transform(fTransform<SinkType, Transforms...>(std::forward<SinkType>(sink), std::forward<Transforms>(transforms)...));
			}

		public:
			BaseType& base() {
				return fBase();
			}
			const BaseType& base() const {
				return fBase();
			}

		public:
			/* convert the string to the corrsponding character-type (std::string) */
			template <str::IsChar ChType>
			constexpr detail::UWrapper<ChType, std::basic_string<ChType>, ItHandling> to() const {
				if constexpr (std::is_same_v<Type, ChType>)
					return { fBase() };
				else {
					detail::UWrapper<ChType, std::basic_string<ChType>, ItHandling> out;

					/* iterate over the codepoints and encode them to the destination-type */
					Iter it{ fBase() };
					while (it.next())
						str::CodepointTo(out.base(), it.get(), 1);
					return out;
				}
			}

			/* convert the string to the corrsponding character-type (str::Local<Capacity>) */
			template <str::IsChar ChType, intptr_t Capacity>
			constexpr detail::UWrapper<ChType, str::Local<ChType, Capacity>, ItHandling> to() const {
				if constexpr (std::is_same_v<Type, ChType>)
					return { fBase() };
				else {
					detail::UWrapper<ChType, str::Local<ChType, Capacity>, ItHandling> out;

					/* iterate over the codepoints and encode them to the destination-type */
					Iter it{ fBase() };
					while (it.next())
						str::CodepointTo(out.base(), it.get(), 1);
					return out;
				}
			}

			/* fetch the codepoint iterator for the string */
			constexpr Iter it() const {
				return Iter{ fBase() };
			}

		public:
			constexpr UString upper() const {

				struct Collect {
				public:
					UString str;

				public:
					constexpr void next(char32_t cp) {
						str::CodepointTo(str.base(), cp, 1);
					}
					constexpr void done() {}
				} out;

				auto trans = fTransform(out, cp::UpperCase{});

				Iter it{ fBase() };
				while (it.next())
					trans.next(it.get());
				trans.done();

				return out.str;
			}
		};
	}

	/* wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, uint64_t ItHandling = str::DefErrorChar>
	struct UView : public detail::UWrapper<ChType, std::basic_string_view<ChType>, ItHandling> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string_view<ChType>, ItHandling>;

	public:
		using Super::Super;
		UView(const std::basic_string_view<ChType>& v) : Super{ v } {}
		UView(std::basic_string_view<ChType>&& v) : Super{ v } {}
	};

	/* wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, uint64_t ItHandling = str::DefErrorChar>
	struct UStr : public detail::UWrapper<ChType, std::basic_string<ChType>, ItHandling> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string<ChType>, ItHandling>;

	public:
		using Super::Super;
		UStr(const std::basic_string_view<ChType>& v) : Super{ v } {}
		UStr(std::basic_string_view<ChType>&& v) : Super{ v } {}
	};

	/* UView-template deduction guides using default error-character for all decoding-operations */
	template <str::IsStr<char> Type>
	UView(Type) -> UView<char, str::DefErrorChar>;
	template <str::IsStr<wchar_t> Type>
	UView(Type) -> UView<wchar_t, str::DefErrorChar>;
	template <str::IsStr<char8_t> Type>
	UView(Type) -> UView<char8_t, str::DefErrorChar>;
	template <str::IsStr<char16_t> Type>
	UView(Type) -> UView<char16_t, str::DefErrorChar>;
	template <str::IsStr<char32_t> Type>
	UView(Type) -> UView<char32_t, str::DefErrorChar>;

	/* UStr-template deduction guides using default error-character for all decoding-operations */
	template <str::IsStr<char> Type>
	UStr(Type) -> UStr<char, str::DefErrorChar>;
	template <str::IsStr<wchar_t> Type>
	UStr(Type) -> UStr<wchar_t, str::DefErrorChar>;
	template <str::IsStr<char8_t> Type>
	UStr(Type) -> UStr<char8_t, str::DefErrorChar>;
	template <str::IsStr<char16_t> Type>
	UStr(Type) -> UStr<char16_t, str::DefErrorChar>;
	template <str::IsStr<char32_t> Type>
	UStr(Type) -> UStr<char32_t, str::DefErrorChar>;

	/* simple default string-type to be used */
	using UString = str::UStr<char16_t, str::DefErrorChar>;
}
