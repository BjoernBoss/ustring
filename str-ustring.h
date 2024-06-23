#pragma once

#include "encode/str-encode.h"

namespace str {
	namespace detail {
		template <class Type, class BaseType, uint64_t ItHandling>
		struct UWrapper : public BaseType {
		public:
			/* character type of the current object */
			using Char = Type;

			/* string-view type of the current object */
			using View = std::basic_string_view<Type>;

			/* string type of the current object */
			using String = std::basic_string<Type>;

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

		public:
			/* convert the string to the corrsponding character-type (std::string) */
			template <str::IsChar ChType>
			constexpr std::basic_string<ChType> to() const {
				if constexpr (std::is_same_v<Type, ChType>)
					return std::basic_string<ChType>{ fBase() };
				else {
					std::basic_string<ChType> out;

					/* iterate over the codepoints and encode them to the destination-type */
					Iter it{ fBase() };
					while (it.next())
						out.append(str::Encode<ChType>(it.get()));
					return out;
				}
			}

			/* convert the string to the corrsponding character-type (str::Local<Capacity>) */
			template <str::IsChar ChType, intptr_t Capacity>
			constexpr str::Local<ChType, Capacity> to() const {
				if constexpr (std::is_same_v<Type, ChType>)
					return str::Local<ChType, Capacity>{ fBase() };
				else {
					str::Local<ChType, Capacity> out;

					/* iterate over the codepoints and encode them to the destination-type */
					Iter it{ fBase() };
					while (it.next())
						out.append(str::Encode<ChType>(it.get()));
					return out;
				}
			}

			/* fetch the codepoint iterator for the string */
			constexpr Iter it() const {
				return Iter{ fBase() };
			}


		};
	}

	/* wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, uint64_t ItHandling>
	struct UView : public detail::UWrapper<ChType, std::basic_string_view<ChType>, ItHandling> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string_view<ChType>, ItHandling>;

	public:
		using Super::Super;
		UView(const std::basic_string_view<ChType>& v) : Super{ v } {}
		UView(std::basic_string_view<ChType>&& v) : Super{ v } {}
	};

	/* wrap std::string to support the extended unicode-operations */
	template <str::IsChar ChType, uint64_t ItHandling>
	struct UStr : public detail::UWrapper<ChType, std::basic_string<ChType>, ItHandling> {
	private:
		using Super = detail::UWrapper<ChType, std::basic_string<ChType>, ItHandling>;

	public:
		using Super::Super;
		UStr(const std::basic_string_view<ChType>& v) : Super{ v } {}
		UStr(std::basic_string_view<ChType>&& v) : Super{ v } {}
	};

	/* UView-template deduction guides using default error-character for all decoding-operations */
	template <str::IsString<char> Type>
	UView(Type) -> UView<char, str::DefErrorChar>;
	template <str::IsString<wchar_t> Type>
	UView(Type) -> UView<wchar_t, str::DefErrorChar>;
	template <str::IsString<char8_t> Type>
	UView(Type) -> UView<char8_t, str::DefErrorChar>;
	template <str::IsString<char16_t> Type>
	UView(Type) -> UView<char16_t, str::DefErrorChar>;
	template <str::IsString<char32_t> Type>
	UView(Type) -> UView<char32_t, str::DefErrorChar>;

	/* UStr-template deduction guides using default error-character for all decoding-operations */
	template <str::IsString<char> Type>
	UStr(Type) -> UStr<char, str::DefErrorChar>;
	template <str::IsString<wchar_t> Type>
	UStr(Type) -> UStr<wchar_t, str::DefErrorChar>;
	template <str::IsString<char8_t> Type>
	UStr(Type) -> UStr<char8_t, str::DefErrorChar>;
	template <str::IsString<char16_t> Type>
	UStr(Type) -> UStr<char16_t, str::DefErrorChar>;
	template <str::IsString<char32_t> Type>
	UStr(Type) -> UStr<char32_t, str::DefErrorChar>;

	/* simple default string-type to be used */
	using UString = str::UStr<char16_t, str::DefErrorChar>;
}
