#pragma once

#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>

namespace str {
	/* codepoint to indicate invalid decoding (guaranteed to be larger than any valid unicode-codepoint) */
	static constexpr char32_t Invalid = char32_t(-1);

	/* default error character (guaranteed to be an ascii-character) */
	static constexpr char32_t DefErrorChar = U'?';

	namespace detail {
		/* check if the type is a character */
		template <class Type> struct TestChar { using type = void; };
		template <> struct TestChar<char> { using type = char; };
		template <> struct TestChar<wchar_t> { using type = wchar_t; };
		template <> struct TestChar<char8_t> { using type = char8_t; };
		template <> struct TestChar<char16_t> { using type = char16_t; };
		template <> struct TestChar<char32_t> { using type = char32_t; };

		using EmptyLambda = decltype([](char32_t) {});
	}

	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsChar = !std::is_void_v<typename detail::TestChar<Type>::type>;

	/* string is anything convertible to a string-view */
	template <class Type, class ChType>
	concept IsString = requires(const Type & t) {
		{ t } -> std::convertible_to<std::basic_string_view<ChType>>;
	};

	/* valid sinks for char32_t must receive zero or more valid codepoints and a final call to done(), after which
	*	the object is considered burnt (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type, class... ValType>
	concept IsSink = requires(Type t, ValType... v) {
		{ t(v...) } -> std::same_as<void>;
	};

	/* codepoint-iterator must move itself upon prev()/next() and return true or return false
	*	(in which case it must stay) and must return the currently pointed to codepoint on get()
	*	iterator may first be initialized upon the first call to prev()/next() and should otherwise return str::Invalid */
	template <class Type>
	concept IsIterator = std::copyable<Type> && requires(Type t, const Type ct) {
		{ t.prev() } -> std::same_as<bool>;
		{ t.next() } -> std::same_as<bool>;
		{ ct.get() } -> std::same_as<char32_t>;
	};

	/* codepoint-mapper must receive a callable sink of type void(char32_t) and return an object, which provides next/done
	*	functions to consume the next codepoints and process them and the type of the object must be exposed as ::Type */
	template <class Type>
	concept IsMapper = requires(const Type t, char32_t c, detail::EmptyLambda l) {
		typename Type::template Type<detail::EmptyLambda>;
		{ t(l) } -> std::same_as<typename Type::template Type<detail::EmptyLambda>>;
		{ t(l).next(c) } -> std::same_as<void>;
		{ t(l).done() } -> std::same_as<void>;
	};
}
