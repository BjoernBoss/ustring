/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2025 Bjoern Boss Henrichsen */
#pragma once

#include <algorithm>
#include <cwchar>
#include <vector>
#include <variant>
#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>
#include <span>
#include <cstring>
#include <iterator>

namespace str {
	/* codepoint to indicate invalid decoding (guaranteed to be larger than any valid unicode-codepoint) */
	static constexpr char32_t Invalid = char32_t(-1);

	namespace detail {
		template <class ExpType, size_t ExpSize, class ActType, size_t ActSize>
		constexpr bool IsBufferSame(const ExpType(&expected)[ExpSize], const ActType(&actual)[ActSize]) {
			if (sizeof(ExpType) != sizeof(ActType) || ExpSize != ActSize)
				return false;

			size_t i = 0;
			while (i < ExpSize && expected[i] == static_cast<ExpType>(actual[i]))
				++i;

			return (i == ExpSize);
		}
		template <class ExpType, size_t ExpSize, class ActType, size_t ActSize>
		constexpr bool HoldSameValues(const ExpType(&expected)[ExpSize], const ActType(&actual)[ActSize]) {
			if (ExpSize != ActSize)
				return false;
			using LargeType = std::conditional_t<sizeof(ExpType) >= sizeof(ActType), ExpType, ActType>;

			size_t i = 0;
			while (i < ExpSize && static_cast<LargeType>(expected[i]) == static_cast<LargeType>(actual[i]))
				++i;

			return (i == ExpSize);
		}

		/* utf8 test-string: \U0000007f\U0000ff00\U00010000 */
		template <class Type, size_t Size>
		constexpr bool IsUtf8(const Type(&test)[Size]) {
			constexpr uint8_t expected[] = { 0x7f, 0xef, 0xbc, 0x80, 0xf0, 0x90, 0x80, 0x80, 0x00 };
			return detail::IsBufferSame(expected, test);
		};

		/* utf16 test-string: \U00010000\U0000ff00 */
		template <class Type, size_t Size>
		constexpr bool IsUtf16(const Type(&test)[Size]) {
			constexpr uint16_t expected[] = { 0xd800, 0xdc00, 0xff00, 0x0000 };
			return detail::IsBufferSame(expected, test);
		};

		/* utf32 test-string: \U00010000\U0000ff00 */
		template <class Type, size_t Size>
		constexpr bool IsUtf32(const Type(&test)[Size]) {
			constexpr uint32_t expected[] = { 0x10000, 0xff00, 0x0000 };
			return detail::IsBufferSame(expected, test);
		};

		static constexpr uint32_t SurrogateFirst = 0xd800;
		static constexpr uint32_t SurrogateUpper = 0xdc00;
		static constexpr uint32_t SurrogateLast = 0xdfff;
		static constexpr uint32_t UnicodeRange = 0x110000;
		static constexpr uint32_t AsciiRange = 0x80;


		/* check if the type is a character */
		template <class Type> struct TestChar { using type = void; };
		template <> struct TestChar<char> { using type = char; };
		template <> struct TestChar<wchar_t> { using type = wchar_t; };
		template <> struct TestChar<char8_t> { using type = char8_t; };
		template <> struct TestChar<char16_t> { using type = char16_t; };
		template <> struct TestChar<char32_t> { using type = char32_t; };

		template <class... Args>
		struct EmptyCollector {
			constexpr void next(Args...) {}
			constexpr void done() {}
		};
	}

	/* constant data to be used for wires and source */
	using Data = std::span<const uint8_t>;

	/* single decoded codepoint and the number of characters consumed for it */
	struct Decoded {
		char32_t cp = str::Invalid;
		uint32_t consumed = 0;
	};

	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsChar = !std::is_void_v<typename detail::TestChar<Type>::type>;

	/* codepoint-iterator is a bidirectional iterator, which iterates over char32_t */
	template <class Type>
	concept IsCPIterator = std::bidirectional_iterator<Type> && std::same_as<std::remove_cvref_t<typename std::iterator_traits<Type>::value_type>, char32_t>;

	/* receivers receive arbitrary many values of the given value-list */
	template <class Type, class... ValType>
	concept IsReceiver = requires(Type t, ValType... v) {
		{ t(v...) } -> std::same_as<void>;
	};

	/* valid collectors must receive zero or more valid arguments via next() and a final call to done(), after which
	*	the object is considered burnt (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type, class... ValType>
	concept IsCollector = requires(Type t, ValType... v) {
		{ t.next(v...) } -> std::same_as<void>;
		{ t.done() } -> std::same_as<void>;
	};

	/* codepoint-mapper must receive a collector to which it writes the mapped content and
	*	return a collector<char32_t> and expose the type of the new collector as ::Type (must
	*	propagate the input type forward and wrap it - if rvalue or lvalue) */
	template <class Type>
	concept IsMapper = requires(const Type t, detail::EmptyCollector<char32_t> c) {
		typename Type::template Type<detail::EmptyCollector<char32_t>>;
		{ t(std::move(c)) } -> std::same_as<typename Type::template Type<detail::EmptyCollector<char32_t>>>;
		{ t(c) } -> std::same_as<typename Type::template Type<detail::EmptyCollector<char32_t>&>>;
		{ t(c) } -> str::IsCollector<char32_t>;
	};

	/* valid analysis must receive zero or more valid codepoints via next() and a final call to done(),
	*	which returns the result of the analysis (the object is considered burnt afterwards) */
	template <class Type>
	concept IsAnalysis = requires(Type t, char32_t c) {
		{ t.next(c) } -> std::same_as<void>;
		{ t.done() } -> std::same_as<bool>;
	};

	/* valid tests must return a boolean flag for every passed-in codepoint */
	template <class Type>
	concept IsTester = requires(Type t, char32_t c) {
		{ t(c) } -> std::same_as<bool>;
	};

	/* any-string runtime-exception */
	template <str::IsChar ChType>
	class RuntimeException {
	private:
		std::basic_string<ChType> pMessage;

	public:
		RuntimeException(const std::basic_string<ChType>& msg) : pMessage{ msg } {}

	public:
		constexpr const std::basic_string<ChType>& what() const {
			return pMessage;
		}
	};
}
