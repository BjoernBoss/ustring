/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include <stdexcept>
#include <algorithm>
#include <cwchar>
#include <vector>
#include <variant>
#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>

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

		struct EmptyCollector {
			constexpr void next(char32_t) {}
			constexpr void done() {}
		};

		/* check what string-type the character is */
		template <class Type> struct StrType { using type = void; };
		template <std::convertible_to<std::string_view> Type> struct StrType<Type> { using type = char; };
		template <std::convertible_to<std::wstring_view> Type> struct StrType<Type> { using type = wchar_t; };
		template <std::convertible_to<std::u8string_view> Type> struct StrType<Type> { using type = char8_t; };
		template <std::convertible_to<std::u16string_view> Type> struct StrType<Type> { using type = char16_t; };
		template <std::convertible_to<std::u32string_view> Type> struct StrType<Type> { using type = char32_t; };
	}

	/* constant data to be used for wires and source */
	struct Data {
	private:
		const uint8_t* pPtr = 0;
		size_t pSize = 0;

	public:
		constexpr Data() = default;
		constexpr Data(const uint8_t* p, size_t s) : pPtr{ p }, pSize{ s } {}
		constexpr Data(const str::Data&) = default;
		constexpr Data(str::Data&&) = default;
		constexpr str::Data& operator=(const str::Data&) = default;
		constexpr str::Data& operator=(str::Data&&) = default;

	public:
		constexpr str::Data subdata(size_t count) const {
			if (count >= pSize)
				return str::Data{};
			return str::Data{ pPtr + count, pSize - count };
		}
		constexpr const uint8_t* data() const {
			return pPtr;
		}
		constexpr size_t size() const {
			return pSize;
		}
		constexpr bool empty() const {
			return (pSize == 0);
		}
	};

	/* single decoded codepoint and the number of characters consumed for it */
	struct Decoded {
		char32_t cp = str::Invalid;
		uint32_t consumed = 0;
	};

	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsChar = !std::is_void_v<typename detail::TestChar<Type>::type>;

	/* character writable interface which requires:
	*	operator() to take the writable object and a character and a count (can be zero)
	*	operator() to take the writable object and a pointer and a size
	*	ChType: the character type of the writer */
	template <class Type>
	struct CharWriter;

	/* byte sink interface which requires:
	*	operator() to take the wire object and a pointer and a size */
	template <class Type>
	struct ByteWriter;

	/* specialize to make type available as character source for a stream (should not be interacted with directly, instead only use str::Stream) */
	template <class Type>
	struct CharStream;

	/* specialize to make type available as byte source for a stream (should not be interacted with directly, instead only use str::Source) */
	template <class Type>
	struct ByteSource;

	/* formattable interface which requires:
	*	operator() to take a sink of any type, a value of the type, a utf32 string-view with formatting-string and a boolean return-value
	*	if the format-string was valid (should leave the sink untouched if the format is invalid; empty format should be valid at all times) */
	template <class Type>
	struct Formatter;

	/* type is anything convertible to a string-view */
	template <class Type>
	concept IsStr = !std::is_void_v<typename detail::StrType<Type>::type>;

	/* type is anything convertible to a string-view of the specific type */
	template <class Type, class ChType>
	concept IsChStr = std::convertible_to<Type, std::basic_string_view<ChType>>;

	/* type is anything that implements the str::CharWriter interface
	*	operator(ChType, size_t) to write the character 0 to n times
	*	operator(const std::basic_string_view<ChType>&) to write the corresponding string */
	template <class Type>
	concept IsSink = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, size_t sz) {
		typename str::CharWriter<std::remove_cvref_t<Type>>::ChType;
		str::CharWriter<std::remove_cvref_t<Type>>{}(t, std::declval<typename str::CharWriter<std::remove_cvref_t<Type>>::ChType>(), sz);
		str::CharWriter<std::remove_cvref_t<Type>>{}(t, std::declval<const std::basic_string_view<typename str::CharWriter<std::remove_cvref_t<Type>>::ChType>&>());
	};

	/* type is anything that implements the str::ByteWriter interface
	*	operator(const str::Data&) to write the corresponding data */
	template <class Type>
	concept IsWire = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, const str::Data & d) {
		str::ByteWriter<std::remove_cvref_t<Type>>{}(t, d);
	};

	/* check if the type specializes str::CharStream accordingly and defines all necessary operations
	*	Note: May keep a reference to the stream object
	*	ChType: character type of the given stream
	*	load(size_t i): load at least [i] characters, or any remaining, if [i] is larger than the stream, and return a reference to them
	*	consume(size_t i): remove the leading [i] characters from the stream (if [i] is greater or equal to loaded size, consume everything)
	*	done(): check if the stream contains at least one character */
	template <class Type>
	concept IsStream = requires(Type & t, size_t i) {
		typename str::CharStream<std::remove_cvref_t<Type>>::ChType;
		{ str::CharStream<std::remove_cvref_t<Type>>{ t }.load(i) } -> std::convertible_to<std::basic_string_view<typename str::CharStream<std::remove_cvref_t<Type>>::ChType>>;
		{ str::CharStream<std::remove_cvref_t<Type>>{ t }.consume(i) } -> std::same_as<void>;
		{ str::CharStream<std::remove_cvref_t<Type>>{ t }.done() } -> std::same_as<bool>;
	};

	/* check if the type specializes str::ByteSource accordingly and defines all necessary operations
	*	Note: May keep a reference to the source object
	*	load(size_t i): load at least [i] bytes, or any remaining, if [i] is larger than the source, and return a reference to them
	*	consume(size_t i): remove the leading [i] bytes from the source (if [i] is greater or equal to loaded size, consume everything)
	*	done(): check if the source contains at least one byte */
	template <class Type>
	concept IsSource = requires(Type & t, size_t i) {
		{ str::ByteSource<std::remove_cvref_t<Type>>{ t }.load(i) } -> std::convertible_to<str::Data>;
		{ str::ByteSource<std::remove_cvref_t<Type>>{ t }.consume(i) } -> std::same_as<void>;
		{ str::ByteSource<std::remove_cvref_t<Type>>{ t }.done() } -> std::same_as<bool>;
	};

	/* extract the character type of the type, which satisfies str::IsSink */
	template <str::IsSink Type>
	using SinkChar = typename str::CharWriter<std::remove_cvref_t<Type>>::ChType;

	/* extract the character type of the type, which satisfies str::IsStr */
	template <class Type>
	using StringChar = typename detail::StrType<Type>::type;

	/* extract the character type of the type, which satisfies str::IsStream */
	template <str::IsStream Type>
	using StreamChar = typename str::CharStream<std::remove_cvref_t<Type>>::ChType;

	/* type is anything that implements thr str::Formatter interface */
	template <class Type>
	concept IsFormattable = requires(const Type & val, const std::u32string_view & fmt, std::string & cs, std::wstring & ws, std::u8string & u8s, std::u16string & u16s, std::u32string & u32s) {
		{ str::Formatter<std::remove_cvref_t<Type>>{}(cs, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(ws, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(u8s, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(u16s, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(u32s, val, fmt) } -> std::same_as<bool>;
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

	/* receivers receive arbitrary many values of the given value-list */
	template <class Type, class... ValType>
	concept IsReceiver = requires(Type t, ValType... v) {
		{ t(v...) } -> std::same_as<void>;
	};

	/* valid collectors must receive zero or more valid arguments via next() and a final call to done(), after which
	*	the object is considered burnt (undefined behavior allowed, if input does not behave well-defined) */
	template <class Type>
	concept IsCollector = requires(Type t, char32_t c) {
		{ t.next(c) } -> std::same_as<void>;
		{ t.done() } -> std::same_as<void>;
	};

	/* codepoint-mapper must receive a collector to which it writes the mapped content
	*	and return a collector and expose the type of the new collector as ::Type */
	template <class Type>
	concept IsMapper = requires(const Type t, detail::EmptyCollector c) {
		typename Type::template Type<detail::EmptyCollector>;
		{ t(c) } -> std::same_as<typename Type::template Type<detail::EmptyCollector>>;
		{ t(c) } -> str::IsCollector;
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

	/* wrappers to interact with character-sinks */
	template <str::IsSink SinkType>
	constexpr void CallSink(SinkType&& sink, str::SinkChar<SinkType> chr, size_t count = 1) {
		str::CharWriter<std::remove_cvref_t<SinkType>>{}(sink, chr, count);
	}
	template <str::IsSink SinkType>
	constexpr void CallSink(SinkType&& sink, const std::basic_string_view<str::SinkChar<SinkType>>& str) {
		str::CharWriter<std::remove_cvref_t<SinkType>>{}(sink, str);
	}

	/* wrapper to write to a byte-sink */
	constexpr auto& CallWire(str::IsWire auto&& sink, const str::Data& data) {
		str::ByteWriter<std::remove_cvref_t<decltype(sink)>>{}(sink, data);
		return sink;
	}

	/* wrapper to interact with formatters */
	constexpr bool CallFormat(str::IsSink auto&& sink, const str::IsFormattable auto& val, const std::u32string_view& fmt = U"") {
		return str::Formatter<std::remove_cvref_t<decltype(val)>>{}(sink, val, fmt);
	}
}
