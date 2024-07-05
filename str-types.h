#pragma once

#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>

namespace str {
	/* codepoint to indicate invalid decoding (guaranteed to be larger than any valid unicode-codepoint) */
	static constexpr char32_t Invalid = char32_t(-1);

	namespace detail {
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
	}

	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsChar = !std::is_void_v<typename detail::TestChar<Type>::type>;

	/* character writable interface which requires:
	*	operator() to take the writable object and a character and a count (can be zero)
	*	operator() to take the writable object and a pointer and a size */
	template <class Type, class ChType>
	struct CharWriter;

	/* byte sink interface which requires:
	*	operator() to take the wire object and a pointer and a size */
	template <class Type>
	struct ByteWriter;

	/* formattable interface which requires:
	*	operator() to take a sink of any type, a value of the type, a utf32 string-view with formatting-string and a boolean return-value
	*	if the format-string was valid (should leave the sink untouched if the format is invalid; empty format should be valid at all times) */
	template <class Type>
	struct Formatter;

	/* string is anything convertible to a string-view of the given char-type */
	template <class Type, class ChType>
	concept IsStr = requires(const Type & t) {
		{ t } -> std::convertible_to<std::basic_string_view<ChType>>;
	};

	/* type is anything that implements the str::CharWriter interface for the given char-type */
	template <class Type, class ChType>
	concept IsSink = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, ChType chr, const ChType * str, size_t sz) {
		str::CharWriter<std::remove_cvref_t<Type>, ChType>{}(t, chr, sz);
		str::CharWriter<std::remove_cvref_t<Type>, ChType>{}(t, str, sz);
	};

	/* type is anything that implements the str::ByteWriter interface */
	template <class Type>
	concept IsWire = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, const uint8_t * ptr, size_t sz) {
		str::ByteWriter<std::remove_cvref_t<Type>>{}(t, ptr, sz);
	};

	/* type is anything that implements thr str::Formatter interface */
	template <class Type>
	concept IsFormattable = requires(const Type & val, const std::u32string_view & fmt, std::string & cs, std::wstring & ws, std::u8string & u8s, std::u16string & u16s, std::u32string & u32s) {
		{ str::Formatter<std::remove_cvref_t<Type>>{}(cs, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(ws, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(u8s, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(u16s, val, fmt) } -> std::same_as<bool>;
		{ str::Formatter<std::remove_cvref_t<Type>>{}(u32s, val, fmt) } -> std::same_as<bool>;
	};

	namespace detail {
		template <class Type> struct StrType { using type = void; };
		template <str::IsStr<char> Type> struct StrType<Type> { using type = char; };
		template <str::IsStr<wchar_t> Type> struct StrType<Type> { using type = wchar_t; };
		template <str::IsStr<char8_t> Type> struct StrType<Type> { using type = char8_t; };
		template <str::IsStr<char16_t> Type> struct StrType<Type> { using type = char16_t; };
		template <str::IsStr<char32_t> Type> struct StrType<Type> { using type = char32_t; };

		template <class Type> struct SinkType { using type = void; };
		template <str::IsSink<char> Type> struct SinkType<Type> { using type = char; };
		template <str::IsSink<wchar_t> Type> struct SinkType<Type> { using type = wchar_t; };
		template <str::IsSink<char8_t> Type> struct SinkType<Type> { using type = char8_t; };
		template <str::IsSink<char16_t> Type> struct SinkType<Type> { using type = char16_t; };
		template <str::IsSink<char32_t> Type> struct SinkType<Type> { using type = char32_t; };
	}

	/* string is str::IsStr for any valid character type */
	template <class Type>
	concept AnyStr = !std::is_void_v<typename detail::StrType<Type>::type>;

	/* type is str::IsSink for any valid character type */
	template <class Type>
	concept AnySink = !std::is_void_v<typename detail::SinkType<Type>::type>;

	/* extract the character type of the type, which satisfies str::AnyStr */
	template <class Type>
	using StrChar = typename detail::StrType<Type>::type;

	/* extract the character type of the type, which satisfies str::AnySink */
	template <class Type>
	using SinkChar = typename detail::SinkType<Type>::type;

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

	/* wrappers to interact with character-sinks */
	template <str::IsChar ChType>
	constexpr void CallSink(str::IsSink<ChType> auto&& sink, ChType chr, size_t count = 1) {
		using Type = std::remove_cvref_t<decltype(sink)>;
		str::CharWriter<Type, ChType>{}(sink, chr, count);
	}
	template <str::IsChar ChType>
	constexpr void CallSink(str::IsSink<ChType> auto&& sink, const std::basic_string_view<ChType>& str) {
		using Type = std::remove_cvref_t<decltype(sink)>;
		str::CharWriter<Type, ChType>{}(sink, str.data(), str.size());
	}

	/* wrapper to write to a byte-sink */
	constexpr auto& CallWire(str::IsWire auto&& sink, const uint8_t* ptr, size_t sz) {
		str::ByteWriter<std::remove_cvref_t<decltype(sink)>>{}(sink, ptr, sz);
		return sink;
	}

	/* wrapper to interact with formatters */
	template <class ChType>
	constexpr bool CallFormat(str::IsSink<ChType> auto&& sink, const str::IsFormattable auto& val, const std::u32string_view& fmt = U"") {
		return str::Formatter<std::remove_cvref_t<decltype(val)>>{}(sink, val, fmt);
	}

	struct Decoded {
		char32_t cp = str::Invalid;
		uint32_t consumed = 0;
	};
}
