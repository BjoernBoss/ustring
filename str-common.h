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
	public:
		const uint8_t* ptr = 0;
		size_t size = 0;

	public:
		constexpr Data() = default;
		constexpr Data(const uint8_t* p, size_t s) : ptr{ p }, size{ s } {}
		constexpr Data(const str::Data&) = default;
		constexpr Data(str::Data&&) = default;
		constexpr str::Data& operator=(const str::Data&) = default;
		constexpr str::Data& operator=(str::Data&&) = default;

	public:
		constexpr str::Data subdata(size_t count) const {
			if (count >= size)
				return str::Data{};
			return str::Data{ ptr + count, size - count };
		}
		constexpr bool empty() const {
			return (size == 0);
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

	/* extract the character type of the type, which satisfies str::IsSink */
	template <str::IsSink Type>
	using SinkChar = typename str::CharWriter<std::remove_cvref_t<Type>>::ChType;

	/* extract the character type of the type, which satisfies str::IsStr */
	template <class Type>
	using StrChar = typename detail::StrType<Type>::type;

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

	/* extract the character type of the type, which satisfies str::IsStream */
	template <str::IsStream Type>
	using StreamChar = typename str::CharStream<std::remove_cvref_t<Type>>::ChType;

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

	/* [str::IsStream] stream-reader to interact with a char-stream
	*	Note: Must not outlive the stream object as it may store a reference to it */
	template <str::IsStream Type>
	class Stream {
	private:
		using Impl = str::CharStream<std::remove_cvref_t<Type>>;

	public:
		using ChType = typename Impl::ChType;

	private:
		Impl pSource;

	public:
		constexpr Stream(Type& s) : pSource{ s } {}

	public:
		constexpr std::basic_string_view<ChType> load(size_t count) {
			return pSource.load(count);
		}
		constexpr void consume(size_t count = size_t(-1)) {
			pSource.consume(count);
		}
		constexpr bool done() const {
			return pSource.done();
		}
	};

	/* [str::IsSource] source-reader to interact with a byte-source
	*	Note: Must not outlive the source object as it may store a reference to it */
	template <str::IsSource Type>
	class Source {
	private:
		using Impl = str::ByteSource<std::remove_cvref_t<Type>>;

	private:
		Impl pSource;

	public:
		constexpr Source(Type& s) : pSource{ s } {}

	public:
		constexpr str::Data load(size_t count) {
			return pSource.load(count);
		}
		constexpr void consume(size_t count = size_t(-1)) {
			pSource.consume(count);
		}
		constexpr bool done() const {
			return pSource.done();
		}
	};

	namespace detail {
		template <class Type>
		class LocalBuffer {
		private:
			static constexpr size_t BufferSize = 4;

		private:
			struct Static {
				Type buffer[BufferSize]{};
			};
			using Dynamic = std::vector<Type>;

		private:
			std::variant<Static, Dynamic> pBuffer;
			Type* pBegin = 0;
			Type* pEnd = 0;

		public:
			constexpr LocalBuffer() : pBuffer{ Static{} } {
				pBegin = std::get<Static>(pBuffer).buffer;
				pEnd = pBegin;
			}

		public:
			constexpr void push(const Type& t) {
				if (std::holds_alternative<Dynamic>(pBuffer)) {
					Dynamic& d = std::get<Dynamic>(pBuffer);
					if (size_t(pEnd - d.data()) >= d.size()) {
						size_t bOff = pBegin - d.data(), eOff = pEnd - d.data();
						d.resize(d.size() + BufferSize);
						pBegin = d.data() + bOff;
						pEnd = d.data() + eOff;
					}
				}
				else if (pEnd - std::get<Static>(pBuffer).buffer >= BufferSize) {
					Dynamic v{ pBegin, pEnd };
					v.push_back(t);
					pBuffer = std::move(v);
					pBegin = std::get<Dynamic>(pBuffer).data();
					pEnd = pBegin + std::get<Dynamic>(pBuffer).size();
					return;
				}
				*pEnd = t;
				++pEnd;
			}
			constexpr Type pop() {
				Type val = *pBegin;
				if (++pBegin == pEnd) {
					if (std::holds_alternative<Static>(pBuffer))
						pBegin = std::get<Static>(pBuffer).buffer;
					else
						pBegin = std::get<Dynamic>(pBuffer).data();
					pEnd = pBegin;
				}
				return val;
			}
			constexpr void clear() {
				if (std::holds_alternative<Static>(pBuffer))
					pBegin = std::get<Static>(pBuffer).buffer;
				else
					pBegin = std::get<Dynamic>(pBuffer).data();
				pEnd = pBegin;
			}
			constexpr size_t size() const {
				return (pEnd - pBegin);
			}
			constexpr Type& get(size_t i) {
				return pBegin[i];
			}
			constexpr Type& front() {
				return pBegin[0];
			}
			constexpr Type& back() {
				return pEnd[-1];
			}
			constexpr Type* begin() {
				return pBegin;
			}
			constexpr Type* end() {
				return pEnd;
			}
		};

		/* optimal unsigned integer size-type to be able to hold the given capacity */
		template <size_t Capacity> using SizeType8Or16 = std::conditional_t<Capacity <= std::numeric_limits<uint8_t>::max(), uint8_t, uint16_t>;
		template <size_t Capacity> using SizeType32OrLess = std::conditional_t<Capacity <= std::numeric_limits<uint16_t>::max(), detail::SizeType8Or16<Capacity>, uint32_t>;
		template <size_t Capacity> using SizeType64OrLess = std::conditional_t<Capacity <= std::numeric_limits<uint32_t>::max(), detail::SizeType32OrLess<Capacity>, uint64_t>;
		template <size_t Capacity> using SizeType = std::conditional_t<Capacity <= std::numeric_limits<uint64_t>::max(), detail::SizeType64OrLess<Capacity>, size_t>;
	}

	/* local string-buffer overflow/underflow exception */
	struct LocalException : public std::runtime_error {
		LocalException(const std::string& s) : runtime_error(s) {}
	};

	/* [str::IsStr/str::IsSink] local stack-buffered string null-terminated string, to be appended to, for intermediate/temporary value building
	*	If capacity is negative any values written over the buffer-capacity are discarded, otherwise an exception is thrown */
	template <str::IsChar ChType, intptr_t Capacity>
		requires (Capacity != 0)
	class Local {
		using ThisType = str::Local<ChType, Capacity>;
		static constexpr size_t ActCapacity = static_cast<size_t>(Capacity < 0 ? -Capacity : Capacity);
		static constexpr bool SilentErrors = (Capacity < 0);

	private:
		/* last entry acts as null-byte */
		ChType pBuffer[ActCapacity + 1] = { 0 };
		detail::SizeType<ActCapacity> pSize = 0;

	public:
		constexpr Local() = default;
		constexpr Local(const std::basic_string_view<ChType>& s) {
			fAppend(s.data(), s.size());
		}
		constexpr Local(const ChType* str, size_t sz) {
			fAppend(str, sz);
		}
		constexpr Local(size_t count, ChType c) {
			fAppend(c, count);
		}

	private:
		constexpr void fAppend(ChType c, size_t size) {
			/* check if an error should be thrown or the buffer should only be filled up to the end */
			if constexpr (SilentErrors)
				size = std::min<size_t>(size, ActCapacity - pSize);
			else if (ActCapacity - pSize < size)
				throw str::LocalException("str::Local capacity exceeded");

			/* write the data to the buffer */
			for (size_t i = 0; i < size; ++i)
				pBuffer[pSize++] = c;
			pBuffer[pSize] = 0;
		}
		constexpr void fAppend(const ChType* begin, size_t size) {
			/* check if an error should be thrown or the buffer should only be filled up to the end */
			if constexpr (SilentErrors)
				size = std::min<size_t>(size, ActCapacity - pSize);
			else if (ActCapacity - pSize < size)
				throw str::LocalException("str::Local capacity exceeded");

			/* write the data to the buffer */
			for (size_t i = 0; i < size; ++i)
				pBuffer[pSize++] = begin[i];
			pBuffer[pSize] = 0;
		}

	public:
		constexpr ThisType& operator+=(const std::basic_string_view<ChType>& s) {
			fAppend(s.data(), s.size());
			return *this;
		}
		constexpr ThisType& operator+=(ChType c) {
			fAppend(c, 1);
			return *this;
		}
		constexpr ThisType& operator=(const std::basic_string_view<ChType>& s) {
			pSize = 0;
			fAppend(s.data(), s.size());
			return *this;
		}
		constexpr ThisType& operator=(ChType c) {
			pSize = 0;
			fAppend(c, 1);
			return *this;
		}
		constexpr const ChType& operator[](size_t index) const {
			return pBuffer[index];
		}
		constexpr operator std::basic_string_view<ChType>() const {
			return std::basic_string_view<ChType>{ pBuffer, pBuffer + pSize };
		}

	public:
		constexpr std::basic_string_view<ChType> view() const {
			return std::basic_string_view<ChType>{ pBuffer, pBuffer + pSize };
		}
		constexpr std::basic_string<ChType> str() const {
			return std::basic_string<ChType>{ pBuffer, pBuffer + pSize };
		}
		constexpr ThisType& assign(const std::basic_string_view<ChType>& s) {
			pSize = 0;
			fAppend(s.data(), s.size());
			return *this;
		}
		constexpr ThisType& assign(const ChType* str, size_t sz) {
			pSize = 0;
			fAppend(str, sz);
			return *this;
		}
		constexpr ThisType& assign(size_t count, ChType c) {
			pSize = 0;
			fAppend(c, count);
			return *this;
		}
		constexpr ThisType& append(const std::basic_string_view<ChType>& s) {
			fAppend(s.data(), s.size());
			return *this;
		}
		constexpr ThisType& append(const ChType* str, size_t sz) {
			fAppend(str, sz);
			return *this;
		}
		constexpr ThisType& append(size_t count, ChType c) {
			fAppend(c, count);
			return *this;
		}
		constexpr void push_back(ChType c) {
			fAppend(c, 1);
		}
		constexpr size_t size() const {
			return static_cast<size_t>(pSize);
		}
		constexpr bool empty() const {
			return (pSize == 0);
		}
		constexpr const ChType* data() const {
			return pBuffer;
		}
		constexpr const ChType* begin() const {
			return pBuffer;
		}
		constexpr const ChType* end() const {
			return (pBuffer + pSize);
		}
		constexpr void clear() {
			pSize = 0;
		}
		constexpr const ChType* c_str() const {
			return pBuffer;
		}
	};

	/* convenience for fast usage */
	template <intptr_t Capacity>
	using LocCh = str::Local<char, Capacity>;
	template <intptr_t Capacity>
	using LocWd = str::Local<wchar_t, Capacity>;
	template <intptr_t Capacity>
	using LocU8 = str::Local<char8_t, Capacity>;
	template <intptr_t Capacity>
	using LocU16 = str::Local<char16_t, Capacity>;
	template <intptr_t Capacity>
	using LocU32 = str::Local<char32_t, Capacity>;

	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer with a null-byte (if capacity is greater than zero) */
	template <str::IsChar ChType>
	class NullChars {
	private:
		ChType* pBegin = 0;
		ChType* pEnd = 0;
		bool pOverflow = false;

	public:
		template <size_t N>
		constexpr NullChars(ChType(&buf)[N]) {
			if constexpr (N == 0)
				return;
			buf[0] = 0;
			pBegin = buf;
			pEnd = buf + (N - 1);
		}
		constexpr NullChars(ChType* buf, size_t capacity) {
			if (capacity == 0)
				return;
			buf[0] = 0;
			pBegin = buf;
			pEnd = buf + (capacity - 1);
		}

	public:
		constexpr void put(ChType c) {
			if (pBegin != pEnd) {
				*pBegin = c;
				*(++pBegin) = 0;
			}
			else
				pOverflow = true;
		}
		constexpr void write(const std::basic_string_view<ChType>& s) {
			size_t sz = s.size();
			if (sz > size_t(pEnd - pBegin)) {
				sz = size_t(pEnd - pBegin);
				pOverflow = true;
			}
			std::copy(s.data(), s.data() + sz, pBegin);
			*(pBegin += sz) = 0;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer and make the written size available */
	template <str::IsChar ChType>
	class Chars {
	private:
		ChType* pPtr = 0;
		size_t pSize = 0;
		size_t pOffset = 0;
		bool pOverflow = false;

	public:
		template <size_t N>
		constexpr Chars(ChType(&buf)[N]) {
			pPtr = buf;
			pSize = N;
		}
		constexpr Chars(ChType* buf, size_t capacity) {
			pPtr = buf;
			pSize = capacity;
		}

	public:
		constexpr void put(ChType c) {
			if (pOffset < pSize)
				pPtr[pOffset++] = c;
			else
				pOverflow = true;
		}
		constexpr void write(const std::basic_string_view<ChType>& s) {
			size_t sz = s.size();
			if (sz > pSize - pOffset) {
				sz = pSize - pOffset;
				pOverflow = true;
			}
			std::copy(s.data(), s.data() + sz, pPtr + pOffset);
			pOffset += sz;
		}
		constexpr size_t size() const {
			return pOffset;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsWire] wrapper to create a byte-sink into a constant buffer or a pointer and make the written size available */
	class Bytes {
	private:
		uint8_t* pPtr = 0;
		size_t pSize = 0;
		size_t pOffset = 0;
		bool pOverflow = false;

	public:
		template <size_t N>
		constexpr Bytes(uint8_t(&buf)[N]) {
			pPtr = buf;
			pSize = N;
		}
		constexpr Bytes(uint8_t* buf, size_t capacity) {
			pPtr = buf;
			pSize = capacity;
		}

	public:
		constexpr void write(const str::Data& d) {
			size_t sz = d.size;
			if (sz > pSize - pOffset) {
				sz = pSize - pOffset;
				pOverflow = true;
			}
			std::copy(d.ptr, d.ptr + sz, pPtr + pOffset);
			pOffset += sz;
		}
		constexpr size_t size() const {
			return pOffset;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsSink] structure to inherit from which can be used as a sink */
	struct InheritSink {
	public:
		constexpr InheritSink() = default;
		virtual ~InheritSink() = default;

	public:
		virtual void write(const std::u32string_view& s) = 0;
		virtual void write(char32_t chr, size_t count) = 0;
	};

	/* [str::IsWire] structure to inherit from which can be used as a wire */
	struct InheritWire {
	public:
		constexpr InheritWire() = default;
		virtual ~InheritWire() = default;

	public:
		virtual void write(const str::Data& d) = 0;
	};
}
