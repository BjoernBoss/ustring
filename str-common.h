#pragma once

#include <string>
#include <iostream>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>
#include <stdexcept>
#include <utility>
#include <algorithm>

/*
*	char8_t/char16_t/char32_t must be their respective unicode-encodings
*	char can be multi-byte using the current locale or any unicode-encoding
*/
namespace str {
	namespace detail {
		/* check expected assumptions */
		static_assert(sizeof(char32_t) == sizeof(uint32_t), "char32_t is expected to be 32bit");
		static_assert(sizeof(char16_t) == sizeof(uint16_t), "char16_t is expected to be 16bit");
		static_assert(sizeof(char8_t) == sizeof(uint8_t), "char8_t is expected to be 8bit");
		static_assert(sizeof(wchar_t) == sizeof(uint16_t) || sizeof(wchar_t) == sizeof(uint32_t), "wchar_t is expected to be at least 16bit and at most 32bit");
		static_assert(sizeof(char) == sizeof(uint8_t), "char is expected to be 8bit");

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

		/* [character cannot be represented in current code page] */
#pragma warning(push)
#pragma warning(disable : 4566)
		static constexpr bool MBIsUtf8 = detail::IsUtf8("\U0000007f\U0000ff00\U00010000");
		static constexpr bool WdIsUtf16 = detail::IsUtf16(L"\U00010000\U0000ff00");
		static constexpr bool WdIsUtf32 = detail::IsUtf32(L"\U00010000\U0000ff00");
#pragma warning(pop)

		/* check assumptions hold */
		static_assert(detail::IsUtf8(u8"\U0000007f\U0000ff00\U00010000"), "char8_t is expected to be utf-8 encoded");
		static_assert(detail::IsUtf16(u"\U00010000\U0000ff00"), "char16_t is expected to be utf-16 encoded");
		static_assert(detail::IsUtf32(U"\U00010000\U0000ff00"), "char32_t is expected to be utf-32 encoded");
		static_assert(detail::WdIsUtf16 || detail::WdIsUtf32, "wchar_t is expected to be utf-16 or utf-32 encoded");

		static constexpr bool MBHoldsAscii = (detail::MBIsUtf8 || detail::HoldSameValues(
			U"\0\001\002\003\004\005\006\a\b\t\n\v\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
			U" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177",
			"\0\001\002\003\004\005\006\a\b\t\n\v\f\r\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
			" !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\177"));

		/* check if the type is a character */
		template <class Type> struct GetCharNative { using type = void; };
		template <> struct GetCharNative<char> { using type = char; };
		template <> struct GetCharNative<wchar_t> { using type = wchar_t; };
		template <class Type> struct GetCharUnicode { using type = void; };
		template <> struct GetCharUnicode<char8_t> { using type = char8_t; };
		template <> struct GetCharUnicode<char16_t> { using type = char16_t; };
		template <> struct GetCharUnicode<char32_t> { using type = char32_t; };
		template <class Type> struct GetCharAny { using type = void; };
		template <> struct GetCharAny<char> { using type = char; };
		template <> struct GetCharAny<wchar_t> { using type = wchar_t; };
		template <> struct GetCharAny<char8_t> { using type = char8_t; };
		template <> struct GetCharAny<char16_t> { using type = char16_t; };
		template <> struct GetCharAny<char32_t> { using type = char32_t; };

		/* optimal unsigned integer size-type to be able to hold the given capacity */
		template <size_t Capacity> using SizeType8Or16 = std::conditional_t<Capacity <= std::numeric_limits<uint8_t>::max(), uint8_t, uint16_t>;
		template <size_t Capacity> using SizeType32OrLess = std::conditional_t<Capacity <= std::numeric_limits<uint16_t>::max(), detail::SizeType8Or16<Capacity>, uint32_t>;
		template <size_t Capacity> using SizeType64OrLess = std::conditional_t<Capacity <= std::numeric_limits<uint32_t>::max(), detail::SizeType32OrLess<Capacity>, uint64_t>;
		template <size_t Capacity> using SizeType = std::conditional_t<Capacity <= std::numeric_limits<uint64_t>::max(), detail::SizeType64OrLess<Capacity>, size_t>;
	}

	/* check if the multibyte character string uses a given utf-encoding/holds ascii */
	static constexpr bool IsCharUtf8 = detail::MBIsUtf8;
	static constexpr bool IsCharAscii = detail::MBHoldsAscii;

	/* check which utf-encoding the wide character string uses */
	static constexpr bool IsWideUtf16 = detail::WdIsUtf16;
	static constexpr bool IsWideUtf32 = detail::WdIsUtf32;

	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsUnicode = !std::is_void_v<typename detail::GetCharUnicode<Type>::type>;
	template <class Type>
	concept IsNative = !std::is_void_v<typename detail::GetCharNative<Type>::type>;
	template <class Type>
	concept IsChar = !std::is_void_v<typename detail::GetCharAny<Type>::type>;

	/* check if the given type directly encodes ascii */
	template <class Type>
	concept IsAscii = str::IsChar<Type> && (!std::is_same_v<Type, char> || detail::MBHoldsAscii);

	/* return the effective character type equivalent to the encoding (i.e. if wchar_t uses
	*	utf-16, will result in char16_t; will only result in char, char8_t, char16_t, char32_t) */
	template <str::IsChar Type>
	using EffChar = std::conditional_t<std::is_same_v<Type, char>, std::conditional_t<str::IsCharUtf8, char8_t, char>,
		std::conditional_t<std::is_same_v<Type, wchar_t>, std::conditional_t<str::IsWideUtf16, char16_t, char32_t>, Type>>;

	/* check if the two character-types are effectively using the same encoding */
	template <class ChTypeA, class ChTypeB>
	concept EffSame = std::is_same_v<str::EffChar<ChTypeA>, str::EffChar<ChTypeB>>;

	/* character sink interface which requires:
	*	operator() to take the sink object and a character and a count (can be zero)
	*	operator() to take the sink object and a pointer and a size */
	template <class Type, class ChType>
	struct CharSink;
	template <class Type, class ChType>
	concept IsSink = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, ChType chr, const ChType * str, size_t sz) {
		str::CharSink<std::remove_cvref_t<Type>, ChType>{}(t, chr, sz);
		str::CharSink<std::remove_cvref_t<Type>, ChType>{}(t, str, sz);
	};

	namespace detail {
		template <class Type> struct GetCharSink { using type = void; };
		template <str::IsSink<char> Type> struct GetCharSink<Type> { using type = char; };
		template <str::IsSink<wchar_t> Type> struct GetCharSink<Type> { using type = wchar_t; };
		template <str::IsSink<char8_t> Type> struct GetCharSink<Type> { using type = char8_t; };
		template <str::IsSink<char16_t> Type> struct GetCharSink<Type> { using type = char16_t; };
		template <str::IsSink<char32_t> Type> struct GetCharSink<Type> { using type = char32_t; };
	}

	/* check if type is any kind of sink and ability to extract the corresponding char-type */
	template <class Type>
	concept AnySink = !std::is_void_v<typename detail::GetCharSink<Type>::type>;
	template <class Type>
	using SinkCharType = typename detail::GetCharSink<Type>::type;

	/* wrapper to write to a sink */
	template <class ChType>
	constexpr auto& SinkChars(str::IsSink<ChType> auto&& sink, ChType chr, size_t count = 1) {
		str::CharSink<std::remove_cvref_t<decltype(sink)>, ChType>{}(sink, chr, count);
		return sink;
	}
	template <class ChType>
	constexpr auto& SinkString(str::IsSink<ChType> auto&& sink, const ChType* str, size_t sz) {
		str::CharSink<std::remove_cvref_t<decltype(sink)>, ChType>{}(sink, str, sz);
		return sink;
	}

	/* string is anything convertible to a string-view */
	template <class Type, class ChType>
	concept IsString = requires(const Type & t) {
		{ t } -> std::convertible_to<std::basic_string_view<ChType>>;
	};

	namespace detail {
		template <class Type> struct GetCharString { using type = void; };
		template <str::IsString<char> Type> struct GetCharString<Type> { using type = char; };
		template <str::IsString<wchar_t> Type> struct GetCharString<Type> { using type = wchar_t; };
		template <str::IsString<char8_t> Type> struct GetCharString<Type> { using type = char8_t; };
		template <str::IsString<char16_t> Type> struct GetCharString<Type> { using type = char16_t; };
		template <str::IsString<char32_t> Type> struct GetCharString<Type> { using type = char32_t; };
	}

	/* check if type is any kind of string and ability to extract the corresponding char-type */
	template <class Type>
	concept AnyString = !std::is_void_v<typename detail::GetCharString<Type>::type>;
	template <class Type>
	using StringCharType = typename detail::GetCharString<Type>::type;

	/* wrapper to get string iterators */
	template <class ChType>
	constexpr std::pair<const ChType*, const ChType*> StringIterators(const str::IsString<ChType> auto& string) {
		std::basic_string_view<ChType> view{ string };
		return { view.data(), view.data() + view.size() };
	}
	template <class ChType>
	constexpr std::basic_string_view<ChType> StringView(const str::IsString<ChType> auto& string) {
		return std::basic_string_view<ChType>{ string };
	}

	/* string-buffer overflow/underflow exception */
	struct BufferException : public std::runtime_error {
		BufferException(const std::string& s) : runtime_error(s) {}
	};

	/* small stack-buffered string null-terminated string, to be appended to, for intermediate/temporary value building
	*	If capacity is negative any values written over the buffer-capacity are discarded, otherwise an exception is thrown */
	template <str::IsChar ChType, intptr_t Capacity>
		requires (Capacity != 0)
	class Small {
		using ThisType = str::Small<ChType, Capacity>;
		static constexpr size_t ActCapacity = static_cast<size_t>(Capacity < 0 ? -Capacity : Capacity);
		static constexpr bool SilentErrors = (Capacity < 0);

	private:
		/* last entry acts as null-byte */
		ChType pBuffer[ActCapacity + 1] = { 0 };
		detail::SizeType<ActCapacity> pSize = 0;

	public:
		constexpr Small() = default;
		constexpr Small(const str::IsString<ChType> auto& s) {
			fAppend(s);
		}
		constexpr Small(const ChType* str, size_t sz) {
			fAppend(str, str + sz);
		}

	private:
		constexpr void fAppend(const auto& s) {
			auto [begin, end] = str::StringIterators<ChType>(s);
			fAppend(begin, end);
		}
		constexpr void fAppend(const ChType* begin, const ChType* end) {
			size_t size = (end - begin);

			/* check if an error should be thrown or the buffer should only be filled up to the end */
			if constexpr (SilentErrors)
				size = std::min<size_t>(size, ActCapacity - pSize);
			else if (ActCapacity - pSize < size)
				throw str::BufferException("str::Small capacity exceeded");

			/* write the data to the buffer */
			for (size_t i = 0; i < size; ++i)
				pBuffer[pSize++] = begin[i];
		}

	public:
		constexpr ThisType& operator+=(const str::IsString<ChType> auto& s) {
			fAppend(s);
			return *this;
		}
		constexpr ThisType& operator+=(ChType c) {
			fAppend(&c, &c + 1);
			return *this;
		}
		constexpr ThisType& operator=(const str::IsString<ChType> auto& s) {
			pSize = 0;
			fAppend(s);
			return *this;
		}
		constexpr ThisType& operator=(ChType c) {
			pSize = 0;
			fAppend(&c, &c + 1);
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
		constexpr ThisType& assign(const str::IsString<ChType> auto& s) {
			pSize = 0;
			fAppend(s);
			return *this;
		}
		constexpr ThisType& assign(const ChType* str, size_t sz) {
			pSize = 0;
			fAppend(str, str + sz);
			return *this;
		}
		constexpr ThisType& append(const str::IsString<ChType> auto& s) {
			fAppend(s);
			return *this;
		}
		constexpr ThisType& append(const ChType* str, size_t sz) {
			fAppend(str, str + sz);
			return *this;
		}
		constexpr void push_back(ChType c) {
			fAppend(&c, &c + 1);
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
	using ChSmall = str::Small<char, Capacity>;
	template <intptr_t Capacity>
	using WdSmall = str::Small<wchar_t, Capacity>;
	template <intptr_t Capacity>
	using U8Small = str::Small<char8_t, Capacity>;
	template <intptr_t Capacity>
	using U16Small = str::Small<char16_t, Capacity>;
	template <intptr_t Capacity>
	using U32Small = str::Small<char32_t, Capacity>;

	/* wrapper to create a sink into a constant buffer or a pointer with a null-byte (if capacity is greater than zero) */
	template <class ChType>
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
		constexpr void write(const ChType* str, size_t sz) {
			if (sz > size_t(pEnd - pBegin)) {
				sz = size_t(pEnd - pBegin);
				pOverflow = true;
			}
			std::copy(str, str + sz, pBegin);
			*(pBegin += sz) = 0;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* wrapper to create a sink into a constant buffer or a pointer and make the written size available */
	template <class ChType>
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
		constexpr void write(const ChType* str, size_t sz) {
			if (sz > pSize - pOffset) {
				sz = pSize - pOffset;
				pOverflow = true;
			}
			std::copy(str, str + sz, pPtr + pOffset);
			pOffset += sz;
		}
		constexpr size_t size() const {
			return pOffset;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* specializations for char-sinks */
	template <class ChType>
	struct CharSink<std::basic_string<ChType>, ChType> {
		constexpr void operator()(std::basic_string<ChType>& sink, ChType chr, size_t count) const {
			if (count == 1)
				sink.push_back(chr);
			else if (count > 0)
				sink.append(count, chr);
		}
		constexpr void operator()(std::basic_string<ChType>& sink, const ChType* str, size_t size) const {
			sink.append(str, size);
		}
	};
	template <class ChType, intptr_t Capacity>
	struct CharSink<str::Small<ChType, Capacity>, ChType> {
		constexpr void operator()(str::Small<ChType, Capacity>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.push_back(chr);
		}
		constexpr void operator()(str::Small<ChType, Capacity>& sink, const ChType* str, size_t size) const {
			sink.append(str, size);
		}
	};
	template <class ChType>
	struct CharSink<std::basic_ostream<ChType>, ChType> {
		constexpr void operator()(std::basic_ostream<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(std::basic_ostream<ChType>& sink, const ChType* str, size_t size) const {
			sink.write(str, size);
		}
	};
	template <class ChType>
	struct CharSink<str::NullChars<ChType>, ChType> {
		constexpr void operator()(str::NullChars<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::NullChars<ChType>& sink, const ChType* str, size_t size) const {
			sink.write(str, size);
		}
	};
	template <class ChType>
	struct CharSink<str::Chars<ChType>, ChType> {
		constexpr void operator()(str::Chars<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::Chars<ChType>& sink, const ChType* str, size_t size) const {
			sink.write(str, size);
		}
	};
}
