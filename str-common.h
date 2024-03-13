#pragma once

#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>
#include <stdexcept>

/*
*	char8_t/char16_t/char32_t must be their respective unicode-encodings
*	char can be multi-byte using the current locale or any unicode-encoding
*/
namespace str {
	namespace detail {
		template <class Type, class ChType> concept IsCharString = requires(const Type t, size_t n) {
			{ t } -> std::convertible_to<std::basic_string_view<ChType>>;
		};
		template <class Type, class ChType> concept IsCharWritable = requires(Type t, ChType c) {
			{ t.push_back(c) };
			{ t.append(std::basic_string_view<ChType>{}) };
		};

		/* check expected assumptions */
		static_assert(sizeof(char32_t) == sizeof(uint32_t), "char32_t is expected to be 32bit");
		static_assert(sizeof(char16_t) == sizeof(uint16_t), "char16_t is expected to be 16bit");
		static_assert(sizeof(char8_t) == sizeof(uint8_t), "char8_t is expected to be 8bit");
		static_assert(sizeof(wchar_t) >= sizeof(uint16_t) && sizeof(wchar_t) < sizeof(uint32_t), "wchar_t is expected to be at least 16bit and at most 32bit");
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

		/* get the type that fullfills detail::IsCharString */
		template <class Type> struct GetCharString { using type = void; };
		template <detail::IsCharString<char> Type> struct GetCharString<Type> { using type = char; };
		template <detail::IsCharString<wchar_t> Type> struct GetCharString<Type> { using type = wchar_t; };
		template <detail::IsCharString<char8_t> Type> struct GetCharString<Type> { using type = char8_t; };
		template <detail::IsCharString<char16_t> Type> struct GetCharString<Type> { using type = char16_t; };
		template <detail::IsCharString<char32_t> Type> struct GetCharString<Type> { using type = char32_t; };

		/* get the type that fullfills detail::IsCharWritable */
		template <class Type> struct GetCharWritable { using type = void; };
		template <detail::IsCharWritable<char> Type> struct GetCharWritable<Type> { using type = char; };
		template <detail::IsCharWritable<wchar_t> Type> struct GetCharWritable<Type> { using type = wchar_t; };
		template <detail::IsCharWritable<char8_t> Type> struct GetCharWritable<Type> { using type = char8_t; };
		template <detail::IsCharWritable<char16_t> Type> struct GetCharWritable<Type> { using type = char16_t; };
		template <detail::IsCharWritable<char32_t> Type> struct GetCharWritable<Type> { using type = char32_t; };

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
	concept IsAscii = (str::IsChar<Type> || !std::is_same_v<Type, char> || detail::MBHoldsAscii);

	/* is type a character string or like a string_view (.data, .size) and get corresponding char-type */
	template <class Type>
	concept AnyString = !std::is_void_v<typename detail::GetCharString<Type>::type>;
	template <class Type, class ChType>
	concept IsString = detail::IsCharString<Type, ChType>;
	template <str::AnyString Type>
	using StringChar = typename detail::GetCharString<Type>::type;

	/* type has a push_back and append method and get corresponding char-type */
	template <class Type>
	concept AnySink = !std::is_void_v<typename detail::GetCharWritable<Type>::type>;
	template <class Type, class ChType>
	concept IsSink = detail::IsCharWritable<Type, ChType>;
	template <str::AnySink Type>
	using SinkChar = typename detail::GetCharWritable<Type>::type;

	/* string-buffer overflow/underflow exception */
	struct BufferException : public std::runtime_error {
		BufferException(const std::string& s) : runtime_error(s) {}
	};

	/* small stack-buffered string, to be appended to, for intermediate/temporary value building, not null-terminated
	*	Implements: str::IsString, str::IsSink
	*	If capacity is negative any values written over the buffer-capacity are discarded, otherwise an exception is thrown */
	template <str::IsChar ChType, intptr_t Capacity>
		requires (Capacity != 0)
	class Small {
		using ThisType = str::Small<ChType, Capacity>;
		static constexpr size_t ActCapacity = static_cast<size_t>(Capacity < 0 ? -Capacity : Capacity);
		static constexpr bool SilentErrors = (Capacity < 0);

	private:
		ChType pBuffer[ActCapacity] = { 0 };
		detail::SizeType<ActCapacity> pSize = 0;

	public:
		constexpr Small() = default;
		constexpr Small(const str::IsString<ChType> auto& s) {
			fAppend(s);
		}

	private:
		constexpr void fAppend(const auto& s) {
			std::basic_string_view<ChType> view{ s };
			size_t size = view.size();

			/* check if an error should be thrown or the buffer should only be filled up to the end */
			if constexpr (SilentErrors)
				size = std::min<size_t>(size, ActCapacity - pSize);
			else if (ActCapacity - pSize < size)
				throw str::BufferException("str::Small capacity exceeded");

			/* write the data to the buffer */
			for (size_t i = 0; i < size; ++i)
				pBuffer[pSize++] = view[i];
		}

	public:
		constexpr ThisType& operator+=(const str::IsString<ChType> auto& s) {
			fAppend(s);
			return *this;
		}
		constexpr ThisType& operator=(const str::IsString<ChType> auto& s) {
			pSize = 0;
			fAppend(s);
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
		constexpr ThisType& append(const str::IsString<ChType> auto& s) {
			fAppend(s);
			return *this;
		}
		constexpr void push_back(ChType c) {
			fAppend(std::basic_string_view<ChType>{ &c, 1 });
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
}
