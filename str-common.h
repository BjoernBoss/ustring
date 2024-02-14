#pragma once

#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>
#include <stdexcept>

namespace str {
	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsUnicode = (std::same_as<Type, char8_t> || std::same_as<Type, char16_t> || std::same_as<Type, char32_t>);
	template <class Type>
	concept IsNative = (std::same_as<Type, char> || std::same_as<Type, wchar_t>);
	template <class Type>
	concept IsChar = (str::IsNative<Type> || str::IsUnicode<Type>);

	namespace detail {
		template <class Type, class ChType> concept IsCharString = requires(const Type t, size_t n) {
			{ t } -> std::convertible_to<std::basic_string_view<ChType>>;
		};
		template <class Type, class ChType> concept IsCharWritable = requires(Type t, ChType c) {
			{ t.push_back(c) };
			{ t.append(std::basic_string_view<ChType>{}) };
		};

		template <class Type> concept AnyCharString = (detail::IsCharString<Type, char> || detail::IsCharString<Type, wchar_t> || detail::IsCharString<Type, char8_t> || detail::IsCharString<Type, char16_t> || detail::IsCharString<Type, char32_t>);
		template <class Type> concept AnyCharWritable = (detail::IsCharWritable<Type, char> || detail::IsCharWritable<Type, wchar_t> || detail::IsCharWritable<Type, char8_t> || detail::IsCharWritable<Type, char16_t> || detail::IsCharWritable<Type, char32_t>);

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

	/* is type a character string or like a string_view (.data, .size) and get corresponding char-type */
	template <class Type>
	concept AnyString = detail::AnyCharString<Type>;
	template <class Type, class ChType>
	concept IsString = detail::IsCharString<Type, ChType>;
	template <str::AnyString Type>
	using StringChar = typename detail::GetCharString<Type>::type;

	/* type has a push_back and append method (.append not used, but required for template to derive a pinned type) and get corresponding char-type */
	template <class Type>
	concept AnySink = detail::AnyCharWritable<Type>;
	template <class Type, class ChType>
	concept IsSink = detail::IsCharWritable<Type, ChType>;
	template <str::AnySink Type>
	using SinkChar = typename detail::GetCharWritable<Type>::type;

	/* string-buffer overflow/underflow exception */
	struct BufferException : public std::runtime_error {
		BufferException(const std::string& s) : runtime_error(s) {}
	};

	/* small stack-buffered string, which can only be appended to, for intermediate/temporary value building, not null-terminated
	*	Implements: str::IsString, str::IsSink
	*	(if SilentError is true, any values written over the buffer-capacity are discarded, otherwise an exception is thrown) */
	template <str::IsChar ChType, size_t Capacity, bool SilentError = false>
		requires (Capacity > 0)
	class Small {
		using ThisType = str::Small<ChType, Capacity, SilentError>;
	private:
		ChType pBuffer[Capacity] = { 0 };
		detail::SizeType<Capacity> pSize = 0;

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
			if constexpr (SilentError)
				size = std::min<size_t>(size, Capacity - pSize);
			else if (Capacity - pSize < size)
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
		constexpr ChType& operator[](size_t index) {
			return pBuffer[index];
		}
		constexpr operator std::basic_string_view<ChType>() const {
			return std::basic_string_view<ChType>{ pBuffer, pBuffer + pSize };
		}
		constexpr operator std::basic_string<ChType>() const {
			return std::basic_string<ChType>{ pBuffer, pBuffer + pSize };
		}

	public:
		constexpr ThisType& assign(const str::IsString<ChType> auto& s) {
			return (*this = s);
		}
		constexpr ThisType& append(const str::IsString<ChType> auto& s) {
			return (*this += s);
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
		constexpr const ChType* begin() const {
			return pBuffer;
		}
		constexpr ChType* begin() {
			return pBuffer;
		}
		constexpr const ChType* end() const {
			return (pBuffer + pSize);
		}
		constexpr ChType* end() {
			return (pBuffer + pSize);
		}
		constexpr void clear() {
			pSize = 0;
		}
	};

	/* convenience for fast construction */
	template <size_t Capacity, bool SilentError = false>
	using ChSmall = str::Small<char, Capacity, SilentError>;
	template <size_t Capacity, bool SilentError = false>
	using WdSmall = str::Small<wchar_t, Capacity, SilentError>;
	template <size_t Capacity, bool SilentError = false>
	using U8Small = str::Small<char8_t, Capacity, SilentError>;
	template <size_t Capacity, bool SilentError = false>
	using U16Small = str::Small<char16_t, Capacity, SilentError>;
	template <size_t Capacity, bool SilentError = false>
	using U32Small = str::Small<char32_t, Capacity, SilentError>;
}
