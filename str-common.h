#pragma once

#include <string>
#include <concepts>
#include <limits>
#include <cinttypes>
#include <type_traits>
#include <stdexcept>
#include <utility>

namespace str {
	/* is type a supported character (not convertible, but exact type!) */
	template <class Type>
	concept IsChar = (std::same_as<Type, char> || std::same_as<Type, wchar_t> || std::same_as<Type, char8_t> || std::same_as<Type, char16_t> || std::same_as<Type, char32_t>);

	namespace detail {
		template <class Type, class ChType> concept IsCharString = requires(const Type t, size_t n) {
			{ t[n] } -> std::convertible_to<ChType>;
			{ t } -> std::convertible_to<std::basic_string_view<ChType>>;
		};
		template <class Type, class ChType> concept IsCharWritable = requires(Type t, ChType c) {
			{  t.push_back(c) };
			{ t.append(&c) };
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

		/* read the size of the object */
		template <class ChType, class Type>
		size_t GetSize(const Type& s) {
			return std::basic_string_view<ChType>{ s }.size();
		}
	}

	/* is type a character string or like a string_view (.data, .size) and get corresponding char-type */
	template <class Type>
	concept AnyString = detail::AnyCharString<Type>;
	template <class Type, class ChType>
	concept IsString = detail::IsCharString<Type, ChType>;
	template <str::AnyString Type>
	using StringChar = typename detail::GetCharString<Type>::type;

	/* type has a push_back and is like a string_view (.data, .size) and get corresponding char-type */
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
	*	(if SilentError is true, any values written over the buffer-size are discarded, otherwise an exception is thrown) */
	template <str::IsChar ChType, size_t Size, bool SilentError = false>
		requires (Size > 0)
	class Small {
		using ThisType = str::Small<ChType, Size, SilentError>;
	private:
		ChType pBuffer[Size] = { 0 };
		detail::SizeType<Size> pSize = 0;

	public:
		Small() = default;
		Small(const ThisType&) = default;
		Small(ThisType&&) = default;
		Small(const str::IsString<ChType> auto& s) {
			fAppend(s);
		}

	private:
		void fAppend(const auto& s) {
			size_t size = detail::GetSize<ChType>(s);

			/* check if an error should be thrown or the buffer should only be filled up to the end */
			if constexpr (SilentError)
				size = std::min<size_t>(size, Size - pSize);
			else if (Size - pSize < size)
				throw str::BufferException("str::Small capacity exceeded");

			/* write the data to the buffer */
			for (size_t i = 0; i < size; ++i)
				pBuffer[pSize++] = s[i];
		}

	public:
		ThisType& operator+=(const str::IsString<ChType> auto& s) {
			fAppend(s);
			return *this;
		}
		const ChType& operator[](size_t index) const {
			return pBuffer[index];
		}
		operator std::basic_string_view<ChType>() const {
			return std::basic_string_view<ChType>{ pBuffer, pBuffer + pSize };
		}
		operator std::basic_string<ChType>() const {
			return std::basic_string<ChType>{ pBuffer, pBuffer + pSize };
		}

	public:
		ThisType& append(const str::IsString<ChType> auto& s) {
			return (*this += s);
		}
		void push_back(ChType c) {
			fAppend(std::basic_string_view<ChType>{ &c, 1 });
		}
		size_t size() const {
			return static_cast<size_t>(pSize);
		}
		bool empty() const {
			return (pSize == 0);
		}
		const ChType* begin() const {
			return pBuffer;
		}
		const ChType* end() const {
			return (pBuffer + pSize);
		}
	};
}
