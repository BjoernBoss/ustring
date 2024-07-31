/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-types.h"

#include <stdexcept>
#include <algorithm>
#include <cwchar>
#include <vector>
#include <variant>

namespace str {
	namespace detail {
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

	/* local stack-buffered string null-terminated string, to be appended to, for intermediate/temporary value building
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
	}

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
}
