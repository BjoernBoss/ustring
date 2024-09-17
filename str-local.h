/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-chars.h"

namespace str {
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
	struct LocalException : public str::RuntimeException {
		constexpr LocalException(const std::wstring& s) : str::RuntimeException{ s } {}
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
				throw str::LocalException(L"str::Local capacity exceeded");

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
				throw str::LocalException(L"str::Local capacity exceeded");

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
		constexpr size_t max_size() const {
			return ActCapacity;
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
	template <intptr_t Capacity>
	using LocalCh = str::Local<char, Capacity>;
	template <intptr_t Capacity>
	using LocalWd = str::Local<wchar_t, Capacity>;
	template <intptr_t Capacity>
	using LocalU8 = str::Local<char8_t, Capacity>;
	template <intptr_t Capacity>
	using LocalU16 = str::Local<char16_t, Capacity>;
	template <intptr_t Capacity>
	using LocalU32 = str::Local<char32_t, Capacity>;

	/* specializations for str::Local to be used as sink */
	template <class Type, intptr_t Capacity>
	struct CharWriter<str::Local<Type, Capacity>> {
		using ChType = Type;
		constexpr void operator()(str::Local<ChType, Capacity>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(str::Local<ChType, Capacity>& sink, const std::basic_string_view<ChType>& s) const {
			sink.append(s);
		}
	};
}
