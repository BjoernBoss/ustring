/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2026 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-chars.h"

namespace str {
	namespace detail {
		template <class Type>
		class LocalQueue {
		private:
			static constexpr size_t BufferSize = 4;

		private:
			struct Static {
				Type buffer[BufferSize]{};
			};
			using Dynamic = std::vector<Type>;

		private:
			std::variant<Static, Dynamic> pBuffer;
			Type* pStart = nullptr;
			size_t pSize = 0;

		public:
			constexpr LocalQueue() : pBuffer{ Static{} } {
				pStart = std::get<Static>(pBuffer).buffer;
			}
			constexpr LocalQueue(const LocalQueue& o) : pBuffer{ o.pBuffer }, pSize{ o.pSize } {
				pStart = fGetBase() + o.fGetOffset();
			}
			constexpr LocalQueue(LocalQueue&& o) {
				size_t offset = o.fGetOffset();
				std::swap(pBuffer, o.pBuffer);
				pSize = std::exchange(o.pSize, 0);
				pStart = fGetBase() + offset;
			}
			constexpr LocalQueue& operator=(const LocalQueue& o) {
				if (this != &o) {
					pBuffer = o.pBuffer;
					pSize = o.pSize;
					pStart = fGetBase() + o.fGetOffset();
				}
				return *this;
			}
			constexpr LocalQueue& operator=(LocalQueue&& o) {
				if (this != &o) {
					size_t offset = o.fGetOffset();
					std::swap(pBuffer, o.pBuffer);
					pSize = std::exchange(o.pSize, 0);
					pStart = fGetBase() + offset;
				}
				return *this;
			}

		private:
			Type* fGetBase() {
				return (std::holds_alternative<Static>(pBuffer) ? std::get<Static>(pBuffer).buffer : std::get<Dynamic>(pBuffer).data());
			}
			size_t fGetOffset() {
				return (pStart - fGetBase());
			}

		public:
			constexpr void push(const Type& t) {
				/* copy the value, as it might be a reference into this buffer itself */
				Type value = t;

				/* check if the buffer needs to be resized */
				if (std::holds_alternative<Static>(pBuffer)) {
					Static& s = std::get<Static>(pBuffer);
					if (pSize == 0)
						pStart = s.buffer;
					else if (size_t(pStart - s.buffer) + pSize == BufferSize) {
						Dynamic d; d.reserve(2 * BufferSize);
						d.assign(pStart, pStart + pBuffer);
						d.resize(2 * BufferSize);
						pStart = (pBuffer = std::move(d)).data();
					}
				}
				else {
					Dynamic& d = std::get<Dynamic>(pBuffer);
					if (pSize == 0)
						pStart = d.data();
					else if (size_t off = size_t(pStart - d.data()); off + pSize == d.size()) {
						d.resize(pSize + BufferSize);
						pStart = d.data() + off;
					}
				}

				/* write the new value to the queue */
				pStart[pSize++] = value;
			}
			constexpr Type pop() {
				--pSize;
				return *(pStart++);
			}
			constexpr void clear() {
				pSize = 0;
			}
			constexpr size_t size() const {
				return pSize;
			}
			constexpr Type& get(size_t i) {
				return pStart[i];
			}
			constexpr Type& front() {
				return pStart[0];
			}
			constexpr Type& back() {
				return pStart[pSize - 1];
			}
			constexpr Type* begin() {
				return pStart;
			}
			constexpr Type* end() {
				return pStart + pSize;
			}
		};

		struct MatchCollector {
		public:
			detail::LocalQueue<char32_t> expected;
			bool matches = true;

		public:
			constexpr void next(char32_t cp) {
				if (expected.size() == 0 || expected.pop() != cp)
					matches = false;
			}
			constexpr void done() {}
			constexpr bool completed() {
				return (matches && expected.size() == 0);
			}
		};

		/* optimal unsigned integer size-type to be able to hold the given capacity */
		template <size_t Capacity> using SizeType8Or16 = std::conditional_t<Capacity <= std::numeric_limits<uint8_t>::max(), uint8_t, uint16_t>;
		template <size_t Capacity> using SizeType32OrLess = std::conditional_t<Capacity <= std::numeric_limits<uint16_t>::max(), detail::SizeType8Or16<Capacity>, uint32_t>;
		template <size_t Capacity> using SizeType64OrLess = std::conditional_t<Capacity <= std::numeric_limits<uint32_t>::max(), detail::SizeType32OrLess<Capacity>, uint64_t>;
		template <size_t Capacity> using SizeType = std::conditional_t<Capacity <= std::numeric_limits<uint64_t>::max(), detail::SizeType64OrLess<Capacity>, size_t>;
	}

	/* local string-buffer overflow/underflow exception */
	struct LocalException : public str::RuntimeException<char> {
		LocalException(const std::string& s) : str::RuntimeException<char>{ s } {}
	};

	/* [str::IsStr/str::IsSink] local stack-buffered string null-terminated string, to be appended to, for intermediate/temporary value building
	*	If capacity is negative any values written over the buffer-capacity are discarded, otherwise an exception is thrown */
	template <str::IsChar ChType, intptr_t Capacity>
		requires (Capacity != 0)
	class Local {
		using ThisType = str::Local<ChType, Capacity>;
		static constexpr size_t ActCapacity = size_t(Capacity < 0 ? -Capacity : Capacity);
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
				throw str::LocalException{ "str::Local capacity exceeded" };

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
				throw str::LocalException{ "str::Local capacity exceeded" };

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
			return std::basic_string_view<ChType>{ pBuffer, pSize };
		}

	public:
		constexpr std::basic_string_view<ChType> view() const {
			return std::basic_string_view<ChType>{ pBuffer, pSize };
		}
		constexpr std::basic_string<ChType> str() const {
			return std::basic_string<ChType>{ pBuffer, pSize };
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
			return size_t(pSize);
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
