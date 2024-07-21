#pragma once

#include "str-common.h"

#include <vector>
#include <iostream>

namespace str {
	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer with a null-byte (if capacity is greater than zero) */
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

	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer and make the written size available */
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
		constexpr void write(const uint8_t* ptr, size_t sz) {
			if (sz > pSize - pOffset) {
				sz = pSize - pOffset;
				pOverflow = true;
			}
			std::copy(ptr, ptr + sz, pPtr + pOffset);
			pOffset += sz;
		}
		constexpr size_t size() const {
			return pOffset;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsCollector] collect the sequence of codepoints into the corresponding sink */
	template <str::AnySink SinkType>
	struct Collect {
	private:
		SinkType&& pSink;

	public:
		constexpr Collect(SinkType&& sink) : pSink{ sink } {}

	public:
		constexpr void next(char32_t cp) {
			str::CodepointTo(pSink, cp, 1);
		}
		constexpr void done() {}
	};
	template <class SinkType>
	Collect(SinkType&) -> Collect<SinkType&>;
	template <class SinkType>
	Collect(SinkType&&) -> Collect<SinkType>;

	/* [str::IsCollector] collect the sequence of codepoints and pass them to the corresponding callable object */
	template <str::IsReceiver<char32_t> CallType>
	struct ForEach {
	private:
		CallType pSink;

	public:
		constexpr ForEach(CallType&& sink) : pSink{ std::forward<CallType>(sink) } {}

	public:
		constexpr void next(char32_t cp) {
			pSink(cp);
		}
		constexpr void done() {}
	};

	/* specializations for char-writers */
	template <class ChType>
	struct CharWriter<std::basic_string<ChType>, ChType> {
		constexpr void operator()(std::basic_string<ChType>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(std::basic_string<ChType>& sink, const ChType* str, size_t size) const {
			sink.append(str, size);
		}
	};
	template <class ChType, intptr_t Capacity>
	struct CharWriter<str::Local<ChType, Capacity>, ChType> {
		constexpr void operator()(str::Local<ChType, Capacity>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(str::Local<ChType, Capacity>& sink, const ChType* str, size_t size) const {
			sink.append(str, size);
		}
	};
	template <class ChType>
	struct CharWriter<std::basic_ostream<ChType>, ChType> {
		constexpr void operator()(std::basic_ostream<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(std::basic_ostream<ChType>& sink, const ChType* str, size_t size) const {
			sink.write(str, size);
		}
	};
	template <class ChType>
	struct CharWriter<str::NullChars<ChType>, ChType> {
		constexpr void operator()(str::NullChars<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::NullChars<ChType>& sink, const ChType* str, size_t size) const {
			sink.write(str, size);
		}
	};
	template <class ChType>
	struct CharWriter<str::Chars<ChType>, ChType> {
		constexpr void operator()(str::Chars<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::Chars<ChType>& sink, const ChType* str, size_t size) const {
			sink.write(str, size);
		}
	};

	/* specializations for byte-writers */
	template <>
	struct ByteWriter<std::vector<uint8_t>> {
		constexpr void operator()(std::vector<uint8_t>& sink, const uint8_t* ptr, size_t size) const {
			sink.insert(sink.end(), ptr, ptr + size);
		}
	};
	template <>
	struct ByteWriter<std::ostream> {
		void operator()(std::ostream& sink, const uint8_t* ptr, size_t size) const {
			sink.write(reinterpret_cast<const char*>(ptr), size);
		}
	};
	template <>
	struct ByteWriter<str::Bytes> {
		void operator()(str::Bytes& sink, const uint8_t* ptr, size_t size) const {
			sink.write(ptr, size);
		}
	};
}
