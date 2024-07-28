/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-wire.h"

#include <vector>
#include <iostream>
#include <memory>

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

	/* [str::IsCollector] collect the sequence of codepoints into the corresponding sink
	*	Note: Must not outlive the sink object as it stores a reference to it */
	template <str::AnySink SinkType>
	struct Collect {
	private:
		SinkType& pSink;

	public:
		constexpr Collect(SinkType& sink) : pSink{ sink } {}

	public:
		constexpr void next(char32_t cp) {
			str::CodepointTo(pSink, cp, 1);
		}
		constexpr void done() {}
	};
	template <class SinkType>
	Collect(SinkType&) -> Collect<SinkType>;

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

	/* [str::IsSink] wrapper to create a sink which immediately passes the data to the wire and out to the corresponding wire
	*	Note: Must not outlive the sink or wire object as it stores a reference to it */
	template <str::IsWire WiType, char32_t CodeError>
	class WireOut {
	private:
		str::ToWire<CodeError>& pWire;
		WiType& pSink;

	public:
		WireOut(str::ToWire<CodeError>& wire, WiType& sink) : pWire{ wire }, pSink{ sink } {}
		WireOut(str::ToWire<CodeError>&& wire, WiType& sink) : pWire{ wire }, pSink{ sink } {}

	public:
		constexpr void write(const char32_t* str, size_t size) {
			pWire.write(pSink, std::u32string_view{ str, size });
		}
		constexpr void put(char32_t c) {
			pWire.write(pSink, std::u32string_view{ &c, 1 });
		}
	};
	template <class WiType, char32_t CodeError>
	WireOut(str::ToWire<CodeError>, WiType&) -> WireOut<WiType, CodeError>;

	/* [str::IsSink] structure to inherit from which can be used as a sink */
	struct InheritSink {
	public:
		constexpr InheritSink() = default;
		virtual ~InheritSink() = default;

	public:
		virtual void write(const char32_t* str, size_t size) = 0;
		virtual void write(char32_t chr, size_t count) = 0;
	};

	/* [str::IsWire] structure to inherit from which can be used as a wire */
	struct InheritWire {
	public:
		constexpr InheritWire() = default;
		virtual ~InheritWire() = default;

	public:
		virtual void write(const uint8_t* str, size_t size) = 0;
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
	template <class WiType, char32_t CodeError>
	struct CharWriter<str::WireOut<WiType, CodeError>, char32_t> {
		constexpr void operator()(str::WireOut<WiType, CodeError>& sink, char32_t chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::WireOut<WiType, CodeError>& sink, const char32_t* str, size_t size) const {
			sink.write(str, size);
		}
	};
	template <>
	struct CharWriter<str::InheritSink, char32_t> {
		constexpr void operator()(str::InheritSink& sink, char32_t chr, size_t count) const {
			sink.write(chr, count);
		}
		constexpr void operator()(str::InheritSink& sink, const char32_t* str, size_t size) const {
			sink.write(str, size);
		}
	};

	/* specializations to unpack various pointer types */
	template <class Type>
	struct CharWriter<std::unique_ptr<Type>, str::SinkChar<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(std::unique_ptr<Type>& sink, ChType chr, size_t count) const {
			str::CallSink<ChType>(*sink, chr, count);
		}
		constexpr void operator()(std::unique_ptr<Type>& sink, const ChType* str, size_t size) const {
			str::CallSink<ChType>(*sink, std::basic_string_view<ChType>{ str, size });
		}
	};
	template <class Type>
	struct CharWriter<std::shared_ptr<Type>, str::SinkChar<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(std::shared_ptr<Type>& sink, ChType chr, size_t count) const {
			str::CallSink<ChType>(*sink, chr, count);
		}
		constexpr void operator()(std::shared_ptr<Type>& sink, const ChType* str, size_t size) const {
			str::CallSink<ChType>(*sink, std::basic_string_view<ChType>{ str, size });
		}
	};
	template <class Type>
	struct CharWriter<Type*, str::SinkChar<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(Type* sink, ChType chr, size_t count) const {
			str::CallSink<ChType>(*sink, chr, count);
		}
		constexpr void operator()(Type* sink, const ChType* str, size_t size) const {
			str::CallSink<ChType>(*sink, std::basic_string_view<ChType>{ str, size });
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
		constexpr void operator()(str::Bytes& sink, const uint8_t* ptr, size_t size) const {
			sink.write(ptr, size);
		}
	};
	template <>
	struct ByteWriter<str::InheritWire> {
		constexpr void operator()(str::InheritWire& sink, const uint8_t* ptr, size_t size) const {
			sink.write(ptr, size);
		}
	};

	/* specializations to unpack various pointer types */
	template <class Type>
	struct ByteWriter<std::unique_ptr<Type>> {
		constexpr void operator()(std::unique_ptr<Type>& sink, const uint8_t* ptr, size_t size) const {
			str::CallWire(*sink, ptr, size);
		}
	};
	template <class Type>
	struct ByteWriter<std::shared_ptr<Type>> {
		constexpr void operator()(std::shared_ptr<Type>& sink, const uint8_t* ptr, size_t size) const {
			str::CallWire(*sink, ptr, size);
		}
	};
	template <class Type>
	struct ByteWriter<Type*> {
		constexpr void operator()(Type* sink, const uint8_t* ptr, size_t size) const {
			str::CallWire(*sink, ptr, size);
		}
	};
}
