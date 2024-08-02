/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-wire.h"
#include "str-coding.h"
#include "str-string.h"

#include <vector>
#include <memory>
#include <iostream>
#include <fstream>
#include <sstream>

namespace str {
	namespace detail {
		template <class Derived, class Base>
		concept IsImpl = std::is_base_of_v<Base, Derived>;
	}

	/* specializations for char-writers */
	template <class Type>
	struct CharWriter<std::basic_string<Type>> {
		using ChType = Type;
		constexpr void operator()(std::basic_string<ChType>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(std::basic_string<ChType>& sink, const std::basic_string_view<ChType>& s) const {
			sink.append(s);
		}
	};
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
	template <class Type, char32_t CodeError>
	struct CharWriter<str::String<Type, CodeError>> {
		using ChType = Type;
		constexpr void operator()(str::String<ChType, CodeError>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(str::String<ChType, CodeError>& sink, const std::basic_string_view<ChType>& s) const {
			sink.append(s);
		}
	};
	template <class Type>
	struct CharWriter<str::NullChars<Type>> {
		using ChType = Type;
		constexpr void operator()(str::NullChars<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::NullChars<ChType>& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s);
		}
	};
	template <class Type>
	struct CharWriter<str::Chars<Type>> {
		using ChType = Type;
		constexpr void operator()(str::Chars<ChType>& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::Chars<ChType>& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s);
		}
	};
	template <>
	struct CharWriter<str::InheritSink> {
		using ChType = char32_t;
		constexpr void operator()(str::InheritSink& sink, char32_t chr, size_t count) const {
			sink.write(chr, count);
		}
		constexpr void operator()(str::InheritSink& sink, const std::u32string_view& s) const {
			sink.write(s);
		}
	};
	template <class Type, char32_t CodeError>
	struct CharWriter<str::WireOut<Type, CodeError>> {
		using ChType = char32_t;
		constexpr void operator()(str::WireOut<Type, CodeError>& sink, char32_t chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(str::WireOut<Type, CodeError>& sink, const std::u32string_view& s) const {
			sink.write(s);
		}
	};
	template <detail::IsImpl<std::basic_ostream<char>> Type>
	struct CharWriter<Type> {
		using ChType = char;
		constexpr void operator()(Type& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(Type& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s.data(), s.size());
		}
	};
	template <detail::IsImpl<std::basic_ostream<wchar_t>> Type>
	struct CharWriter<Type> {
		using ChType = wchar_t;
		constexpr void operator()(Type& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(Type& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s.data(), s.size());
		}
	};
	template <detail::IsImpl<std::basic_ostream<char8_t>> Type>
	struct CharWriter<Type> {
		using ChType = char8_t;
		constexpr void operator()(Type& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(Type& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s.data(), s.size());
		}
	};
	template <detail::IsImpl<std::basic_ostream<char16_t>> Type>
	struct CharWriter<Type> {
		using ChType = char16_t;
		constexpr void operator()(Type& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(Type& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s.data(), s.size());
		}
	};
	template <detail::IsImpl<std::basic_ostream<char32_t>> Type>
	struct CharWriter<Type> {
		using ChType = char32_t;
		constexpr void operator()(Type& sink, ChType chr, size_t count) const {
			for (size_t i = 0; i < count; ++i)
				sink.put(chr);
		}
		constexpr void operator()(Type& sink, const std::basic_string_view<ChType>& s) const {
			sink.write(s.data(), s.size());
		}
	};

	/* specializations to unpack various pointer types */
	template <class Type>
	struct CharWriter<std::unique_ptr<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(std::unique_ptr<Type>& sink, ChType chr, size_t count) const {
			str::CallSink(*sink, chr, count);
		}
		constexpr void operator()(std::unique_ptr<Type>& sink, const std::basic_string_view<ChType>& s) const {
			str::CallSink(*sink, s);
		}
	};
	template <class Type>
	struct CharWriter<std::shared_ptr<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(std::shared_ptr<Type>& sink, ChType chr, size_t count) const {
			str::CallSink(*sink, chr, count);
		}
		constexpr void operator()(std::shared_ptr<Type>& sink, const std::basic_string_view<ChType>& s) const {
			str::CallSink(*sink, s);
		}
	};
	template <class Type>
	struct CharWriter<Type*> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(Type* sink, ChType chr, size_t count) const {
			str::CallSink(*sink, chr, count);
		}
		constexpr void operator()(Type* sink, const std::basic_string_view<ChType>& s) const {
			str::CallSink(*sink, s);
		}
	};

	/* specializations for byte-writers */
	template <>
	struct ByteWriter<std::vector<uint8_t>> {
		constexpr void operator()(std::vector<uint8_t>& sink, const str::Data& d) const {
			sink.insert(sink.end(), d.ptr, d.ptr + d.size);
		}
	};
	template <detail::IsImpl<std::ostream> Type>
	struct ByteWriter<Type> {
		void operator()(Type& sink, const str::Data& d) const {
			sink.write(reinterpret_cast<const char*>(d.ptr), d.size);
		}
	};
	template <>
	struct ByteWriter<str::Bytes> {
		constexpr void operator()(str::Bytes& sink, const str::Data& d) const {
			sink.write(d);
		}
	};
	template <>
	struct ByteWriter<str::InheritWire> {
		constexpr void operator()(str::InheritWire& sink, const str::Data& d) const {
			sink.write(d);
		}
	};

	/* specializations to unpack various pointer types */
	template <class Type>
	struct ByteWriter<std::unique_ptr<Type>> {
		constexpr void operator()(std::unique_ptr<Type>& sink, const str::Data& d) const {
			str::CallWire(*sink, d);
		}
	};
	template <class Type>
	struct ByteWriter<std::shared_ptr<Type>> {
		constexpr void operator()(std::shared_ptr<Type>& sink, const str::Data& d) const {
			str::CallWire(*sink, d);
		}
	};
	template <class Type>
	struct ByteWriter<Type*> {
		constexpr void operator()(Type* sink, const str::Data& d) const {
			str::CallWire(*sink, d);
		}
	};

	/* specializations to allow for interactions between str::Stream */
	template <str::IsStream Type>
	struct CharStream<str::Stream<Type>> {
		using ChType = str::StreamChar<Type>;
	private:
		str::Stream<Type>& pStream;

	public:
		constexpr CharStream(str::Stream<Type>& s) : pStream{ s } {}

	public:
		constexpr std::basic_string_view<ChType> load(size_t count) {
			return pStream.load(count);
		}
		constexpr void consume(size_t count) {
			pStream.consume(count);
		}
		constexpr bool done() const {
			return pStream.done();
		}
	};

	/* specializations for character streams */
	template <str::IsChStr<char> Type>
	struct CharStream<Type> {
		using ChType = char;
	private:
		std::string_view pView;

	public:
		constexpr CharStream(const Type& s) : pView{ s } {}

	public:
		constexpr std::string_view load(size_t count) {
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return pView.empty();
		}
	};
	template <str::IsChStr<wchar_t> Type>
	struct CharStream<Type> {
		using ChType = wchar_t;
	private:
		std::wstring_view pView;

	public:
		constexpr CharStream(const Type& s) : pView{ s } {}

	public:
		constexpr std::wstring_view load(size_t count) {
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return pView.empty();
		}
	};
	template <str::IsChStr<char8_t> Type>
	struct CharStream<Type> {
		using ChType = char8_t;
	private:
		std::u8string_view pView;

	public:
		constexpr CharStream(const Type& s) : pView{ s } {}

	public:
		constexpr std::u8string_view load(size_t count) {
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return pView.empty();
		}
	};
	template <str::IsChStr<char16_t> Type>
	struct CharStream<Type> {
		using ChType = char16_t;
	private:
		std::u16string_view pView;

	public:
		constexpr CharStream(const Type& s) : pView{ s } {}

	public:
		constexpr std::u16string_view load(size_t count) {
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return pView.empty();
		}
	};
	template <str::IsChStr<char32_t> Type>
	struct CharStream<Type> {
		using ChType = char32_t;
	private:
		std::u32string_view pView;

	public:
		constexpr CharStream(const Type& s) : pView{ s } {}

	public:
		constexpr std::u32string_view load(size_t count) {
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return pView.empty();
		}
	};
	template <class Type, char32_t CodeError>
	struct CharStream<str::WireIn<Type, CodeError>> {
		using ChType = char32_t;
		static constexpr size_t BytesPerIteration = 256;
	private:
		str::WireIn<Type, CodeError>& pStream;

	public:
		constexpr CharStream(str::WireIn<Type, CodeError>& s) : pStream{ s } {}

	public:
		constexpr std::u32string_view load(size_t count) {
			/* check if the capacity is already available */
			if (count <= pStream.pView.size())
				return pStream.pView;

			/* check if the offset should be reset */
			size_t offset = (pStream.pView.empty() ? 0 : pStream.pView.data() - pStream.pBuffer.data());
			if (offset > pStream.pView.size()) {
				pStream.pBuffer = pStream.pBuffer.substr(offset);
				offset = 0;
			}
			else
				count += offset;

			/* iterate until the given number of characters has been produced or until the end has been reached */
			while (pStream.pBuffer.size() < count) {
				str::Data data = pStream.pSource.load(BytesPerIteration);

				/* feed the data through the wire and consume them from the source */
				pStream.pWire.readTo(pStream.pBuffer, data);
				pStream.pSource.consume(data.size);

				/* check if the end has been reached */
				if (data.size >= BytesPerIteration)
					continue;
				pStream.pWire.lastTo(pStream.pBuffer, {});
				break;
			}

			/* update the view */
			pStream.pView = std::u32string_view{ pStream.pBuffer }.substr(offset);
			return pStream.pView;
		}
		constexpr void consume(size_t count) {
			pStream.pView = pStream.pView.substr(std::min<size_t>(count, pStream.pView.size()));
		}
		constexpr bool done() const {
			return (pStream.pView.empty() && pStream.pSource.done());
		}
	};

	/* specializations to allow for interactions between str::Source */
	template <str::IsSource Type>
	struct ByteSource<str::Source<Type>> {
	private:
		str::Source<Type>& pSource;

	public:
		constexpr ByteSource(str::Source<Type>& s) : pSource{ s } {}

	public:
		constexpr str::Data load(size_t count) {
			return pSource.load(count);
		}
		constexpr void consume(size_t count) {
			pSource.consume(count);
		}
		constexpr bool done() const {
			return pSource.done();
		}
	};

	/* specializations for byte streams */
	template <std::convertible_to<str::Data> Type>
	struct ByteSource<Type> {
	private:
		str::Data pData;

	public:
		constexpr ByteSource(const str::Data& d) : pData{ d } {}

	public:
		constexpr str::Data load(size_t count) {
			return pData;
		}
		constexpr void consume(size_t count) {
			pData = pData.subdata(std::min<size_t>(count, pData.size));
		}
		constexpr bool done() const {
			return pData.empty();
		}
	};
	template <detail::IsImpl<std::istream> Type>
	struct ByteSource<Type> {
		static constexpr size_t MinBytesToLoad = 256;
	private:
		std::istream& pStream;
		std::vector<uint8_t> pBuffer;
		str::Data pData;

	public:
		constexpr ByteSource(Type& s) : pStream{ s } {}

	public:
		constexpr str::Data load(size_t count) {
			/* check if the capacity is already available */
			if (count <= pData.size)
				return pData;
			count = std::max<size_t>(count, MinBytesToLoad);

			/* check if the buffer should be reset and reset it to have enough capacity */
			size_t offset = (pData.empty() ? 0 : pData.ptr - pBuffer.data());
			if (offset >= pData.size) {
				std::memmove(pBuffer.data(), pData.ptr, pData.size);
				offset = 0;
			}
			pBuffer.resize(offset + count);

			/* setup the data-object and read the actual data from the stream */
			uint8_t* data = pBuffer.data() + offset;
			pStream.read(reinterpret_cast<char*>(data), count);
			pData = { data, pData.size + pStream.gcount() };
			return pData;
		}
		constexpr void consume(size_t count) {
			pData = pData.subdata(std::min<size_t>(count, pData.size));
		}
		constexpr bool done() const {
			return (pData.empty() && pStream.eof());
		}
	};
}
