/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-wire.h"
#include "str-coding.h"
#include "str-string.h"

#include <memory>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstring>

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
		constexpr void operator()(str::InheritSink& sink, std::u32string_view s) const {
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
		constexpr void operator()(str::WireOut<Type, CodeError>& sink, std::u32string_view s) const {
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
	template <str::IsSink Type>
	struct CharWriter<std::unique_ptr<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(std::unique_ptr<Type>& sink, ChType chr, size_t count) const {
			str::CallSink(*sink, chr, count);
		}
		constexpr void operator()(std::unique_ptr<Type>& sink, const std::basic_string_view<ChType>& s) const {
			str::CallSink(*sink, s);
		}
	};
	template <str::IsSink Type>
	struct CharWriter<std::shared_ptr<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(std::shared_ptr<Type>& sink, ChType chr, size_t count) const {
			str::CallSink(*sink, chr, count);
		}
		constexpr void operator()(std::shared_ptr<Type>& sink, const std::basic_string_view<ChType>& s) const {
			str::CallSink(*sink, s);
		}
	};
	template <str::IsSink Type>
	struct CharWriter<Type*> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(Type* sink, ChType chr, size_t count) const {
			str::CallSink(*sink, chr, count);
		}
		constexpr void operator()(Type* sink, const std::basic_string_view<ChType>& s) const {
			str::CallSink(*sink, s);
		}
	};
	template <str::IsSink Type>
	struct CharWriter<str::BufferSink<Type>> {
		using ChType = str::SinkChar<Type>;
		constexpr void operator()(str::BufferSink<Type>& sink, ChType chr, size_t count) const {
			sink.append(count, chr);
		}
		constexpr void operator()(str::BufferSink<Type>& sink, const std::basic_string_view<ChType>& s) const {
			sink.append(s);
		}
	};

	/* specializations for character streams */
	template <class Type>
	struct CharLoader<str::Stream<Type>> {
		using ChType = str::StreamChar<Type>;
		constexpr size_t operator()(str::Stream<Type>& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
	template <detail::IsImpl<std::basic_istream<char>> Type>
	struct CharLoader<Type> {
		using ChType = char;
		constexpr size_t operator()(std::basic_istream<char>& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size).gcount();
		}
	};
	template <detail::IsImpl<std::basic_istream<wchar_t>> Type>
	struct CharLoader<Type> {
		using ChType = wchar_t;
		constexpr size_t operator()(std::basic_istream<wchar_t>& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size).gcount();
		}
	};
	template <class Type>
	struct CharLoader<str::LimitStream<Type>> {
		using ChType = typename str::LimitStream<Type>::ChType;
		constexpr size_t operator()(str::LimitStream<Type>& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
	template <str::IsCharLoader Type>
	struct CharLoader<std::unique_ptr<Type>> {
		using ChType = str::CharLoaderChar<Type>;
		constexpr size_t operator()(std::unique_ptr<Type>& src, ChType* buffer, size_t size) const {
			return str::CallCharLoader(*src, buffer, size);
		}
	};
	template <str::IsCharLoader Type>
	struct CharLoader<std::shared_ptr<Type>> {
		using ChType = str::CharLoaderChar<Type>;
		constexpr size_t operator()(std::shared_ptr<Type>& src, ChType* buffer, size_t size) const {
			return str::CallCharLoader(*src, buffer, size);
		}
	};
	template <str::IsCharLoader Type>
	struct CharLoader<Type*> {
		using ChType = str::CharLoaderChar<Type>;
		constexpr size_t operator()(Type* src, ChType* buffer, size_t size) const {
			return str::CallCharLoader(*src, buffer, size);
		}
	};
	template <class Type, char32_t CodeError>
	struct CharLoader<str::WireIn<Type, CodeError>> {
		using ChType = char32_t;
		constexpr size_t operator()(str::WireIn<Type, CodeError>& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
	template <class Type, char32_t CodeError>
	struct CharLoader<str::U32Stream<Type, CodeError>> {
		using ChType = char32_t;
		constexpr size_t operator()(str::U32Stream<Type, CodeError>& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
	template <>
	struct CharLoader<str::InheritStream> {
		using ChType = char32_t;
		constexpr size_t operator()(str::InheritStream& src, ChType* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};

	/* specializations for byte-reader */
	template <>
	struct ByteReader<str::Data> {
		constexpr str::Data operator()(const str::Data& data) {
			return data;
		}
	};
	template <>
	struct ByteReader<std::vector<uint8_t>> {
		constexpr str::Data operator()(const std::vector<uint8_t>& data) {
			return { data.data(), data.size() };
		}
	};
	template <>
	struct ByteReader<std::span<uint8_t>> {
		constexpr str::Data operator()(const std::span<uint8_t>& data) {
			return { data.data(), data.size() };
		}
	};
	template <size_t Size>
	struct ByteReader<uint8_t[Size]> {
		constexpr str::Data operator()(const uint8_t(&data)[Size]) {
			return { data, Size };
		}
	};

	/* specializations for byte-writers */
	template <>
	struct ByteWriter<std::vector<uint8_t>> {
		constexpr void operator()(std::vector<uint8_t>& sink, const str::Data& d) const {
			sink.insert(sink.end(), d.data(), d.data() + d.size());
		}
	};
	template <detail::IsImpl<std::ostream> Type>
	struct ByteWriter<Type> {
		void operator()(Type& sink, const str::Data& d) const {
			sink.write(reinterpret_cast<const char*>(d.data()), d.size());
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
	template <str::IsWire Type>
	struct ByteWriter<std::unique_ptr<Type>> {
		constexpr void operator()(std::unique_ptr<Type>& sink, const str::Data& d) const {
			str::CallWire(*sink, d);
		}
	};
	template <str::IsWire Type>
	struct ByteWriter<std::shared_ptr<Type>> {
		constexpr void operator()(std::shared_ptr<Type>& sink, const str::Data& d) const {
			str::CallWire(*sink, d);
		}
	};
	template <str::IsWire Type>
	struct ByteWriter<Type*> {
		constexpr void operator()(Type* sink, const str::Data& d) const {
			str::CallWire(*sink, d);
		}
	};
	template <str::IsWire Type>
	struct ByteWriter<str::BufferWire<Type>> {
		constexpr void operator()(str::BufferWire<Type>& sink, const str::Data& d) const {
			sink.write(d);
		}
	};

	/* specializations for byte streams */
	template <class Type>
	struct ByteLoader<str::Source<Type>> {
		constexpr size_t operator()(str::Source<Type>& src, uint8_t* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
	template <detail::IsImpl<std::istream> Type>
	struct ByteLoader<Type> {
		constexpr size_t operator()(std::istream& src, uint8_t* buffer, size_t size) const {
			return src.read(reinterpret_cast<char*>(buffer), size).gcount();
		}
	};
	template <class Type>
	struct ByteLoader<str::LimitSource<Type>> {
		constexpr size_t operator()(str::LimitSource<Type>& src, uint8_t* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
	template <str::IsSource Type>
	struct ByteLoader<std::unique_ptr<Type>> {
		constexpr size_t operator()(std::unique_ptr<Type>& src, uint8_t* buffer, size_t size) const {
			return str::CallByteLoader(*src, buffer, size);
		}
	};
	template <str::IsSource Type>
	struct ByteLoader<std::shared_ptr<Type>> {
		constexpr size_t operator()(std::shared_ptr<Type>& src, uint8_t* buffer, size_t size) const {
			return str::CallByteLoader(*src, buffer, size);
		}
	};
	template <str::IsSource Type>
	struct ByteLoader<Type*> {
		constexpr size_t operator()(Type* src, uint8_t* buffer, size_t size) const {
			return str::CallByteLoader(*src, buffer, size);
		}
	};
	template <>
	struct ByteLoader<str::InheritSource> {
		constexpr size_t operator()(str::InheritSource& src, uint8_t* buffer, size_t size) const {
			return src.read(buffer, size);
		}
	};
}
