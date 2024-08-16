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

		template <class ChType>
		struct IStreamWrapper {
			static constexpr size_t MinBytesToLoad = 256;
		private:
			std::basic_istream<ChType>& pStream;
			std::vector<ChType> pBuffer;
			std::basic_string_view<ChType> pView;
			bool pClosed = false;

		public:
			template <class CType>
			constexpr IStreamWrapper(CType&& s) : pStream{ std::forward<CType>(s) } {}

		public:
			constexpr std::basic_string_view<ChType> load(size_t count) {
				/* check if the capacity is already available or if the source is done */
				if (count <= pView.size() || pClosed)
					return pView;
				count = std::max<size_t>(count, MinBytesToLoad);

				/* check if the buffer should be reset and reset it to have enough capacity */
				size_t offset = (pView.empty() ? 0 : pView.data() - pBuffer.data());
				if (offset >= pView.size()) {
					std::memmove(pBuffer.data(), pView.data(), pView.size());
					offset = 0;
				}
				size_t endOfData = offset + pView.size();
				pBuffer.resize(endOfData + count);

				/* setup the data-object and read the actual data from the stream */
				pStream.read(reinterpret_cast<ChType*>(pBuffer.data() + endOfData), count);
				count = pStream.gcount();
				pView = { pBuffer.data() + offset, pView.size() + count };
				pClosed = (count == 0);
				return pView;
			}
			constexpr void consume(size_t count) {
				pView = pView.substr(std::min<size_t>(count, pView.size()));
			}
			constexpr bool done() const {
				return (pView.empty() && pStream.eof());
			}
		};
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

	/* specializations for character streams */
	template <str::IsStream Type>
	struct CharStream<str::Stream<Type>> {
		using ChType = str::StreamChar<Type>;
	private:
		str::Stream<Type>& pStream;

	public:
		template <class CType>
		constexpr CharStream(CType&& s) : pStream{ std::forward<CType>(s) } {}

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
	template <class Type>
	struct CharStream<str::LimitStream<Type>> {
		using ChType = str::StreamChar<Type>;
	private:
		str::Stream<Type>& pStream;
		size_t pCount = 0;

	public:
		template <class CType>
		constexpr CharStream(CType&& s) : pStream{ s.pStream }, pCount{ s.pCount } {}

	public:
		constexpr std::u32string_view load(size_t count) {
			std::u32string_view str = pStream.load(std::min<size_t>(count, pCount));
			return (str.size() > pCount ? str.substr(0, pCount) : str);
		}
		constexpr void consume(size_t count) {
			count = std::min<size_t>(count, pCount);
			pCount -= count;
			pStream.consume(count);
		}
		constexpr bool done() const {
			return (pCount == 0 || pStream.done());
		}
	};
	template <class Type, char32_t CodeError>
	struct CharStream<str::WireIn<Type, CodeError>> {
		using ChType = char32_t;
		static constexpr size_t BytesPerIteration = 256;
	private:
		str::Source<Type>& pSource;
		str::FromWire<CodeError> pWire;
		std::u32string pBuffer;
		std::u32string_view pView;

	public:
		template <class CType>
		constexpr CharStream(CType&& s) : pSource{ s.pSource }, pWire{ s.pCoding, s.pMode } {}

	public:
		constexpr std::u32string_view load(size_t count) {
			/* check if the capacity is already available */
			if (count <= pView.size())
				return pView;

			/* check if the offset should be reset */
			size_t offset = (pView.empty() ? 0 : pView.data() - pBuffer.data());
			if (offset > pView.size()) {
				pBuffer = pBuffer.substr(offset);
				offset = 0;
			}
			else
				count += offset;

			/* iterate until the given number of characters has been produced or until the end has been reached */
			while (pBuffer.size() < count) {
				str::Data data = pSource.load(BytesPerIteration);

				/* feed the data through the wire and consume them from the source */
				pWire.readTo(pBuffer, data);
				pSource.consume(data.size());

				/* check if the end has been reached */
				if (data.size() >= BytesPerIteration)
					continue;
				pWire.lastTo(pBuffer, {});
				break;
			}

			/* update the view */
			pView = std::u32string_view{ pBuffer }.substr(offset);
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return (pView.empty() && pSource.done());
		}
	};
	template <class DChType, class Type, char32_t CodeError>
	struct CharStream<str::Convert<DChType, Type, CodeError>> {
		using ChType = DChType;
		using SChType = str::StreamChar<Type>;
		static constexpr size_t CharsPerIteration = 64;
	private:
		str::Stream<Type>& pStream;
		std::basic_string<DChType> pBuffer;
		std::basic_string_view<DChType> pView;
		bool pClosed = false;

	public:
		template <class CType>
		constexpr CharStream(CType&& s) : pStream{ s.pStream } {}

	public:
		constexpr std::basic_string_view<ChType> load(size_t count) {
			/* check if the capacity is already available or if the source is closed */
			if (count <= pView.size() || pClosed)
				return pView;
			count = std::max<size_t>(count, CharsPerIteration + pView.size());

			/* check if the offset should be reset */
			size_t offset = (pView.empty() ? 0 : pView.data() - pBuffer.data());
			if (offset > pView.size()) {
				pBuffer = pBuffer.substr(offset);
				offset = 0;
			}
			else
				count += offset;

			/* iterate until the given number of characters has been produced or until the end has been reached */
			while (pBuffer.size() < count) {
				auto [cp, consumed] = str::PartialTranscode<DChType, CodeError>(pStream.load(str::MaxEncSize<SChType>));

				/* check if the end has been reached and trigger a final transcoding to ensure proper error-handling on incomplete characters) */
				if (consumed == 0) {
					auto [cp, consumed] = str::GetTranscode<DChType, CodeError>(pStream.load(str::MaxEncSize<SChType>));
					pStream.consume(consumed);
					pBuffer.append(cp);
					pClosed = true;
					break;
				}

				/* write the actual codepoint out to the buffer and consume it */
				pStream.consume(consumed);
				pBuffer.append(cp);
			}

			/* update the view */
			pView = std::u32string_view{ pBuffer }.substr(offset);
			return pView;
		}
		constexpr void consume(size_t count) {
			pView = pView.substr(std::min<size_t>(count, pView.size()));
		}
		constexpr bool done() const {
			return (pView.empty() && pStream.done());
		}
	};
	template <detail::IsImpl<std::basic_istream<char>> Type>
	struct CharStream<Type> {
		using ChType = char;
	private:
		detail::IStreamWrapper<char> pStream;

	public:
		template <class CType>
		constexpr CharStream(CType&& s) : pStream{ std::forward<CType>(s) } {}

	public:
		constexpr std::string_view load(size_t count) {
			return pStream.load(count);
		}
		constexpr void consume(size_t count) {
			pStream.consume(count);
		}
		constexpr bool done() const {
			return pStream.done();
		}
	};
	template <detail::IsImpl<std::basic_istream<wchar_t>> Type>
	struct CharStream<Type> {
		using ChType = wchar_t;
	private:
		detail::IStreamWrapper<wchar_t> pStream;

	public:
		template <class CType>
		constexpr CharStream(CType&& s) : pStream{ std::forward<CType>(s) } {}

	public:
		constexpr std::wstring_view load(size_t count) {
			return pStream.load(count);
		}
		constexpr void consume(size_t count) {
			pStream.consume(count);
		}
		constexpr bool done() const {
			return pStream.done();
		}
	};

	/* specializations for byte streams */
	template <str::IsSource Type>
	struct ByteSource<str::Source<Type>> {
	private:
		str::Source<Type>& pSource;

	public:
		template <class CType>
		constexpr ByteSource(CType&& s) : pSource{ std::forward<CType>(s) } {}

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
			pData = pData.subdata(std::min<size_t>(count, pData.size()));
		}
		constexpr bool done() const {
			return pData.empty();
		}
	};
	template <detail::IsImpl<std::istream> Type>
	struct ByteSource<Type> {
	private:
		detail::IStreamWrapper<char> pStream;

	public:
		template <class CType>
		constexpr ByteSource(CType&& s) : pStream{ std::forward<CType>(s) } {}

	public:
		constexpr str::Data load(size_t count) {
			auto view = pStream.load(count);
			return { reinterpret_cast<const uint8_t*>(view.data()), view.size() };
		}
		constexpr void consume(size_t count) {
			pStream.consume(count);
		}
		constexpr bool done() const {
			return pStream.done();
		}
	};
	template <class Type>
	struct ByteSource<str::LimitSource<Type>> {
	private:
		str::Source<Type>& pSource;
		size_t pCount = 0;

	public:
		template <class CType>
		constexpr ByteSource(CType&& s) : pSource{ s.pSource }, pCount{ s.pCount } {}

	public:
		constexpr str::Data load(size_t count) {
			str::Data data = pSource.load(std::min<size_t>(count, pCount));
			return str::Data{ data.data(), std::min<size_t>(data.size(), pCount) };
		}
		constexpr void consume(size_t count) {
			count = std::min<size_t>(count, pCount);
			pCount -= count;
			pSource.consume(count);
		}
		constexpr bool done() const {
			return (pCount == 0 || pSource.done());
		}
	};
}
