/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-bytes.h"
#include "str-chars.h"

namespace str {
	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer with a null-byte (if capacity is greater than zero)
	*	Note: Must not outlive the source data as it stores a reference to it */
	template <str::IsChar ChType>
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
		constexpr void write(const std::basic_string_view<ChType>& s) {
			size_t sz = s.size();
			if (sz > size_t(pEnd - pBegin)) {
				sz = size_t(pEnd - pBegin);
				pOverflow = true;
			}
			std::copy(s.data(), s.data() + sz, pBegin);
			*(pBegin += sz) = 0;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer and make the written size available
	*	Note: Must not outlive the source data as it stores a reference to it */
	template <str::IsChar ChType>
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
		constexpr void write(const std::basic_string_view<ChType>& s) {
			size_t sz = s.size();
			if (sz > pSize - pOffset) {
				sz = pSize - pOffset;
				pOverflow = true;
			}
			std::copy(s.data(), s.data() + sz, pPtr + pOffset);
			pOffset += sz;
		}
		constexpr size_t size() const {
			return pOffset;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsWire] wrapper to create a byte-sink into a constant buffer or a pointer and make the written size available
	*	Note: Must not outlive the source data as it stores a reference to it */
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
		constexpr void write(const str::Data& d) {
			size_t sz = d.size();
			if (sz > pSize - pOffset) {
				sz = pSize - pOffset;
				pOverflow = true;
			}
			std::copy(d.data(), d.data() + sz, pPtr + pOffset);
			pOffset += sz;
		}
		constexpr size_t size() const {
			return pOffset;
		}
		constexpr bool overflow() const {
			return pOverflow;
		}
	};

	/* [str::IsSource] wrapper to create a source which reads at most count-number of bytes from the source
	*	Note: Must not outlive the source as it stores a reference to it */
	template <str::IsSource Type>
	class LimitSource {
	private:
		str::Source<Type> pSource;
		size_t pCount = 0;

	public:
		constexpr LimitSource(Type& source, size_t count) : pSource{ source }, pCount{ count } {}
		constexpr LimitSource(Type&& source, size_t count) : pSource{ std::move(source) }, pCount{ count } {}

	public:
		constexpr size_t read(uint8_t* buffer, size_t size) {
			size = pSource.read(buffer, std::min<size_t>(pCount, size));
			pCount -= size;
			return size;
		}
	};

	/* [str::IsStream] wrapper to create a stream which reads at most count-number of characters from the stream
	*	Note: Must not outlive the stream as it stores a reference to it */
	template <str::IsStream Type>
	class LimitStream {
	public:
		using ChType = str::StreamChar<Type>;

	private:
		str::Stream<Type> pStream;
		size_t pCount = 0;

	public:
		constexpr LimitStream(Type& stream, size_t count) : pStream{ stream }, pCount{ count } {}
		constexpr LimitStream(Type&& stream, size_t count) : pStream{ std::move(stream) }, pCount{ count } {}

	public:
		constexpr size_t read(ChType* buffer, size_t size) {
			size = pStream.read(buffer, std::min<size_t>(pCount, size));
			pCount -= size;
			return size;
		}
	};

	/* [str::IsStream] wrapper to create a stream which reads the data from the byte-source and passes them to a str::FromWire object
	*	Note: Must not outlive the source as it stores a reference to it */
	template <str::IsSource Type, char32_t CodeError = err::DefChar>
	class WireIn {
	private:
		static constexpr size_t BytesPerIteration = 256;

	private:
		str::Source<Type> pSource;
		str::FromWire<CodeError> pWire;
		std::u32string pBuffer;
		std::u32string_view pView;
		bool pClosed = false;

	public:
		constexpr WireIn(Type& source, str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll) : pSource{ source }, pWire{ coding, mode } {}
		constexpr WireIn(Type&& source, str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll) : pSource{ std::move(source) }, pWire{ coding, mode } {}

	private:
		constexpr void fLoad(size_t size) {
			/* check if the offset should be reset */
			size_t offset = 0;
			if (pView.empty())
				pBuffer.clear();
			else {
				offset = (pView.data() - pBuffer.data());
				if (offset >= pView.size()) {
					pBuffer = pBuffer.substr(offset);
					offset = 0;
				}
				else
					size += offset;
			}

			/* iterate until the given number of characters has been produced or until the end has been reached */
			while (pBuffer.size() < size) {
				str::Data data = pSource.load(BytesPerIteration);

				/* feed the data through the wire and consume them from the source */
				pWire.readTo(pBuffer, data);
				pSource.consume(data.size());

				/* check if the end has been reached */
				if (data.size() >= BytesPerIteration)
					continue;
				pWire.lastTo(pBuffer, str::Data{});
				pClosed = true;
				break;
			}
			pView = std::u32string_view{ pBuffer }.substr(offset);
		}

	public:
		constexpr size_t read(char32_t* buffer, size_t size) {
			/* check if the capacity is not yet available */
			if (size > pView.size() && !pClosed)
				fLoad(size);

			/* copy the data out */
			size = std::min<size_t>(size, pView.size());
			std::copy(pView.begin(), pView.begin() + size, buffer);
			pView = pView.substr(size);
			return size;
		}
	};

	/* [str::IsSink] wrapper to create a sink which immediately passes the data to the wire and out to the corresponding wire
	*	Note: Must not outlive target-wire as it stores a reference to it */
	template <str::IsWire Type, char32_t CodeError = err::DefChar>
	class WireOut {
		friend struct CharWriter<str::WireOut<Type, CodeError>>;
	private:
		Type& pSink;
		str::ToWire<CodeError> pWire;

	public:
		constexpr WireOut(Type& sink, str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true) : pSink{ sink }, pWire{ coding, addBOM } {}
		constexpr WireOut(Type&& sink, str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true) : pSink{ sink }, pWire{ coding, addBOM } {}

	public:
		constexpr void write(std::u32string_view s) {
			pWire.write(pSink, s);
		}
		constexpr void put(char32_t c) {
			pWire.write(pSink, std::u32string_view{ &c, 1 });
		}
	};

	/* [str::IsStream] wrapper to create a stream which reads the data from the source-stream and transcodes them to a stream of codepoints
	*	Note: Must not outlive the source as it stores a reference to it */
	template <str::IsStream Type, char32_t CodeError = err::DefChar>
	struct U32Stream {
	public:
		using ChType = char32_t;

	private:
		using SChType = str::StreamChar<Type>;
		static constexpr size_t CharsPerIteration = 64;

	private:
		str::Stream<Type> pStream;
		std::u32string pBuffer;
		std::u32string_view pView;
		bool pClosed = false;

	public:
		constexpr U32Stream(Type& stream) : pStream{ stream } {}
		constexpr U32Stream(Type&& stream) : pStream{ std::move(stream) } {}

	private:
		constexpr void fLoad(size_t size) {
			size = std::max<size_t>(size, CharsPerIteration + pView.size());

			/* check if the offset should be reset */
			size_t offset = 0;
			if (pView.empty())
				pBuffer.clear();
			else {
				offset = (pView.data() - pBuffer.data());
				if (offset >= pView.size()) {
					pBuffer = pBuffer.substr(offset);
					offset = 0;
				}
				else
					size += offset;
			}

			/* iterate until the given number of characters has been produced or until the end has been reached */
			while (pBuffer.size() < size) {
				auto [cp, consumed] = str::PartialCodepoint<CodeError>(pStream.load(str::MaxEncSize<SChType>));

				/* check if the end has been reached and trigger a final transcoding to ensure proper error-handling on incomplete characters) */
				if (consumed == 0) {
					auto [cp, consumed] = str::GetCodepoint<CodeError>(pStream.load(str::MaxEncSize<SChType>));
					pStream.consume(consumed);
					pBuffer.push_back(cp);
					pClosed = true;
					break;
				}

				/* write the actual codepoint out to the buffer and consume it */
				pStream.consume(consumed);
				pBuffer.push_back(cp);
			}
			pView = std::u32string_view{ pBuffer }.substr(offset);
		}

	public:
		constexpr size_t read(char32_t* buffer, size_t size) {
			/* check if the capacity is not yet available */
			if (size > pView.size() && !pClosed)
				fLoad(size);

			/* copy the data out */
			size = std::min<size_t>(size, pView.size());
			std::copy(pView.begin(), pView.begin() + size, buffer);
			pView = pView.substr(size);
			return size;
		}
	};

	/* [str::IsWire] structure to inherit from which can be used as a wire */
	struct InheritWire {
	public:
		constexpr InheritWire() = default;
		virtual ~InheritWire() = default;

	public:
		virtual void write(const str::Data& d) = 0;
	};

	/* wrapper type to create str::InheritWire from any wire-type
	*	Note: cannot be directly used, as only str::InheritWire implements the wire-specialization
	*	Note: Must not outlive the wire as it stores a reference to it */
	template <str::IsWire Type>
	struct WireImplementation final : public str::InheritWire {
	private:
		Type& pWire;

	public:
		constexpr WireImplementation(Type& wire) : pWire{ wire } {}
		constexpr WireImplementation(Type&& wire) : pWire{ wire } {}

	public:
		void write(const str::Data& d) override {
			str::CallWire(pWire, d);
		}
	};

	/* [str::IsSink] structure to inherit from which can be used as a sink */
	struct InheritSink {
	public:
		constexpr InheritSink() = default;
		virtual ~InheritSink() = default;

	public:
		virtual void write(std::u32string_view s) = 0;
		virtual void write(char32_t chr, size_t count) = 0;
	};

	/* wrapper type to create str::InheritSink from any sink-type
	*	Note: cannot be directly used, as only str::InheritSink implements the sink-specialization
	*	Note: Must not outlive the sink as it stores a reference to it */
	template <str::IsSink Type>
	struct SinkImplementation final : public str::InheritSink {
	private:
		Type& pSink;

	public:
		constexpr SinkImplementation(Type& sink) : pSink{ sink } {}
		constexpr SinkImplementation(Type&& sink) : pSink{ sink } {}

	public:
		void write(char32_t chr, size_t count) override {
			str::CodepointTo<str::err::DefChar>(pSink, chr, count);
		}
		void write(std::u32string_view s) override {
			str::TranscodeAllTo<str::err::DefChar>(pSink, s);
		}
	};

	/* [str::IsSource] structure to inherit from which can be used as a source */
	struct InheritSource {
	public:
		constexpr InheritSource() = default;
		virtual ~InheritSource() = default;

	public:
		virtual size_t read(uint8_t* buffer, size_t size) = 0;
	};

	/* wrapper type to create str::InheritSource from any source-type
	*	Note: cannot be directly used, as only str::InheritSource implements the source-specialization
	*	Note: Must not outlive the wire as it stores a reference to it */
	template <str::IsSource Type>
	struct SourceImplementation final : public str::InheritSource {
	private:
		str::Source<Type> pSource;

	public:
		constexpr SourceImplementation(Type& source) : pSource{ source } {}
		constexpr SourceImplementation(Type&& source) : pSource{ std::move(source) } {}

	public:
		size_t read(uint8_t* buffer, size_t size) override {
			return pSource.read(buffer, size);
		}
	};

	/* [str::IsStream] structure to inherit from which can be used as a stream */
	struct InheritStream {
	public:
		constexpr InheritStream() = default;
		virtual ~InheritStream() = default;

	public:
		virtual size_t read(char32_t* buffer, size_t size) = 0;
	};

	/* wrapper type to create str::InheritStream from any stream-type
	*	Note: cannot be directly used, as only str::InheritStream implements the stream-specialization
	*	Note: Must not outlive the stream as it stores a reference to it */
	template <str::IsStream Type>
	struct StreamImplementation final : public str::InheritStream {
	private:
		str::U32Stream<Type, str::err::DefChar> pStream;

	public:
		StreamImplementation(Type& stream) : pStream{ stream } {}
		StreamImplementation(Type&& stream) : pStream{ std::move(stream) } {}

	public:
		size_t read(char32_t* buffer, size_t size) override {
			return str::CallCharLoader(pStream, buffer, size);
		}
	};
}
