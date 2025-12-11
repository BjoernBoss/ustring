/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2025 Bjoern Boss Henrichsen */
#pragma once

#include "common/str-common.h"
#include "common/str-bytes.h"
#include "common/str-chars.h"
#include "format/str-wire.h"

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
	*	Note: For rvalues, a local move-constructed value of the source is held, otherwise a reference is held and it must not outlive the source */
	template <str::IsSource Type>
	class LimitSource {
	private:
		str::Source<Type> pSource;
		size_t pCount = 0;

	public:
		constexpr LimitSource(Type&& source, size_t count) : pSource{ std::forward<Type>(source) }, pCount{ count } {}

	public:
		constexpr size_t read(uint8_t* buffer, size_t size) {
			size = pSource.read(buffer, std::min<size_t>(pCount, size));
			pCount -= size;
			return size;
		}
	};
	template <class Type> LimitSource(Type&, size_t) -> LimitSource<Type&>;
	template <class Type> LimitSource(Type&&, size_t) -> LimitSource<Type>;

	/* [str::IsStream] wrapper to create a stream which reads at most count-number of characters from the stream
	*	Note: For rvalues, a local move-constructed value of the stream is held, otherwise a reference is held and it must not outlive the stream */
	template <str::IsStream Type>
	class LimitStream {
	public:
		using ChType = str::StreamChar<Type>;

	private:
		str::Stream<Type> pStream;
		size_t pCount = 0;

	public:
		constexpr LimitStream(Type&& stream, size_t count) : pStream{ std::forward<Type>(stream) }, pCount{ count } {}

	public:
		constexpr size_t read(ChType* buffer, size_t size) {
			size = pStream.read(buffer, std::min<size_t>(pCount, size));
			pCount -= size;
			return size;
		}
	};
	template <class Type> LimitStream(Type&, size_t) -> LimitStream<Type&>;
	template <class Type> LimitStream(Type&&, size_t) -> LimitStream<Type>;

	/* [str::IsStream] wrapper to create a stream which reads the data from the byte-source and passes them to a str::FromWire object
	*	Note: For rvalues, a local move-constructed value of the source is held, otherwise a reference is held and it must not outlive the source */
	template <str::IsSource Type, str::CodeError Error = str::CodeError::replace>
	class WireIn {
	private:
		static constexpr size_t BytesPerIteration = 256;

	private:
		str::Source<Type> pSource;
		str::FromWire<Error> pWire;
		std::u32string pBuffer;
		std::u32string_view pView;
		bool pClosed = false;

	public:
		constexpr WireIn(Type&& source, str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll) : pSource{ std::forward<Type>(source) }, pWire{ coding, mode } {}

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
	template <class Type> WireIn(Type&) -> WireIn<Type&>;
	template <class Type> WireIn(Type&&) -> WireIn<Type>;
	template <class Type> WireIn(Type&, str::WireCoding) -> WireIn<Type&>;
	template <class Type> WireIn(Type&&, str::WireCoding) -> WireIn<Type>;
	template <class Type> WireIn(Type&, str::WireCoding, str::BOMMode) -> WireIn<Type&>;
	template <class Type> WireIn(Type&&, str::WireCoding, str::BOMMode) -> WireIn<Type>;

	/* [str::IsSink] wrapper to create a sink which immediately passes the data to the wire and out to the corresponding wire
	*	Note: For rvalues, a local move-constructed value of the wire is held, otherwise a reference is held and it must not outlive the wire */
	template <str::IsWire Type, str::CodeError Error = str::CodeError::replace>
	class WireOut {
		friend struct CharWriter<str::WireOut<Type, Error>>;
	private:
		Type pSink;
		str::ToWire<Error> pWire;

	public:
		constexpr WireOut(Type&& wire, str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true) : pSink{ std::forward<Type>(wire) }, pWire{ coding, addBOM } {}

	public:
		constexpr void write(std::u32string_view s) {
			pWire.write(pSink, s);
		}
		constexpr void put(char32_t c) {
			pWire.write(pSink, std::u32string_view{ &c, 1 });
		}
	};
	template <class Type> WireOut(Type&) -> WireOut<Type&>;
	template <class Type> WireOut(Type&&) -> WireOut<Type>;
	template <class Type> WireOut(Type&, str::WireCoding) -> WireOut<Type&>;
	template <class Type> WireOut(Type&&, str::WireCoding) -> WireOut<Type>;
	template <class Type> WireOut(Type&, str::WireCoding, bool) -> WireOut<Type&>;
	template <class Type> WireOut(Type&&, str::WireCoding, bool) -> WireOut<Type>;

	/* [str::IsStream] wrapper to create a stream which reads the data from the source-stream and transcodes them to a stream of codepoints
	*	Note: For rvalues, a local move-constructed value of the stream is held, otherwise a reference is held and it must not outlive the stream */
	template <str::IsStream Type, str::CodeError Error = str::CodeError::replace>
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
		constexpr U32Stream(Type&& stream) : pStream{ std::forward<Type>(stream) } {}

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
				auto [cp, consumed] = str::PartialCodepoint<Error>(pStream.load(str::MaxEncSize<SChType>));

				/* check if the end has been reached and trigger a final transcoding to ensure proper error-handling on incomplete characters) */
				if (consumed == 0) {
					auto [cp, consumed] = str::GetCodepoint<Error>(pStream.load(str::MaxEncSize<SChType>));
					pStream.consume(consumed);
					if (consumed > 0)
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
	*	Note: For rvalues, a local move-constructed value of the wire is held, otherwise a reference is held and it must not outlive the wire */
	template <str::IsWire Type>
	struct WireImplementation final : public str::InheritWire {
	private:
		Type pWire;

	public:
		constexpr WireImplementation(Type&& wire) : pWire{ std::forward<Type>(wire) } {}

	public:
		void write(const str::Data& d) override {
			str::CallWire(pWire, d);
		}
	};
	template <class Type> WireImplementation(Type&) -> WireImplementation<Type&>;
	template <class Type> WireImplementation(Type&&) -> WireImplementation<Type>;

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
	*	Note: For rvalues, a local move-constructed value of the sink is held, otherwise a reference is held and it must not outlive the sink */
	template <str::IsSink Type>
	struct SinkImplementation final : public str::InheritSink {
	private:
		Type pSink;

	public:
		constexpr SinkImplementation(Type&& sink) : pSink{ std::forward<Type>(sink) } {}

	public:
		void write(char32_t chr, size_t count) override {
			str::CodepointTo<str::CodeError::replace>(pSink, chr, count);
		}
		void write(std::u32string_view s) override {
			str::FastcodeAllTo<str::CodeError::replace>(pSink, s);
		}
	};
	template <class Type> SinkImplementation(Type&) -> SinkImplementation<Type&>;
	template <class Type> SinkImplementation(Type&&) -> SinkImplementation<Type>;

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
	*	Note: For rvalues, a local move-constructed value of the source is held, otherwise a reference is held and it must not outlive the source */
	template <str::IsSource Type>
	struct SourceImplementation final : public str::InheritSource {
	private:
		str::Source<Type> pSource;

	public:
		constexpr SourceImplementation(Type&& source) : pSource{ std::forward<Type>(source) } {}

	public:
		size_t read(uint8_t* buffer, size_t size) override {
			return pSource.read(buffer, size);
		}
	};
	template <class Type> SourceImplementation(Type&) -> SourceImplementation<Type&>;
	template <class Type> SourceImplementation(Type&&) -> SourceImplementation<Type>;

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
	*	Note: For rvalues, a local move-constructed value of the stream is held, otherwise a reference is held and it must not outlive the stream */
	template <str::IsStream Type>
	struct StreamImplementation final : public str::InheritStream {
	private:
		str::U32Stream<Type, str::CodeError::replace> pStream;

	public:
		StreamImplementation(Type&& stream) : pStream{ std::forward<Type>(stream) } {}

	public:
		size_t read(char32_t* buffer, size_t size) override {
			return str::CallCharLoader(pStream, buffer, size);
		}
	};
	template <class Type> StreamImplementation(Type&) -> StreamImplementation<Type&>;
	template <class Type> StreamImplementation(Type&&) -> StreamImplementation<Type>;

	/* [str::IsSink] struct to buffer the data before writing them out to the sink
	*	Note: For rvalues, a local move-constructed value of the sink is held, otherwise a reference is held and it must not outlive the sink */
	template <str::IsSink Type>
	struct BufferSink {
	public:
		using ChType = str::SinkChar<Type>;

	private:
		std::basic_string<ChType> pBuffer;
		Type pSink;
		size_t pContent = 0;

	public:
		constexpr BufferSink(Type&& sink, size_t bufferSize) : pSink{ std::forward<Type>(sink) } {
			pBuffer.resize(bufferSize);
		}
		~BufferSink() {
			if (pContent > 0)
				fFlush();
		}

	private:
		void fFlush() {
			str::CallSink(pSink, std::basic_string_view<ChType>{ pBuffer }.substr(0, pContent));
			pContent = 0;
		}

	public:
		constexpr void append(size_t count, ChType chr) {
			while (count > 0) {
				/* write the data to the buffer */
				size_t _count = std::min<size_t>(count, pBuffer.size() - pContent);
				std::fill_n(pBuffer.begin() + pContent, _count, chr);
				pContent += _count;
				count -= _count;

				/* check if the buffer needs to be flushed */
				if (pContent >= pBuffer.size())
					fFlush();
			}
		}
		constexpr void append(std::basic_string_view<ChType> s) {
			while (!s.empty()) {
				/* write the data to the buffer */
				size_t _count = std::min<size_t>(s.size(), pBuffer.size() - pContent);
				std::copy(s.begin(), s.begin() + _count, pBuffer.begin() + pContent);
				pContent += _count;
				s = s.substr(_count);

				/* check if the buffer needs to be flushed */
				if (pContent >= pBuffer.size())
					fFlush();
			}
		}
	};
	template <class Type> BufferSink(Type&, size_t) -> BufferSink<Type&>;
	template <class Type> BufferSink(Type&&, size_t) -> BufferSink<Type>;

	/* [str::IsWire] struct to buffer the data before writing them out to the wire
	*	Note: For rvalues, a local move-constructed value of the wire is held, otherwise a reference is held and it must not outlive the wire */
	template <str::IsWire Type>
	struct BufferWire {
	private:
		std::vector<uint8_t> pBuffer;
		Type pWire;
		size_t pContent = 0;

	public:
		constexpr BufferWire(Type&& wire, size_t bufferSize) : pWire{ std::forward<Type>(wire) } {
			pBuffer.resize(bufferSize);
		}
		~BufferWire() {
			if (pContent > 0)
				fFlush();
		}

	private:
		void fFlush() {
			str::CallWire(pWire, str::Data{ pBuffer }.subspan(0, pContent));
			pContent = 0;
		}

	public:
		constexpr void write(str::Data d) {
			while (!d.empty()) {
				/* write the data to the buffer */
				size_t _count = std::min<size_t>(d.size(), pBuffer.size() - pContent);
				std::copy(d.begin(), d.begin() + _count, pBuffer.begin() + pContent);
				pContent += _count;
				d = d.subspan(_count);

				/* check if the buffer needs to be flushed */
				if (pContent >= pBuffer.size())
					fFlush();
			}
		}
	};
	template <class Type> BufferWire(Type&, size_t) -> BufferWire<Type&>;
	template <class Type> BufferWire(Type&&, size_t) -> BufferWire<Type>;
}
