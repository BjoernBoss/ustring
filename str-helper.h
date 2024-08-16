/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"

namespace str {
	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer with a null-byte (if capacity is greater than zero) */
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

	/* [str::IsSink] wrapper to create a sink into a constant buffer or a pointer and make the written size available */
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

	/* [str::IsSink] structure to inherit from which can be used as a sink */
	struct InheritSink {
	public:
		constexpr InheritSink() = default;
		virtual ~InheritSink() = default;

	public:
		virtual void write(const std::u32string_view& s) = 0;
		virtual void write(char32_t chr, size_t count) = 0;
	};

	/* [str::IsWire] structure to inherit from which can be used as a wire */
	struct InheritWire {
	public:
		constexpr InheritWire() = default;
		virtual ~InheritWire() = default;

	public:
		virtual void write(const str::Data& d) = 0;
	};

	/* [str::IsStream] stream-reader to interact with a char-stream
	*	Note: Must not outlive the stream object as it may store a reference to it
	*	Important: Stream-object may build up state around the source-stream, which already extracts more
	*	than requested and therefore source-streams should be passed around as str::Stream-objects */
	template <str::IsStream Type>
	class Stream {
	private:
		using Impl = str::CharStream<std::remove_cvref_t<Type>>;

	public:
		using ChType = typename Impl::ChType;

	private:
		Impl pStream;

	public:
		constexpr Stream(Type& s) : pStream{ s } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		constexpr Stream(Type&& s) : pStream{ s } {}

	public:
		constexpr std::basic_string_view<ChType> load(size_t count) {
			return pStream.load(count);
		}
		constexpr void consume(size_t count = size_t(-1)) {
			pStream.consume(count);
		}
		constexpr bool done() const {
			return pStream.done();
		}
	};

	/* [str::IsSource] source-reader to interact with a byte-source
	*	Note: Must not outlive the source object as it may store a reference to it
	*	Important: Stream-object may build up state around the source, which already extracts more
	*	than requested and therefore sources should be passed around as str::Source-objects */
	template <str::IsSource Type>
	class Source {
	private:
		using Impl = str::ByteSource<std::remove_cvref_t<Type>>;

	private:
		Impl pSource;

	public:
		constexpr Source(Type& s) : pSource{ s } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		constexpr Source(Type&& s) : pSource{ s } {}

	public:
		constexpr str::Data load(size_t count) {
			return pSource.load(count);
		}
		constexpr void consume(size_t count = size_t(-1)) {
			pSource.consume(count);
		}
		constexpr bool done() const {
			return pSource.done();
		}
	};

	/* [str::IsStream] wrapper to create a stream which reads at most count-number of characters from the stream
	*	Note: Must not outlive the stream as it may store a reference to it */
	template <str::IsStream Type>
	class LimitStream {
		friend struct str::CharStream<str::LimitStream<Type>>;
	private:
		str::Stream<Type> pStream;
		size_t pCount = 0;

	public:
		LimitStream(Type& stream, size_t count) : pStream{ stream }, pCount{ count } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		LimitStream(Type&& stream, size_t count) : pStream{ stream }, pCount{ count } {}
	};

	/* [str::IsSource] wrapper to create a source which reads at most count-number of bytes from the source
	*	Note: Must not outlive the source as it may store a reference to it */
	template <str::IsSource Type>
	class LimitSource {
		friend struct str::ByteSource<str::LimitSource<Type>>;
	private:
		str::Source<Type> pSource;
		size_t pCount = 0;

	public:
		LimitSource(Type& source, size_t count) : pSource{ source }, pCount{ count } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		LimitSource(Type&& source, size_t count) : pSource{ source }, pCount{ count } {}
	};

	/* [str::IsStream] wrapper to create a stream which reads the data from the source-stream and transcodes them to the target character-type
	*	Note: Must not outlive the source as it may store a reference to it */
	template <str::IsChar ChType, str::IsStream Type, char32_t CodeError = err::DefChar>
	struct Convert {
		friend struct str::CharStream<str::Convert<ChType, Type, CodeError>>;
	private:
		str::Stream<Type> pStream;

	public:
		Convert(Type& stream) : pStream{ stream } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		Convert(Type&& stream) : pStream{ stream } {}
	};
	template <str::IsStream Type, char32_t CodeError = err::DefChar>
	using ConvertCh = str::Convert<char, Type, CodeError>;
	template <str::IsStream Type, char32_t CodeError = err::DefChar>
	using ConvertWd = str::Convert<wchar_t, Type, CodeError>;
	template <str::IsStream Type, char32_t CodeError = err::DefChar>
	using ConvertU8 = str::Convert<char8_t, Type, CodeError>;
	template <str::IsStream Type, char32_t CodeError = err::DefChar>
	using ConvertU16 = str::Convert<char16_t, Type, CodeError>;
	template <str::IsStream Type, char32_t CodeError = err::DefChar>
	using ConvertU32 = str::Convert<char32_t, Type, CodeError>;

	/* [str::IsStream] wrapper to create a stream which reads the data from the byte-source and passes them to a str::FromWire object
	*	Note: Must not outlive the source as it may store a reference to it */
	template <str::IsSource Type, char32_t CodeError = err::DefChar>
	class WireIn {
		friend struct str::CharStream<str::WireIn<Type, CodeError>>;
	private:
		str::Source<Type> pSource;
		str::WireCoding pCoding;
		str::BOMMode pMode;

	public:
		WireIn(Type& source, str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll) : pSource{ source }, pCoding{ coding }, pMode{ mode } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		WireIn(Type&& source, str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll) : pSource{ source }, pCoding{ coding }, pMode{ mode } {}
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
		WireOut(Type& sink, str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true) : pSink{ sink }, pWire{ coding, addBOM } {}
		template <class _ = std::enable_if_t<!std::is_rvalue_reference_v<Type>>>
		WireOut(Type&& sink, str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true) : pSink{ sink }, pWire{ coding, addBOM } {}

	public:
		constexpr void write(const std::u32string_view& s) {
			pWire.write(pSink, s);
		}
		constexpr void put(char32_t c) {
			pWire.write(pSink, std::u32string_view{ &c, 1 });
		}
	};
}
