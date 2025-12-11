/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2025 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"

namespace str {
	/* data reader interface requires:
	*	operator(const Type&) -> str::Data */
	template <class Type>
	struct ByteReader;

	/* byte sink interface which requires:
	*	operator(Type&, const str::Data&) -> void */
	template <class Type>
	struct ByteWriter;

	/* specialize to make type available as byte source for a stream which requires:
	*	operator(Type&, uint8_t*, size_t) -> size_t (must first return less than capacity, when end of source is reached) */
	template <class Type>
	struct ByteLoader;

	/* type is anything that implements the str::ByteReader interface */
	template <class Type>
	concept IsData = requires(const Type & t) {
		{ str::ByteReader<std::remove_cvref_t<Type>>{}(t) } -> std::convertible_to<str::Data>;
	};

	/* type is anything that implements the str::ByteWriter interface */
	template <class Type>
	concept IsWire = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, const str::Data & d) {
		str::ByteWriter<std::remove_cvref_t<Type>>{}(t, d);
	};

	/* type is anything that implements the str::ByteLoader interface */
	template <class Type>
	concept IsByteLoader = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, uint8_t * b, size_t c) {
			{ str::ByteLoader<std::remove_cvref_t<Type>>{}(t, b, c) } -> std::same_as<size_t>;
	};

	/* type is anything that can be wrapped by an str::Source */
	template <class Type>
	concept IsSource = str::IsData<Type> || str::IsByteLoader<Type>;

	/* wrapper to fetch the data from a byte-reader-type */
	constexpr str::Data CallData(const str::IsData auto& data) {
		return str::ByteReader<std::remove_cvref_t<decltype(data)>>{}(data);
	}

	/* wrapper to write to a byte-wire */
	constexpr void CallWire(str::IsWire auto&& wire, const str::Data& data) {
		str::ByteWriter<std::remove_cvref_t<decltype(wire)>>{}(wire, data);
	}

	/* wrapper to write to load data from a byte-source */
	constexpr size_t CallByteLoader(str::IsByteLoader auto&& source, uint8_t* buffer, size_t size) {
		return str::ByteLoader<std::remove_cvref_t<decltype(source)>>{}(source, buffer, size);
	}

	namespace detail {
		template <class Type>
		class SourceConst {
		private:
			str::Data pData;

		public:
			constexpr SourceConst(const Type& d) : pData{ str::CallData(d) } {}

		public:
			constexpr str::Data load(size_t size) {
				return pData;
			}
			constexpr void consume(size_t size) {
				pData = pData.subspan(std::min<size_t>(pData.size(), size));
			}
			constexpr bool done() const {
				return pData.empty();
			}
		};
		template <class Type, size_t MinLoadCapacity>
		class SourceLoad {
		private:
			std::vector<uint8_t> pBuffer;
			str::Data pData;
			Type pSource;
			bool pClosed = false;

		public:
			constexpr SourceLoad(Type&& d) : pSource{ std::forward<Type>(d) } {}

		private:
			constexpr void fLoad(size_t size) {
				size = std::max<size_t>(size, MinLoadCapacity);

				/* check if the cache should be reset and reset it to have enough capacity */
				size_t offset = (pData.empty() ? 0 : pData.data() - pBuffer.data());
				if (offset >= pData.size()) {
					std::move(pData.begin(), pData.end(), pBuffer.begin());
					offset = 0;
				}
				size_t endOfData = offset + pData.size();
				pBuffer.resize(endOfData + size);

				/* setup the data-object and read the actual data from the stream */
				size_t loaded = str::CallByteLoader(pSource, pBuffer.data() + endOfData, size);
				pData = { pBuffer.data() + offset, pData.size() + loaded };
				pClosed = (loaded < size);
			}

		public:
			constexpr str::Data load(size_t size) {
				if (size > pData.size() && !pClosed)
					fLoad(size - pData.size());
				return pData;
			}
			constexpr void consume(size_t size) {
				pData = pData.subspan(std::min<size_t>(pData.size(), size));
			}
			constexpr bool done() {
				/* check if the view is currently empty, in which case there might still
				*	be more data available - fetch them to get an accurate close-state */
				if (pData.empty() && !pClosed)
					fLoad(0);
				return (pData.empty() && pClosed);
			}
		};
	}

	/* [str::IsSource] source-reader to interact with a byte-source
	*	Note: For rvalues, a local move-constructed value of the source is held, otherwise a reference is held and it must not outlive the source
	*	Important: Source-object may build up state around the source, which already extracts more
	*	than requested and therefore sources should be passed around as str::Source-objects
	*	load(size_t i): load at least [i] bytes, or any remaining, if [i] is larger than the source, and return a reference to them
	*	read(uint8_t*, size_t i): read [i] bytes, or the remining, if [i] is larger than the source, into the buffer and consume them and return the number of bytes
	*	consume(size_t i): remove the leading [i] bytes from the source (if [i] is greater or equal to loaded size, consume everything)
	*	done(): check if the source contains at least one byte */
	template <str::IsSource Type, size_t MinLoadCapacity = 256>
	class Source {
	private:
		using Impl = std::conditional_t<str::IsData<Type>, detail::SourceConst<Type>, detail::SourceLoad<Type, MinLoadCapacity>>;

	private:
		Impl pImpl;

	public:
		constexpr Source(Type&& s) : pImpl{ std::forward<Type>(s) } {}

	public:
		constexpr str::Data load(size_t size) {
			return pImpl.load(size);
		}
		constexpr size_t read(uint8_t* buffer, size_t size) {
			str::Data data = pImpl.load(size);
			size = std::min<size_t>(data.size(), size);

			std::copy(data.begin(), data.begin() + size, buffer);
			pImpl.consume(size);
			return size;
		}
		constexpr void consume(size_t size = std::numeric_limits<size_t>::max()) {
			pImpl.consume(size);
		}
		constexpr bool done() {
			return pImpl.done();
		}
	};
	template <class Type> Source(Type&) -> Source<Type&>;
	template <class Type> Source(Type&&) -> Source<Type>;
}
