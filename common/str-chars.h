/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"

namespace str {
	namespace detail {
		template <class Type> struct StrType { using type = void; };
		template <std::convertible_to<std::string_view> Type> struct StrType<Type> { using type = char; };
		template <std::convertible_to<std::wstring_view> Type> struct StrType<Type> { using type = wchar_t; };
		template <std::convertible_to<std::u8string_view> Type> struct StrType<Type> { using type = char8_t; };
		template <std::convertible_to<std::u16string_view> Type> struct StrType<Type> { using type = char16_t; };
		template <std::convertible_to<std::u32string_view> Type> struct StrType<Type> { using type = char32_t; };
	}

	/* character writable interface which requires:
	*	operator() to take the writable object and a character and a count (can be zero)
	*	operator() to take the writable object and a pointer and a size
	*	ChType: the character type of the writer */
	template <class Type>
	struct CharWriter;

	/* specialize to make type available as character source for a stream which requires:
	*	typename ChType = ... (character type)
	*	operator(Type&, ChType*, size_t) -> size_t (must first return less than capacity, when end of source is reached) */
	template <class Type>
	struct CharLoader;

	/* type is anything convertible to a string-view */
	template <class Type>
	concept IsStr = !std::is_void_v<typename detail::StrType<Type>::type>;

	/* type is anything convertible to a string-view of the specific type */
	template <class Type, class ChType>
	concept IsChStr = std::convertible_to<Type, std::basic_string_view<ChType>>;

	/* type is anything that implements the str::CharWriter interface
	*	operator(ChType, size_t) to write the character 0 to n times
	*	operator(const std::basic_string_view<ChType>&) to write the corresponding string */
	template <class Type>
	concept IsSink = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, size_t sz) {
		typename str::CharWriter<std::remove_cvref_t<Type>>::ChType;
		str::CharWriter<std::remove_cvref_t<Type>>{}(t, std::declval<typename str::CharWriter<std::remove_cvref_t<Type>>::ChType>(), sz);
		str::CharWriter<std::remove_cvref_t<Type>>{}(t, std::declval<const std::basic_string_view<typename str::CharWriter<std::remove_cvref_t<Type>>::ChType>&>());
	};

	/* type is anything that implements the str::CharLoader interface */
	template <class Type>
	concept IsCharLoader = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, size_t c) {
		typename str::CharLoader<std::remove_cvref_t<Type>>::ChType;
		{ str::CharLoader<std::remove_cvref_t<Type>>{}(t, std::declval<typename str::CharLoader<std::remove_cvref_t<Type>>::ChType*>(), c) } -> std::same_as<size_t>;
	};

	/* type is anything that can be wrapped by an str::Stream */
	template <class Type>
	concept IsStream = str::IsStr<Type> || str::IsCharLoader<Type>;

	/* extract the character type of the type, which satisfies str::IsSink */
	template <str::IsSink Type>
	using SinkChar = typename str::CharWriter<std::remove_cvref_t<Type>>::ChType;

	/* extract the character type of the type, which satisfies str::IsStr */
	template <class Type>
	using StringChar = typename detail::StrType<Type>::type;

	/* extract the character type of the type, which satisfies str::IsCharLoader */
	template <str::IsCharLoader Type>
	using CharLoaderChar = typename str::CharLoader<std::remove_cvref_t<Type>>::ChType;

	namespace detail {
		template <class Type> struct StreamType { using type = void; };
		template <str::IsStr Type> struct StreamType<Type> { using type = str::StringChar<Type>; };
		template <str::IsCharLoader Type> struct StreamType<Type> { using type = str::CharLoaderChar<Type>; };
	}

	/* extract the character type of the type, which satisfies str::IsStream */
	template <str::IsStream Type>
	using StreamChar = typename detail::StreamType<Type>::type;

	/* wrappers to interact with character-sinks */
	template <str::IsSink SinkType>
	constexpr void CallSink(SinkType&& sink, str::SinkChar<SinkType> chr, size_t count = 1) {
		str::CharWriter<std::remove_cvref_t<SinkType>>{}(sink, chr, count);
	}
	template <str::IsSink SinkType>
	constexpr void CallSink(SinkType&& sink, const std::basic_string_view<str::SinkChar<SinkType>>& str) {
		str::CharWriter<std::remove_cvref_t<SinkType>>{}(sink, str);
	}

	/* wrapper to write to load chars from a char-loader */
	template <str::IsCharLoader LoaderType>
	constexpr size_t CallCharLoader(LoaderType&& stream, str::CharLoaderChar<LoaderType>* buffer, size_t size) {
		return str::CharLoader<std::remove_cvref_t<LoaderType>>{}(stream, buffer, size);
	}

	namespace detail {
		template <class Type, class ChType>
		class StreamConst {
		private:
			std::basic_string_view<ChType> pView;

		public:
			constexpr StreamConst(const Type& s) : pView{ s } {}

		public:
			constexpr std::basic_string_view<ChType> load(size_t size) {
				return pView;
			}
			constexpr void consume(size_t size) {
				pView = pView.substr(std::min<size_t>(pView.size(), size));
			}
			constexpr bool done() const {
				return pView.empty();
			}
		};
		template <class Type, class ChType, size_t MinLoadCapacity>
		class StreamLoad {
		private:
			std::basic_string<ChType> pBuffer;
			Type& pStream;
			std::basic_string_view<ChType> pView;
			bool pClosed = false;

		public:
			/* perform initial load to ensure closed-flag is set */
			constexpr StreamLoad(Type& s) : pStream{ s } {
				fLoad(0);
			}
			constexpr StreamLoad(Type&& s) : pStream{ s } {
				fLoad(0);
			}

		private:
			constexpr void fLoad(size_t size) {
				size = std::max<size_t>(size, MinLoadCapacity);

				/* check if the cache should be reset and reset it to have enough capacity */
				size_t offset = (pView.empty() ? 0 : pView.data() - pBuffer.data());
				if (offset >= pView.size()) {
					std::memmove(pBuffer.data(), pView.data(), pView.size() * sizeof(ChType));
					offset = 0;
				}
				size_t endOfData = offset + pView.size();
				pBuffer.resize(endOfData + size);

				/* setup the data-object and read the actual data from the stream */
				size_t loaded = str::CallCharLoader(pStream, pBuffer.data() + endOfData, size);
				pView = { pBuffer.data() + offset, pView.size() + loaded };
				pClosed = (loaded < size);
			}

		public:
			constexpr std::basic_string_view<ChType> load(size_t size) {
				if (size > pView.size() && !pClosed)
					fLoad(size - pView.size());
				return pView;
			}
			constexpr void consume(size_t size) {
				pView = pView.substr(std::min<size_t>(pView.size(), size));
			}
			constexpr bool done() const {
				return (pView.empty() && pClosed);
			}
		};
	}

	/* [str::IsStream] stream-reader to interact with a char-stream
	*	Note: Must not outlive the stream object as it may store a reference to it
	*	Important: Stream-object may build up state around the source-stream, which already extracts more
	*	than requested and therefore source-streams should be passed around as str::Stream-objects
	*	load(size_t i): load at least [i] characters, or any remaining, if [i] is larger than the stream, and return a reference to them
	*	read(ChType*, size_t i): read [i] characters, or the remining, if [i] is larger than the stream, into the buffer and consume them and return the number of characters
	*	consume(size_t i): remove the leading [i] characters from the stream (if [i] is greater or equal to loaded size, consume everything)
	*	done(): check if the stream contains at least one character */
	template <str::IsStream Type, size_t MinLoadCapacity = 256>
	class Stream {
	public:
		using ChType = str::StreamChar<Type>;

	private:
		using Impl = std::conditional_t<str::IsStr<Type>, detail::StreamConst<Type, ChType>, detail::StreamLoad<Type, ChType, MinLoadCapacity>>;

	private:
		Impl pImpl;

	public:
		constexpr Stream(Type& s) : pImpl{ s } {}
		constexpr Stream(Type&& s) : pImpl{ std::move(s) } {}

	public:
		constexpr std::basic_string_view<ChType> load(size_t size) {
			return pImpl.load(size);
		}
		constexpr size_t read(ChType* buffer, size_t size) {
			std::basic_string_view<ChType> data = pImpl.load(size);
			size = std::min<size_t>(data.size(), size);

			std::copy(data.begin(), data.begin() + size, buffer);
			pImpl.consume(size);
			return size;
		}
		constexpr void consume(size_t size = size_t(-1)) {
			pImpl.consume(size);
		}
		constexpr bool done() const {
			return pImpl.done();
		}
	};
}
