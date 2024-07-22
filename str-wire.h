/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-coding.h"
#include "str-escape.h"

namespace str {
	enum class WireCoding : uint8_t {
		utf8,
		ascii,
		utf16le,
		utf16be,
		utf32le,
		utf32be
	};

	/*
	*	none: decode using the given wire-encoding
	*	optional: if a BOM exists for the current wire-encoding, remove the BOM
	*	detectAll: if a BOM is found to match any wire-encoding, use the given wire-encoding instead and remove the BOM, otherwise use the given wire-encoding
	*	detectNoUtf32le: same as detectAll but exclude utf32le, as utf16le is a subset of it, and hence utf32le might be detected for utf16le, which starts with a null-byte
	*/
	enum class BOMMode : uint8_t {
		none,
		optional,
		detectAll,
		detectNoUtf32le
	};

	namespace detail {
		static constexpr char32_t BOMCodePoint = 0xfeff;
		static constexpr size_t AsciiBOMSize = 8;
		static constexpr size_t Utf8BOMSize = 3;
		static constexpr size_t Utf16BOMSize = 2;
		static constexpr size_t Utf32BOMSize = 4;
		static constexpr size_t MaxBOMSize = std::max<size_t>({ detail::AsciiBOMSize, detail::Utf8BOMSize, detail::Utf16BOMSize, detail::Utf32BOMSize });

		static constexpr size_t MaxCodingSize = std::max<size_t>({ detail::MaxBOMSize, str::MaxEscapeSize, str::MaxEncBytes<char>,
			str::MaxEncBytes<wchar_t>, str::MaxEncBytes<char8_t>, str::MaxEncBytes<char16_t>, str::MaxEncBytes<char32_t> });

		static constexpr uint8_t AsciiBOM[detail::AsciiBOMSize] = { u8'\\', u8'u', u8'{', u8'f', u8'e', u8'f', u8'f', u8'}' };
		static constexpr uint8_t Utf8BOM[detail::Utf8BOMSize] = { 0xef, 0xbb, 0xbf };
		static constexpr uint8_t Utf16leBOM[detail::Utf16BOMSize] = { 0xff, 0xfe };
		static constexpr uint8_t Utf16beBOM[detail::Utf16BOMSize] = { 0xfe, 0xff };
		static constexpr uint8_t Utf32leBOM[detail::Utf32BOMSize] = { 0xff, 0xfe, 0x00, 0x00 };
		static constexpr uint8_t Utf32beBOM[detail::Utf32BOMSize] = { 0x00, 0x00, 0xfe, 0xff };
	}

	/* read a string from raw bytes and dynamically detect the encoding based on a BOM
	*	or use the given encoding (will at all times consume all passed in bytes) */
	template <char32_t CodeError = err::DefChar>
	class FromWire {
		static constexpr size_t TranscodeBufCapacity = std::max<size_t>(1024, detail::MaxCodingSize);
	private:
		uint8_t pBuffer[detail::MaxCodingSize] = { 0 };
		size_t pBufSize = 0;
		str::WireCoding pCoding = str::WireCoding::utf8;
		str::BOMMode pMode = str::BOMMode::none;
		bool pSourceCompleted = false;

	public:
		constexpr FromWire(str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll) {
			pCoding = coding;
			pMode = mode;
		}

	private:
		constexpr size_t fDetectBOM(const uint8_t* begin, const uint8_t* end, bool sourceComplete) {
			/* write the data to the buffer and update the size */
			size_t count = std::min<size_t>(detail::MaxBOMSize - pBufSize, size_t(end - begin));
			std::copy(begin, begin + count, pBuffer);
			pBufSize += count;

			/* check what encoding it might be (check utf32 before utf16 as utf16-BOM is a subset of utf32-BOM) */
			size_t bomSize = 0;
			str::WireCoding detected = str::WireCoding::ascii;
			if (pBufSize >= (bomSize = detail::Utf8BOMSize) && std::equal(pBuffer, pBuffer + bomSize, detail::Utf8BOM))
				detected = str::WireCoding::utf8;
			else if (pBufSize >= (bomSize = detail::AsciiBOMSize) && std::equal(pBuffer, pBuffer + bomSize, detail::AsciiBOM))
				detected = str::WireCoding::ascii;
			else if (pBufSize >= (bomSize = detail::Utf32BOMSize) && std::equal(pBuffer, pBuffer + bomSize, detail::Utf32leBOM))
				detected = str::WireCoding::utf32le;
			else if (pBufSize >= (bomSize = detail::Utf32BOMSize) && std::equal(pBuffer, pBuffer + bomSize, detail::Utf32beBOM))
				detected = str::WireCoding::utf32be;
			else if (pBufSize >= (bomSize = detail::Utf16BOMSize) && std::equal(pBuffer, pBuffer + bomSize, detail::Utf16leBOM))
				detected = str::WireCoding::utf16le;
			else if (pBufSize >= (bomSize = detail::Utf16BOMSize) && std::equal(pBuffer, pBuffer + bomSize, detail::Utf16beBOM))
				detected = str::WireCoding::utf16be;

			/* check if the BOM can certainly not be detected anymore (i.e. source is done or buffer already contains more than BOM-size)
			*	in which case the default encoding can just be kept, and otherwise defer until more data are available */
			else {
				if (pBufSize >= detail::MaxBOMSize || sourceComplete)
					pMode = str::BOMMode::none;
				return count;
			}

			/* check if only a matching encoding may consume the BOM */
			if (pMode == str::BOMMode::optional && detected != pCoding) {
				pMode = str::BOMMode::none;
				return count;
			}

			/* check if utf16-le was detected, which is a subset of utf32-le, in which case utf32-le might still become possible */
			if (detected == str::WireCoding::utf16le && pBufSize < detail::Utf32BOMSize && !sourceComplete && pMode != str::BOMMode::detectNoUtf32le)
				return count;

			/* check if utf32-le was detected, which might be excluded in favor of utf16-le */
			if (detected == str::WireCoding::utf32le && pMode == str::BOMMode::detectNoUtf32le) {
				detected = str::WireCoding::utf16le;
				bomSize = detail::Utf16BOMSize;
			}

			/* consume the bom-size number of bytes from the buffer */
			pBufSize -= bomSize;
			for (size_t i = 0; i < pBufSize; ++i)
				pBuffer[i] = pBuffer[i + bomSize];

			/* mark the bom as detected and update the encoding */
			pMode = str::BOMMode::none;
			pCoding = detected;
			return count;
		}
		constexpr uint8_t fNextByte(const uint8_t* index, const uint8_t* begin) const {
			if (index >= begin)
				return *index;
			return pBuffer[pBufSize - (begin - index)];
		}
		template <class WiType, bool LittleEndian, bool AsciiMode>
		constexpr void fProcess(auto& sink, const uint8_t* begin, const uint8_t* end, bool sourceComplete) {
			WiType buffer[TranscodeBufCapacity]{};
			size_t chars = 0;
			const uint8_t* index = begin - intptr_t(pBufSize);

			/* iterate until all data have been transcoded */
			while (true) {
				size_t count = std::min<size_t>(TranscodeBufCapacity - chars, size_t(end - index) / sizeof(WiType));
				if (count == 0)
					break;

				/* fetch the next complete characters or until the buffer has been filled up (no space for optimization
				*	if sizeof(WiType) == 1, as bytes might be located in pBuffer as well instead of just from the input) */
				for (size_t i = 0; i < count; ++i) {
					uint32_t value = uint32_t(fNextByte(index, begin));

					/* check if the value is little endian and extract it */
					if constexpr (LittleEndian) {
						if constexpr (sizeof(WiType) > 1)
							value |= uint32_t(fNextByte(index + 1, begin)) << 8;
						if constexpr (sizeof(WiType) > 2) {
							value |= uint32_t(fNextByte(index + 2, begin)) << 16;
							value |= uint32_t(fNextByte(index + 3, begin)) << 24;
						}
					}

					/* extract the big endian value */
					else {
						if constexpr (sizeof(WiType) > 1)
							value = (value << 8) | fNextByte(index + 1, begin);
						if constexpr (sizeof(WiType) > 2) {
							value = (value << 8) | fNextByte(index + 2, begin);
							value = (value << 8) | fNextByte(index + 3, begin);
						}
					}
					buffer[chars++] = WiType(value);
					index += sizeof(WiType);
				}
				std::basic_string_view<WiType> view = { buffer, buffer + chars };

				/* iterate over the chars until all chars have been transcoded or an incomplete error has been returned */
				size_t consumed = 0;
				while (consumed < chars) {
					/* check if the length is 0, in which case the codepoint must be incomplete */
					str::Decoded dec{};
					if constexpr (AsciiMode)
						dec = str::PartialEscaped<CodeError>(view.substr(consumed));
					else
						dec = str::PartialCodepoint<CodeError>(view.substr(consumed));
					if (dec.consumed == 0)
						break;

					/* consume the codepoint and write it to the sink */
					consumed += dec.consumed;
					str::CodepointTo<CodeError>(sink, dec.cp, 1);
				}

				/* move any remaining characters down in the buffer */
				chars -= consumed;
				for (size_t i = 0; i < chars; ++i)
					buffer[i] = buffer[i + consumed];
			}

			/* reset the index to not consider the buffered chars as consumed (in order to be able to fetch the raw non-reordered bytes) */
			index -= chars * sizeof(WiType);
			pBufSize = (end - index);

			/* check if these were the last bytes, in which case any remaining data have
			*	to be considered incomplete and an error char can be added for them */
			if (sourceComplete) {
				/* write the invalid codepoint to the sink, which will definitely not be writable and trigger the error-handler */
				if (pBufSize > 0)
					str::CodepointTo<CodeError>(sink, str::Invalid, 1);
				pBufSize = 0;
			}

			/* write the remaining bytes back to the local buffer (cannot overwrite itself as it will at worst
			*	write itself directly to itself, and at best, it will only read from the incoming data) */
			else for (size_t i = 0; i < pBufSize; ++i)
				pBuffer[i] = fNextByte(index++, begin);
		}
		constexpr void fSinkInto(auto& sink, const uint8_t* ptr, size_t size, bool sourceComplete) {
			const uint8_t* begin = ptr;
			const uint8_t* end = ptr + size;

			/* check if the source has already been considered completed before */
			if (pSourceCompleted)
				return;
			pSourceCompleted = sourceComplete;

			/* check if the BOM has not yet been read and immediately return if the BOM is
			*	still considered unprocessed, as too few bytes must have been provided */
			if (pMode != str::BOMMode::none) {
				begin += fDetectBOM(begin, end, sourceComplete);
				if (pMode != str::BOMMode::none)
					return;
			}

			/* pass the data to the proper handler */
			if (pCoding == str::WireCoding::utf8)
				fProcess<char8_t, false, false>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf16le)
				fProcess<char16_t, true, false>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf16be)
				fProcess<char16_t, false, false>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf32le)
				fProcess<char32_t, true, false>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf32be)
				fProcess<char32_t, false, false>(sink, begin, end, sourceComplete);
			else
				fProcess<char8_t, false, true>(sink, begin, end, sourceComplete);
		}

	public:
		constexpr auto& to(str::AnySink auto&& sink, const uint8_t* ptr, size_t size, bool sourceComplete = false) {
			fSinkInto(sink, ptr, size, sourceComplete);
			return sink;
		}
		template <str::AnySink SinkType>
		constexpr SinkType read(const uint8_t* ptr, size_t size, bool sourceComplete = false) {
			SinkType out{};
			fSinkInto(out, ptr, size, sourceComplete);
			return out;
		}
	};

	/* write a string of any type to a byte-sink and encode it using the defined wire-encoding */
	template <char32_t CodeError = err::DefChar>
	class ToWire {
	private:
		str::WireCoding pCoding = str::WireCoding::utf8;
		bool pAddBOM = false;

	public:
		constexpr ToWire(str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true) {
			pCoding = coding;
			pAddBOM = addBOM;
		}

	private:
		template <class Type, bool LittleEndian>
		static constexpr void fToBytes(uint8_t* buffer, Type value) {
			if constexpr (LittleEndian) {
				for (int i = 0; i < sizeof(Type); ++i) {
					buffer[i] = uint8_t(value);
					value >>= 8;
				}
			}
			else for (int i = sizeof(Type) - 1; i >= 0; --i) {
				buffer[i] = uint8_t(value);
				value >>= 8;
			}
		}
		template <class WiType, bool LittleEndian, bool AsciiMode>
		static constexpr void fWriteCP(auto& sink, char32_t cp) {
			/* check if an ascii-encoded character should be written out */
			if constexpr (AsciiMode) {
				str::Escaped<char8_t> enc = str::Escape<str::Escaped<char8_t>, CodeError>(cp, false, 1);
				str::CallWire(sink, reinterpret_cast<const uint8_t*>(enc.data()), enc.size());
			}
			else {
				str::Encoded<WiType> enc = str::Codepoint<str::Encoded<WiType>, CodeError>(cp, 1);

				/* check if the data can be written out directly or apply the byte-order */
				if constexpr (sizeof(WiType) == 1)
					str::CallWire(sink, reinterpret_cast<const uint8_t*>(enc.data()), enc.size());
				else {
					uint8_t buf[str::MaxEncBytes<WiType>] = { 0 };
					size_t off = 0;
					for (size_t i = 0; i < enc.size(); ++i) {
						fToBytes<WiType, LittleEndian>(buf + off, enc[i]);
						off += sizeof(WiType);
					}
					str::CallWire(sink, buf, off);
				}
			}
		}
		template <class ChType, class WiType, bool LittleEndian, bool AsciiMode>
		constexpr void fProcess(auto& sink, std::basic_string_view<ChType> view) {
			/* check if a BOM needs to be added */
			if (pAddBOM)
				fWriteCP<WiType, LittleEndian, AsciiMode>(sink, detail::BOMCodePoint);
			pAddBOM = false;

			/* decode all characters and write them out */
			while (!view.empty()) {
				auto [cp, len] = str::GetCodepoint<CodeError>(view);
				view = view.substr(len);

				/* write the codepoint out (only if its valid) */
				if (cp != str::Invalid)
					fWriteCP<WiType, LittleEndian, AsciiMode>(sink, cp);
			}
		}

	public:
		constexpr auto& write(str::IsWire auto&& sink, const str::AnyStr auto& string) {
			using ChType = str::StrChar<decltype(string)>;
			std::basic_string_view<ChType> view{ string };

			/* pass the data to the proper handler (will automatically write the BOM if necessary) */
			if (pCoding == str::WireCoding::utf8)
				fProcess<ChType, char8_t, false, false>(sink, view);
			else if (pCoding == str::WireCoding::utf16le)
				fProcess<ChType, char16_t, true, false>(sink, view);
			else if (pCoding == str::WireCoding::utf16be)
				fProcess<ChType, char16_t, false, false>(sink, view);
			else if (pCoding == str::WireCoding::utf32le)
				fProcess<ChType, char32_t, true, false>(sink, view);
			else if (pCoding == str::WireCoding::utf32be)
				fProcess<ChType, char32_t, false, false>(sink, view);
			else
				fProcess<ChType, char8_t, false, true>(sink, view);
			return sink;
		}
	};
}
