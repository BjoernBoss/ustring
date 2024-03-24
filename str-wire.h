#pragma once

#include "str-common.h"
#include "str-convert.h"

#include <string>
#include <algorithm>
#include <vector>
#include <iostream>
#include <type_traits>

namespace str {
	/* create the escape-sequence in ascii-only characters using ascii characters, common escape
	*	sequences, \xhh, \u{(0|[1-9a-fA-F]h*)} (compact is designed for one-liner strings) */
	constexpr auto& EscapeAsciiInto(str::AnySink auto&& sink, char32_t cp, bool compact) {
		/* check if the character is an escape sequence */
		if (cp == U'\t')
			str::Append(sink, compact ? U"\\t" : U"\t");
		else if (cp == U'\n')
			str::Append(sink, compact ? U"\\n" : U"\n");
		else if (cp == U'\0')
			str::Append(sink, U"\\0");
		else if (cp == U'\r')
			str::Append(sink, compact ? U"\\r" : U"\r");
		else if (cp == U'\"')
			str::Append(sink, compact ? U"\\\"" : U"\"");
		else if (cp == U'\'')
			str::Append(sink, compact ? U"\\'" : U"'");
		else if (cp == U'\\')
			str::Append(sink, U"\\\\");

		/* check if the character can be added as-is */
		else if (cp >= 0x20 && cp != 0x7f && cp::Ascii(cp))
			str::AppChars(sink, cp);

		/* check if the codepoint can be added as short-version */
		else if (cp <= 0xff) {
			str::Append(sink, U"\\x");
			str::AppChars(sink, cp::DigitToAscii(cp >> 4));
			str::AppChars(sink, cp::DigitToAscii(cp & 0x0f));
		}

		/* add the codepoint as the unicode-codepoint */
		else {
			str::Append(sink, U"\\u{");
			int32_t digit = 28;
			while (((cp >> digit) & 0x0f) == 0)
				digit -= 4;

			while (digit >= 0) {
				str::AppChars(sink, cp::DigitToAscii((cp >> digit) & 0x0f));
				digit -= 4;
			}

			str::AppChars(sink, U'}');
		}
		return sink;
	}

	/* create the escape-sequence in ascii-only characters (returning std::basic_string) */
	template <str::IsChar ChType>
	constexpr std::basic_string<ChType> EscapeAscii(char32_t cp, bool compact) {
		std::basic_string<ChType> out{};
		return str::EscapeAsciiInto(out, cp, compact);
	}

	/* create the escape-sequence in ascii-only characters (returning str::Small<Capacity>) */
	template <str::IsChar ChType, intptr_t Capacity>
	constexpr str::Small<ChType, Capacity> EscapeAscii(char32_t cp, bool compact) {
		str::Small<ChType, Capacity> out{};
		return str::EscapeAsciiInto(out, cp, compact);
	}

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

		/* 10 for ascii-encoding of \u{10ffff} */
		static constexpr size_t MaxEscapeHexits = 6;
		static constexpr size_t MaxAsciiEscape = detail::MaxEscapeHexits + 4;

		static constexpr size_t MaxCodingSize = std::max<size_t>({ detail::MaxBOMSize, detail::MaxAsciiEscape, str::MaxEncBytes<char>,
			str::MaxEncBytes<wchar_t>, str::MaxEncBytes<char8_t>, str::MaxEncBytes<char16_t>, str::MaxEncBytes<char32_t> });

		static constexpr uint8_t AsciiBOM[detail::AsciiBOMSize] = { u8'\\', u8'u', u8'{', u8'f', u8'e', u8'f', u8'f', u8'}' };
		static constexpr uint8_t Utf8BOM[detail::Utf8BOMSize] = { 0xef, 0xbb, 0xbf };
		static constexpr uint8_t Utf16leBOM[detail::Utf16BOMSize] = { 0xff, 0xfe };
		static constexpr uint8_t Utf16beBOM[detail::Utf16BOMSize] = { 0xfe, 0xff };
		static constexpr uint8_t Utf32leBOM[detail::Utf32BOMSize] = { 0xff, 0xfe, 0x00, 0x00 };
		static constexpr uint8_t Utf32beBOM[detail::Utf32BOMSize] = { 0x00, 0x00, 0xfe, 0xff };
	}

	/* byte sink interface which requires:
	*	operator() to take the wire object and a pointer and a size */
	template <class Type>
	struct ByteSink;
	template <class Type>
	concept IsWire = !std::is_const_v<std::remove_reference_t<Type>> &&
		requires(Type & t, const uint8_t * ptr, size_t sz) {
		str::ByteSink<std::remove_cvref_t<Type>>{}(t, ptr, sz);
	};

	/* wrapper to write to a byte-sink */
	constexpr auto& SinkBytes(str::IsWire auto&& sink, const uint8_t* ptr, size_t sz) {
		str::ByteSink<std::remove_cvref_t<decltype(sink)>>{}(sink, ptr, sz);
		return sink;
	}

	/* read a string from raw bytes and dynamically detect the encoding based on a BOM or use the given encoding (will at all times consume all passed in bytes) */
	class FromWire {
		static constexpr size_t TranscodeBufCapacity = std::max<size_t>(1024, detail::MaxCodingSize);
	private:
		uint8_t pBuffer[detail::MaxCodingSize] = { 0 };
		size_t pBufSize = 0;
		char32_t pCpOnError = 0;
		str::WireCoding pCoding = str::WireCoding::utf8;
		str::BOMMode pMode = str::BOMMode::none;
		bool pSourceCompleted = false;

	public:
		constexpr FromWire(str::WireCoding coding = str::WireCoding::utf8, str::BOMMode mode = str::BOMMode::detectAll, char32_t cpOnError = cp::DefErrorChar) {
			pCpOnError = (cp::Unicode(cpOnError) ? cpOnError : 0);
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
		template <class WiType, bool LittleEndian>
		constexpr void fProcessData(auto& sink, const uint8_t* begin, const uint8_t* end, bool sourceComplete) {
			WiType buffer[TranscodeBufCapacity]{};
			size_t chars = 0;
			const uint8_t* index = begin - intptr_t(pBufSize);

			/* iterate until all data have been transcoded */
			while (true) {
				size_t count = std::min<size_t>(TranscodeBufCapacity - chars, size_t(end - index) / sizeof(WiType));
				if (count == 0)
					break;

				/* fetch the next complete characters or until the buffer has been filled up */
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

				/* iterate over the chars until all chars have been transcoded or an incomplete error has been returned */
				size_t consumed = 0;
				std::basic_string_view<WiType> view{ buffer, buffer + chars };
				while (consumed < chars) {
					auto [cp, len] = str::TranscodeInto(sink, view.substr(consumed), false);
					if (cp == cp::Incomplete)
						break;

					/* consume the character and check if the codepoint could either not be decoded
					*	or encoded properly, in which case the error characer can be written out */
					consumed += len;
					if (cp != cp::Success && pCpOnError != 0)
						str::EncodeInto(sink, pCpOnError);
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
				if (pBufSize > 0 && pCpOnError != 0)
					str::EncodeInto(sink, pCpOnError);
				pBufSize = 0;
			}

			/* write the remaining bytes back to the local buffer (cannot overwrite itself as it will at worst
			*	write itself directly to itself, and at best, it will only read from the incoming data) */
			else for (size_t i = 0; i < pBufSize; ++i)
				pBuffer[i] = fNextByte(index++, begin);
		}
		constexpr void fProcessAscii(auto& sink, const uint8_t* begin, const uint8_t* end, bool sourceComplete) {
			const uint8_t* index = begin - intptr_t(pBufSize);

			/* escape sequence */
			char8_t buffer[detail::MaxAsciiEscape] = { 0 };
			size_t escaped = 0;
			char32_t hexValue = 0;

			/* iterate until all data have been transcoded */
			while (index < end) {
				/* check if the next character is an ascii character */
				char8_t c = char32_t(fNextByte(index++, begin));

				/* check if an escape sequence is currently active and if this is a valid continuation to it (no need to check if the
				*	codepoint is a valid ascii-character, as the escape-continuation checks will only continue for valid ascii characters) */
				if (escaped > 0) {
					/* check if this is the first character of the escape sequence */
					if (escaped == 1) {
						bool invalid = false;

						/* check if this is a complex escape sequence */
						if (c == U'x' || c == U'u') {
							buffer[escaped++] = c;
							hexValue = 0;
							continue;
						}
						escaped = 0;

						/* lookup the escape-sequence */
						switch (c) {
						case U'\\':
						case U'/':
						case U'"':
						case U'\'':
						case U'?':
							break;
						case U't':
							c = U'\t';
							break;
						case U'r':
							c = U'\r';
							break;
						case U'n':
							c = U'\n';
							break;
						case U'b':
							c = U'\b';
							break;
						case U'f':
							c = U'\f';
							break;
						case U'0':
							c = U'\0';
							break;
						case U'a':
							c = U'\a';
							break;
						case U'v':
							c = U'\v';
							break;
						default:
							invalid = true;
							break;
						}

						/* write the character out or write the error-cp if the escape sequence is unknown */
						if ((invalid || !str::EncodeInto(sink, c)) && pCpOnError != 0)
							str::EncodeInto(sink, pCpOnError);
						continue;
					}

					/* check if the \xhh sequence has been completed/is still valid */
					if (buffer[1] == U'x') {
						size_t val = cp::AsciiToDigit(c);

						/* check if the next character is valid (hexValue cannot overflow as it has 32-bits and only two hexits will be added) */
						if (val < 16) {
							hexValue = hexValue * 16 + char32_t(val);
							buffer[escaped++] = c;

							/* check if the end has been reached */
							if (escaped >= 4) {
								escaped = 0;
								if (!str::EncodeInto(sink, char32_t(hexValue)) && pCpOnError != 0)
									str::EncodeInto(sink, pCpOnError);
							}
							continue;
						}
					}

					/* check if this is the opening bracket of the \u sequence */
					else if (escaped == 2 && c == U'{') {
						buffer[escaped++] = c;
						continue;
					}

					/* check if the end of the \u sequence has been reached */
					else if (escaped >= 4 && c == U'}') {
						escaped = 0;
						if (!str::EncodeInto(sink, char32_t(hexValue)) && pCpOnError != 0)
							str::EncodeInto(sink, pCpOnError);
						continue;
					}

					/* check if a valid digit has been encountered (if first digit was null, no other digits are allowed) */
					else if (escaped < 3 + detail::MaxEscapeHexits && (hexValue > 0 || escaped == 3)) {
						size_t val = cp::AsciiToDigit(c);
						if (val < 16) {
							hexValue = hexValue * 16 + char32_t(val);
							buffer[escaped++] = c;
							continue;
						}
					}

					/* if this point has been reached, the escape sequence can be aborted as it will not be continued in a valid fashion */
					if (pCpOnError != 0)
						str::EncodeInto(sink, pCpOnError);
					escaped = 0;
				}

				/* check if the codepoint starts a new escape sequence or write it out */
				if (c == U'\\')
					buffer[escaped++] = u8'\\';
				else if ((!cp::Ascii(c) || !str::EncodeInto(sink, c)) && pCpOnError != 0)
					str::EncodeInto(sink, pCpOnError);
			}

			/* reset the index to not consider the buffered chars as consumed */
			index -= escaped * sizeof(char8_t);
			pBufSize = (end - index);

			/* check if these were the last bytes, in which case any remaining data have
			*	to be considered incomplete and an error char can be added for them */
			if (sourceComplete) {
				if (pBufSize > 0 && pCpOnError != 0)
					str::EncodeInto(sink, pCpOnError);
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
				fProcessData<char8_t, false>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf16le)
				fProcessData<char16_t, true>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf16be)
				fProcessData<char16_t, false>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf32le)
				fProcessData<char32_t, true>(sink, begin, end, sourceComplete);
			else if (pCoding == str::WireCoding::utf32be)
				fProcessData<char32_t, false>(sink, begin, end, sourceComplete);
			else
				fProcessAscii(sink, begin, end, sourceComplete);
		}

	public:
		constexpr auto& into(str::AnySink auto&& sink, const uint8_t* ptr, size_t size, bool sourceComplete = false) {
			fSinkInto(sink, ptr, size, sourceComplete);
			return sink;
		}
		template <str::IsChar ChType>
		constexpr std::basic_string<ChType> to(const uint8_t* ptr, size_t size, bool sourceComplete = false) {
			std::basic_string<ChType> out{};
			fSinkInto(out, ptr, size, sourceComplete);
			return out;
		}
		template <str::IsChar ChType, intptr_t Capacity>
		constexpr str::Small<ChType, Capacity> to(const uint8_t* ptr, size_t size, bool sourceComplete = false) {
			str::Small<ChType, Capacity> out{};
			fSinkInto(out, ptr, size, sourceComplete);
			return out;
		}
	};

	/* write a string of any type to a byte-sink and encode it using the defined wire-encoding */
	class ToWire {
	private:
		char32_t pCpOnError = 0;
		str::WireCoding pCoding = str::WireCoding::utf8;
		bool pAddBOM = false;

	public:
		constexpr ToWire(str::WireCoding coding = str::WireCoding::utf8, bool addBOM = true, char32_t cpOnError = cp::DefErrorChar) {
			pCpOnError = (cp::Unicode(cpOnError) ? cpOnError : 0);
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
		template <class WiType, bool LittleEndian>
		static constexpr void fWriteDataCP(auto& sink, char32_t cp) {
			/* encode the character (cannot fail as this function is only called with valid codepoints for unicode-types) */
			str::CPSmall<WiType> small = str::Encode<WiType>(cp);

			/* convert the encoded string to bytes */
			uint8_t buffer[str::MaxEncBytes<WiType>] = { 0 };
			size_t size = 0;
			for (size_t i = 0; i < small.size(); ++i) {
				fToBytes<WiType, LittleEndian>(buffer + size, small[i]);
				size += sizeof(WiType);
			}

			/* write the bytes to the sink */
			str::SinkBytes(sink, buffer, size);
		}
		template <class ChType, class WiType, bool LittleEndian>
		constexpr void fProcessData(auto& sink, std::basic_string_view<ChType> view) {
			/* check if a BOM needs to be added */
			if (pAddBOM)
				fWriteDataCP<WiType, LittleEndian>(sink, detail::BOMCodePoint);
			pAddBOM = false;

			/* decode all characters and write them out */
			while (!view.empty()) {
				auto [cp, len] = str::Decode(view, true);
				view = view.substr(len);

				/* check if the codepoint is valid */
				if (cp::Valid(cp))
					fWriteDataCP<WiType, LittleEndian>(sink, cp);

				/* write the error character out */
				else if (pCpOnError != 0)
					fWriteDataCP<WiType, LittleEndian>(sink, pCpOnError);
			}
		}
		static constexpr void fWriteAsciiCP(auto& sink, char32_t cp) {
			char8_t buffer[detail::MaxAsciiEscape] = { 0 };

			/* write the characters to the buffer (cannot overflow the buffer as ascii maps one-to-one to utf8 and all this function is only called with valid codepoints) */
			size_t size = str::EscapeAsciiInto(str::Chars(buffer), cp, false).size();

			/* write the ascii characters to the sink */
			str::SinkBytes(sink, reinterpret_cast<const uint8_t*>(buffer), size);
		}
		template <class ChType>
		constexpr void fProcessAscii(auto& sink, std::basic_string_view<ChType> view) {
			/* check if a BOM needs to be added */
			if (pAddBOM)
				fWriteAsciiCP(sink, detail::BOMCodePoint);
			pAddBOM = false;

			/* decode all characters and write them out */
			while (!view.empty()) {
				auto [cp, len] = str::Decode(view, true);
				view = view.substr(len);

				/* check if the codepoint is valid */
				if (cp::Valid(cp))
					fWriteAsciiCP(sink, cp);

				/* write the error character out */
				else if (pCpOnError != 0)
					fWriteAsciiCP(sink, pCpOnError);
			}
		}

	public:
		constexpr auto& write(str::IsWire auto&& sink, const str::AnyString auto& string) {
			using ChType = str::StringCharType<decltype(string)>;
			std::basic_string_view<ChType> view = str::StringView<ChType>(string);

			/* pass the data to the proper handler (will automatically write the BOM if necessary) */
			if (pCoding == str::WireCoding::utf8)
				fProcessData<ChType, char8_t, false>(sink, view);
			else if (pCoding == str::WireCoding::utf16le)
				fProcessData<ChType, char16_t, true>(sink, view);
			else if (pCoding == str::WireCoding::utf16be)
				fProcessData<ChType, char16_t, false>(sink, view);
			else if (pCoding == str::WireCoding::utf32le)
				fProcessData<ChType, char32_t, true>(sink, view);
			else if (pCoding == str::WireCoding::utf32be)
				fProcessData<ChType, char32_t, false>(sink, view);
			else
				fProcessAscii<ChType>(sink, view);
			return sink;
		}
	};

	/* wrapper to create a byte-sink into a constant buffer or a pointer and make the written size available */
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

	/* specializations for byte-sinks */
	template <>
	struct ByteSink<std::vector<uint8_t>> {
		constexpr void operator()(std::vector<uint8_t>& sink, const uint8_t* ptr, size_t size) const {
			sink.insert(sink.end(), ptr, ptr + size);
		}
	};
	template <>
	struct ByteSink<std::ostream> {
		void operator()(std::ostream& sink, const uint8_t* ptr, size_t size) const {
			sink.write(reinterpret_cast<const char*>(ptr), size);
		}
	};
	template <>
	struct ByteSink<str::Bytes> {
		void operator()(str::Bytes& sink, const uint8_t* ptr, size_t size) const {
			sink.write(ptr, size);
		}
	};
}