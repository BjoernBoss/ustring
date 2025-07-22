/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024 Bjoern Boss Henrichsen */
#pragma once

#include "str-common.h"
#include "str-coding.h"

#include <cmath>
#include <utility>
#include <tuple>
#include <cstring>

/*
*	Coding-Rules:
*	 - decoding using str::GetAscii<err::Nothing>, any invalid decodings will simply abort the parsing up to that point
*	 - encoding using str::CodepointTo<err::Nothing>/str::FastcodeAllTo<err::Nothing>
*		(character set is so small and essential that every codepage should support it)
*/
namespace str {
	/* check if type is non-bool numeric type */
	template <class Type>
	concept IsNumber = std::is_arithmetic_v<Type> && !std::is_same_v<Type, bool>;
	template <class Type>
	concept IsInteger = std::is_integral_v<Type> && !std::is_same_v<Type, bool>;
	template <class Type>
	concept IsFloat = std::is_floating_point_v<Type>;

	enum class NumResult : uint8_t {
		empty,
		invalid,
		valid,
		range
	};
	enum class NumStyle : uint8_t {
		lower,
		upper,
		lowerWithPrefix,
		upperWithPrefix
	};

	/*
	*	none: no prefix expected or parsed
	*	optional: consume prefix if it matches the radix
	*	mandatory: fail if no prefix matching the current radix is encountered
	*	overwrite: update the radix if a prefix is detected
	*	detect: fail if no prefix is found and otherwise use it to detect the radix
	*/
	enum class PrefixMode : uint8_t {
		none,
		optional,
		mandatory,
		overwrite,
		detect
	};

	/*
	*	general: fixed or scientific, depending on precision and magnitude of number
	*	example: fixed: 100.00; scientific: 1.0000e+02
	*	example: full: 10.0000; trim: 10.0; short: 10
	*/
	enum class FloatStyle : uint8_t {
		generalFull,
		generalTrim,
		generalShort,
		fixedFull,
		fixedTrim,
		fixedShort,
		scientificFull,
		scientificTrim,
		scientificShort,

		/* common mappings */
		general = generalShort,
		scientific = scientificFull,
		fixed = fixedTrim
	};

	struct ParsedNum {
		size_t consumed = 0;
		str::NumResult result = str::NumResult::empty;
	};

	template <str::IsNumber Type>
	struct ParsedNumValue {
		Type value = 0;
		size_t consumed = 0;
		str::NumResult result = str::NumResult::empty;
	};

	/* valid radix range as well as radix indicating float is a hex-float */
	static constexpr size_t MinRadix = 2;
	static constexpr size_t MaxRadix = 36;
	static constexpr size_t HexFloat = size_t(-1);

	namespace detail {
		inline constexpr uint8_t BitsForNumber(uint64_t v) {
			if (v == 0)
				return 0;

			/* check if the value is smaller than 32-bit and look for the value to become zeor */
			if (v <= std::numeric_limits<uint32_t>::max()) {
				uint8_t bits = 1;
				while (v >> bits)
					++bits;
				return bits;
			}

			/* find the highest set bit */
			uint8_t bit = 63;
			while ((v & (uint64_t(0x01) << bit)) == 0x00)
				--bit;
			return (bit + 1);
		}

		inline constexpr bool RoundToNearestRoundUp(uint64_t lowerMantissa, uint64_t topTail) {
			bool lowestBit = (lowerMantissa & 0x01) != 0;
			bool firstTailBit = (topTail >> 63) != 0;
			bool tailIsZero = (topTail << 1) == 0;

			/* check if the value should be rounded (based on rounding towards nearest) */
			return firstTailBit && (!tailIsZero || lowestBit);
		}

		/* ensure that exponent of this size will not result in any overflows for primitive additions
		*	(including additions/subtractions of normal integer-type sizes such as 32/64/128/...) */
		static constexpr int32_t LargeIntSafeExponentLimit = int32_t(0x01 << 24);

		static constexpr long double LogBase2[str::MaxRadix + 1] = {
			0.0000000000000000, 0.0000000000000000, 1.0000000000000000, 1.5849625007211563,
			2.0000000000000000, 2.3219280948873620, 2.5849625007211560, 2.8073549220576040,
			3.0000000000000000, 3.1699250014423126, 3.3219280948873626, 3.4594316186372978,
			3.5849625007211565, 3.7004397181410920, 3.8073549220576037, 3.9068905956085187,
			4.0000000000000000, 4.0874628412503400, 4.1699250014423120, 4.2479275134435850,
			4.3219280948873630, 4.3923174227787610, 4.4594316186372970, 4.5235619560570130,
			4.5849625007211570, 4.6438561897747240, 4.7004397181410930, 4.7548875021634690,
			4.8073549220576040, 4.8579809951275730, 4.9068905956085190, 4.9541963103868760,
			5.0000000000000000, 5.0443941193584530, 5.0874628412503400, 5.1292830169449660,
			5.1699250014423120
		};
		static constexpr uint32_t MagnitudeIn32Bit[str::MaxRadix + 1] = {
			0x00000000, 0x00000000, 0x80000000, 0xcfd41b91, 0x40000000, 0x48c27395, 0x81bf1000, 0x75db9c97,
			0x40000000, 0xcfd41b91, 0x3b9aca00, 0x8c8b6d2b, 0x19a10000, 0x309f1021, 0x57f6c100, 0x98c29b81,
			0x10000000, 0x18754571, 0x247dbc80, 0x3547667b, 0x4c4b4000, 0x6b5a6e1d, 0x94ace180, 0xcaf18367,
			0x0b640000, 0x0e8d4a51, 0x1269ae40, 0x17179149, 0x1cb91000, 0x23744899, 0x2b73a840, 0x34e63b41,
			0x40000000, 0x4cfa3cc1, 0x5c13d840, 0x6d91b519, 0x81bf1000
		};
		static constexpr uint8_t DigitsIn32Bit[str::MaxRadix + 1] = {
			 0,  0, 31, 20, 15, 13, 12, 11,
			10, 10,  9,  9,  8,  8,  8,  8,
			 7,  7,  7,  7,  7,  7,  7,  7,
			 6,  6,  6,  6,  6,  6,  6,  6,
			 6,  6,  6,  6,  6
		};
		static constexpr const char32_t* PrefixLower = U"__b_q___o_d_____x___________________";
		static constexpr const char32_t* PrefixUpper = U"__B_Q___O_D_____X___________________";
		static constexpr const char32_t* DigitLower = U"0123456789abcdefghijklmnopqrstuvwxyz";
		static constexpr uint8_t AsciiDigitMap[detail::AsciiRange] = {
		0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
		0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
		0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0xff, 0xff, 0xff, 0xff, 0xff,
		0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
		0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0xff, 0xff, 0xff, 0xff, 0xff
		};
		static constexpr const char32_t* DigitUpper = U"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

		/* to differentiate nan/inf from normal numbers */
		static constexpr uint32_t MaxRadixBeforeSpecialChar = std::min<uint32_t>(
			std::max<uint32_t>({ detail::AsciiDigitMap[U'i'], detail::AsciiDigitMap[U'n'], detail::AsciiDigitMap[U'f'] }),
			std::max<uint32_t>({ detail::AsciiDigitMap[U'n'], detail::AsciiDigitMap[U'a'], detail::AsciiDigitMap[U'n'] })
		);

		struct PreComputedEntry {
			int32_t size = 0;
			int32_t nulls = 0;
			int32_t offset = 0;
		};
		static constexpr uint32_t MaxPowerTenEntries = 16;
		static constexpr uint32_t MaxPowerTenSplit = 20;
		static constexpr uint32_t MaxPowerTenExponentDegrade = detail::MaxPowerTenSplit * detail::MaxPowerTenEntries * 4;
		static constexpr detail::PreComputedEntry PreComputedPowerTen[detail::MaxPowerTenEntries] = {
			{ 0x01, 0x00, 0x000 }, { 0x03, 0x00, 0x001 }, { 0x04, 0x01, 0x004 }, { 0x06, 0x01, 0x008 }, { 0x07, 0x02, 0x00e }, { 0x08, 0x03, 0x015 }, { 0x0a, 0x03, 0x01d }, { 0x0b, 0x04, 0x027 },
			{ 0x0c, 0x05, 0x032 }, { 0x0e, 0x05, 0x03e }, { 0x0f, 0x06, 0x04c }, { 0x11, 0x06, 0x05b }, { 0x12, 0x07, 0x06c }, { 0x13, 0x08, 0x07e }, { 0x16, 0x08, 0x091 }, { 0x17, 0x09, 0x0a7 }
		};
		static constexpr uint32_t PreComputedPowerTenData[] = {
			0x00000001, 0x63100000, 0x6bc75e2d, 0x00000005, 0xb9f56100, 0x5ca4bfab, 0x6329f1c3, 0x0000001d,
			0x10000000, 0x946590d9, 0xd762422c, 0x9a224501, 0x4f272617, 0x0000009f, 0xcec10000, 0x63a22764,
			0xefa418ca, 0xcdd17b25, 0x6bdfef70, 0x9dea3e1f, 0x0000035f, 0xa82e8f10, 0xaab24308, 0x8e211a7c,
			0xf38ace40, 0x84c4ce0b, 0x7ceb0b27, 0xad2594c3, 0x00001249, 0x21000000, 0x17bb8a0c, 0x56af8ea4,
			0x06479fa9, 0x5d4bb236, 0x80dc5fe0, 0xf0feaa0a, 0xa88ed940, 0x6b1a80d0, 0x00006323, 0x1e851000,
			0x6e4f615b, 0x187b2a69, 0x0450e21c, 0x2fdd342b, 0x635027ee, 0xa6c97199, 0x8e4ae916, 0x17082e28,
			0x1a496e6f, 0x0002196e, 0xfbc32d81, 0x5222d0f4, 0xb70f2850, 0x5713f2f3, 0xdc421413, 0xd6395d7d,
			0xf8591999, 0x0092381c, 0x86b314d6, 0x7aa577b9, 0x12b7fe61, 0x000b616a, 0xbb100000, 0x02f79478,
			0x8c1b74c0, 0xb0f05d00, 0xa9dbc675, 0xe2d9b914, 0x650f72df, 0x77284b4c, 0x6df6e016, 0x514391c2,
			0x2795c9cf, 0xd6e2ab55, 0x9ca8e627, 0x003db1a6, 0xb9b2e100, 0x8288753c, 0xcd3f1693, 0x89b43a6b,
			0x089e87de, 0x684d4546, 0xfddba60c, 0xdf249391, 0x3068ec13, 0x99b44427, 0xb68141ee, 0x5802cac3,
			0xd96851f1, 0x7d7625a2, 0x014e718d, 0x10000000, 0x09ab5531, 0xa60c58d2, 0x566126cb, 0x6a1c8387,
			0x7587f4c1, 0x2c44e876, 0x41a047cf, 0xc908059e, 0xa0ba063e, 0xe7cfc8e8, 0xe1fac055, 0xef0144b2,
			0x24207eb0, 0xd1722573, 0xe4b8f981, 0x071505ae, 0x1c410000, 0x6e174a27, 0xec62ae57, 0xef2289aa,
			0xb6a2fbdd, 0x17e1efe4, 0x3366bdf2, 0x37b48880, 0xbfb82c3e, 0x19acde91, 0xd4f46408, 0x35ff6a4e,
			0x67566a0e, 0x40dbb914, 0x782a3bca, 0x6b329b68, 0xf5afc5d9, 0x266469bc, 0x97cbe710, 0x26d769e8,
			0xb4e4723e, 0x5b90aa86, 0x9c333922, 0x4b7a0775, 0x2d47e991, 0x9a6ef977, 0x160b40e7, 0x0c92f8c4,
			0xf25ff010, 0x25c36c11, 0xc9f98b42, 0x730b919d, 0x05ff7caf, 0xb0432d85, 0x2d2b7569, 0xa657842c,
			0xd01fef10, 0xa1000000, 0x6c5cd4e9, 0x9be47d6f, 0xf93bd9e7, 0x77626fa1, 0xc68b3451, 0xde2b59e8,
			0xcf3cde58, 0x2246ff58, 0xa8577c15, 0x26e77559, 0x17776753, 0xebe6b763, 0xe3fd0a5f, 0x33e83969,
			0xa805a035, 0xf631b987, 0x211f0f43, 0xd85a43db, 0xab1bf596, 0x683f19a2, 0x00000004, 0xf4dd1000,
			0x5d450952, 0xaeb442e1, 0xa3b3342e, 0x3fcda36f, 0xb4287a6e, 0x4bc177f7, 0x67d2c8d0, 0xaea8f8e0,
			0xadc93b67, 0x6cc856b3, 0x959d9d0b, 0x5b48c100, 0x4abe8a3d, 0x52d936f4, 0x71dbe84d, 0xf91c21c5,
			0x4a458109, 0xd7aad86a, 0x08e14c7c, 0x759ba59c, 0xe43c8800, 0x00000017
		};
		static constexpr uint64_t PreComputedPowerTenSmall[detail::MaxPowerTenSplit] = {
			0x0000000000000001, 0x000000000000000a, 0x0000000000000064, 0x00000000000003e8,
			0x0000000000002710, 0x00000000000186a0, 0x00000000000f4240, 0x0000000000989680,
			0x0000000005f5e100, 0x000000003b9aca00, 0x00000002540be400, 0x000000174876e800,
			0x000000e8d4a51000, 0x000009184e72a000, 0x00005af3107a4000, 0x00038d7ea4c68000,
			0x002386f26fc10000, 0x016345785d8a0000, 0x0de0b6b3a7640000, 0x8ac7230489e80000
		};

		template <size_t Units>
		static constexpr bool DynamicInt = (Units < 2);

		/* large integer with optional dynamic capacity (Invariant: if size > 0: data[size - 1] must never be null)
		*	- requires at least two units to be static, else dynamic capacity is used
		*	- last data-package is considered scratch-pad and is not guaranteed to perserve any information */
		template <size_t Units>
		struct LargeInt {
			std::conditional_t<detail::DynamicInt<Units>, std::vector<uint32_t>, uint32_t[Units]> data{};
			int32_t size = 0;
			int32_t nulls = 0;
			int32_t capacity = 0;
		};

		template <size_t Units>
		constexpr detail::LargeInt<Units> LargeLoad(uint64_t v, uint32_t capacity) {
			detail::LargeInt<Units> out;
			if constexpr (detail::DynamicInt<Units>) {
				int32_t actual = std::max<uint32_t>(capacity, 2);
				out.data.resize(actual);
				out.capacity = actual;
			}
			else
				out.capacity = Units;

			/* check if the value itself is null */
			if (v == 0)
				return out;

			/* write the value itself to the output integer, as compressed as possible */
			if ((out.data[0] = uint32_t(v)))
				out.size = 1;
			else
				out.nulls = 1;
			if ((out.data[out.size] = uint32_t(v >> 32)))
				++out.size;
			return out;
		}
		template <size_t Units>
		constexpr void LargeMul(detail::LargeInt<Units>& a, uint32_t b) {
			/* check if one of the values is null */
			if (a.size == 0 || b == 0) {
				a.size = 0;
				a.nulls = 0;
				return;
			}

			/* count the leading nulls of a and compute its actual size */
			int32_t aOff = 0;
			while (a.data[aOff] == 0)
				++aOff;
			int32_t aSize = a.size - aOff;

			/* compute the output size and nulls and check if additional information must be skipped, as the result would overflow the capacity */
			a.size = aSize + 1;
			a.nulls += aOff;
			bool skip = (a.size > a.capacity);

			/* perform the actual multiplication into the output (cannot overwrite used data, as it first reads from a, then writes, and aOff can at most be positive) */
			uint32_t carry = 0;
			if (skip) {
				a.size = a.capacity;
				++a.nulls;
				carry = uint32_t((uint64_t(b) * uint64_t(a.data[aOff])) >> 32);
				++aOff;
				--aSize;
			}

#pragma warning(push)

			/* [Reading invalid data from 'a.data']:
			*	aOff + (aSize - 1) cannot become larger than a.capacity, as aSize = a.size - aOff (aSize + aOff = a.size)
			*	and further if a skip occurs, aOff is increased, but aSize is decreased again */
#pragma warning(disable : 6385)

			/* [Buffer overrun while writing to 'a.data']:
			*	Cannot overrun as aSize can at most be (a.capacity - 1), as aSize = a.capacity
			*	would trigger a skip, which would again reduce aSize by one */
#pragma warning(disable : 6386)

			/* iterate over the elements of a and perform the mulitplication */
			for (int32_t i = 0; i < aSize; ++i) {
				uint64_t value = uint64_t(b) * uint64_t(a.data[aOff + i]) + uint64_t(carry);

				/* write the value to the output and update the shift */
				a.data[i] = uint32_t(value);
				carry = uint32_t(value >> 32);
			}

			/* write the remainder of the carry to the output */
			a.data[aSize] = carry;
#pragma warning(pop)

			/* clip any trailing nulls in the output (cannot underflow as value cannot be null) */
			while (a.data[a.size - 1] == 0)
				--a.size;
		}
		template <size_t Units>
		constexpr detail::LargeInt<Units> LargeMul(const detail::LargeInt<Units>& a, const detail::LargeInt<Units>& b) {
			detail::LargeInt<Units> out = detail::LargeLoad<Units>(0, a.capacity);

			/* check if the values themselves are null */
			if (a.size == 0 || b.size == 0)
				return out;

			/* count the leading nulls of a and b and compute their actual sizes */
			int32_t aOff = 0, bOff = 0;
			while (a.data[aOff] == 0)
				++aOff;
			while (b.data[bOff] == 0)
				++bOff;
			int32_t aSize = a.size - aOff, bSize = b.size - bOff;

			/* compute the output size and nulls and check if additional information must be skipped, as the result would overflow the capacity */
			out.size = aSize + bSize;
			out.nulls = a.nulls + b.nulls + aOff + bOff;
			int32_t skipped = 0;
			if (out.size > out.capacity) {
				skipped = (out.size - out.capacity);
				out.size = out.capacity;
				out.nulls += skipped;
			}

			/* perform the multiplication with the smaller of the two values on the outer loop */
			uint32_t* resData = &out.data[0];
			const uint32_t* outerData = (aSize <= bSize ? &a.data[aOff] : &b.data[bOff]);
			const uint32_t* innerData = (aSize <= bSize ? &b.data[bOff] : &a.data[aOff]);
			int32_t outerSize = (aSize <= bSize ? aSize : bSize), innerSize = (aSize <= bSize ? bSize : aSize);
			for (int32_t oIndex = 0; oIndex < outerSize; ++oIndex) {
				uint64_t oValue = outerData[oIndex];

				/* check if any data to be multiplied exist and compute the index into the output buffer and into the input inner-data */
				if (oValue == 0)
					continue;
				int32_t offset = oIndex - skipped, iIndex = 0;
				uint32_t carry = 0;
				if (offset < 0) {
					if ((iIndex = -offset) >= innerSize)
						continue;
					carry = uint32_t((oValue * uint64_t(innerData[size_t(iIndex - 1)])) >> 32);
				}

				/* iterate over the elements of a and perform the mulitplication */
				for (; iIndex < innerSize; ++iIndex) {
					uint64_t value = oValue * uint64_t(innerData[iIndex]) + uint64_t(resData[size_t(offset + iIndex)]) + uint64_t(carry);

					/* write the value to the output and update the shift */
					resData[size_t(offset + iIndex)] = uint32_t(value);
					carry = uint32_t(value >> 32);
				}

				/* write the remainder of the carry to the output */
				resData[size_t(offset + iIndex)] = carry;
			}

			/* clip any trailing nulls in the output (cannot underflow as value cannot be null) */
			while (out.data[out.size - 1] == 0)
				--out.size;
			return out;
		}
		template <size_t Units>
		constexpr uint32_t LargeDiv(detail::LargeInt<Units>& n, const detail::LargeInt<Units>& d) {
			int32_t diff = int32_t(n.nulls + n.size) - int32_t(d.nulls + d.size);
			if (diff < 0 || n.size == 0)
				return 0;

			/* skip any leading nulls in the denominator (cannot overflow, as the denominator must not be null) */
			int32_t dOff = 0;
			while (d.data[dOff] == 0)
				++dOff;
			int32_t dSize = (d.size - dOff);

			/* extract the high-order denominator to be used (shift it for the 32nd bit to be set) */
			uint32_t shift = 0;
			uint64_t denValue = uint64_t(d.data[size_t(dOff + dSize - 1)]);
			while ((denValue << 1) <= std::numeric_limits<uint32_t>::max()) {
				denValue <<= 1;
				++shift;
			}
			if (shift != 0 && dSize > 1)
				denValue |= uint64_t(d.data[size_t(dOff + dSize - 2)] >> (32 - shift));

			/* perform the division by performing a partial division (only using the high-order values) and then performing a multiply-subtraction */
			uint32_t result = 0;
			while (diff >= 0) {
				/* check if the numerator needs to be shifted such that the subtraction has enough space to be performed without
				*	loss of information (cannot result in an underflow of nulls, as total difference must at least be equal) */
				int32_t minSize = (diff > 0 && dSize < n.capacity ? dSize + 1 : dSize);
				if (minSize > n.size) {
					int32_t nulls = (minSize - n.size);
					for (int32_t i = minSize; i > 0; --i)
						n.data[size_t(i - 1)] = (i > nulls ? n.data[size_t(i - 1 - nulls)] : 0);
					n.nulls -= nulls;
					n.size += nulls;
				}

				/* extract the next numerator to be used and check if the end has been reached */
				uint64_t numValue = uint64_t(n.data[size_t(n.size - 1)]);
				if (shift != 0) {
					numValue <<= shift;
					if (n.size > 1)
						numValue |= (uint64_t(n.data[size_t(n.size - 2)]) >> (32 - shift));
				}
				if (diff == 0 && numValue < denValue)
					break;

				/* check if the division can be extended by 32-bit */
				bool additional = (diff > 0 && numValue <= std::numeric_limits<uint32_t>::max());
				if (additional) {
					numValue <<= 32;
					if (n.size > 1)
						numValue |= uint64_t(n.data[size_t(n.size - 2)]) << shift;
					if (n.size > 2)
						numValue |= (uint64_t(n.data[size_t(n.size - 3)]) >> (32 - shift));
				}

				/* perform the actual division and limit it to prevent overflows (will at all times result in a value
				*	of at least 1, as any numerator smaller than/equal to denominator will result in additional-data
				*	being passed in, and any other value is already larger and will therefore at least result in 1) */
				uint64_t divResult = numValue / denValue;
				if (divResult > std::numeric_limits<uint32_t>::max())
					divResult = std::numeric_limits<uint32_t>::max();

				/* setup the offsets and actual size to be used for the multiply-subtraciton (loss
				*	of information in case of the denominator holding maximum-number of information) */
				uint32_t borrow = 0, initSub = 0;
				int32_t subSize = dSize, nOff = (n.size - dSize);
				if (additional) {
					++subSize;
					--nOff;
				}

				/* perform the mulitply-subtraction on the numerator */
				for (int32_t i = 0; i < subSize; ++i) {
					uint64_t sub = borrow;
					if (i < dSize)
						sub += uint64_t(d.data[size_t(dOff + i)]) * divResult;
					borrow = uint32_t(sub >> 32);

					/* perform the subtraction and check if a borrow has to be carried through */
					if (i > 0 || nOff >= 0) {
						uint32_t old = n.data[size_t(i + nOff)];
						if (old < uint32_t(sub))
							++borrow;
						n.data[size_t(i + nOff)] -= uint32_t(sub);
					}
					else if (uint32_t(sub) > 0) {
						initSub = uint32_t(sub);
						++borrow;
					}
				}

				/* check if a borrow is left, in which case the result was too large by one and has to be added back again */
				if (borrow > 0) {
					--divResult;

					/* perform the addition back again (any remaining carry can be ignored, as it will be equivalent to the previous borrow, which is now being fixed) */
					uint32_t carry = 0;
					for (int32_t i = 0; i < subSize; ++i) {
						uint64_t add = carry;
						if (i < dSize)
							add += uint64_t(d.data[size_t(dOff + i)]);

						if (i == 0 && nOff < 0)
							add += initSub;
						else {
							add += n.data[size_t(i + nOff)];
							n.data[size_t(i + nOff)] = uint32_t(add);
						}
						carry = uint32_t(add >> 32);
					}
				}

				/* check if the division yielded no result, in which case the numerator holds the remainder (check
				*	before updating the result, as it might contain the valid result from the previous iteration) */
				if (divResult == 0)
					return result;
				result = uint32_t(divResult);

				/* update the numerator size by checking how many slots have been cleared form the numerator */
				while (n.size > 0 && n.data[size_t(n.size - 1)] == 0) {
					--n.size;
					--diff;
				}

				/* check if this was a 'perfect-division', in which case the numerator holds no information anymore */
				if (n.size == 0) {
					n.nulls = 0;
					break;
				}
			}
			return result;
		}
		template <size_t Units>
		constexpr detail::LargeInt<Units> LargePowDef(const detail::LargeInt<Units>& v, uint32_t b, uint32_t e) {
			detail::LargeInt<Units> out = v, temp = detail::LargeLoad<Units>(b, v.capacity);

			/* square-multiply algorithm */
			while (e > 0) {
				if (e & 0x01)
					out = detail::LargeMul<Units>(out, temp);
				if ((e >>= 1) > 0)
					temp = detail::LargeMul<Units>(temp, temp);
			}
			return out;
		}
		template <size_t Units>
		constexpr detail::LargeInt<Units> LargePowSmall(const detail::LargeInt<Units>& v, uint32_t b, uint32_t e) {
			detail::LargeInt<Units> out = v;

			/* perform the direct in-place scale-up (using square-multiply algorithm) */
			uint32_t scaleUp = 1, walk = b;
			while (e > 0) {
				if (e & 0x01)
					scaleUp *= walk;
				if ((e >>= 1) > 0)
					walk *= walk;
			}

			/* perform the small multiplication and return the value */
			detail::LargeMul<Units>(out, scaleUp);
			return out;
		}
		template <size_t Units>
		constexpr detail::LargeInt<Units> LargePowTwo(const detail::LargeInt<Units>& v, uint32_t e) {
			detail::LargeInt<Units> out = v;

			/* compute the shift to be applied to the nulls itself and multiply the rest with the remainder */
			out.nulls += (e / 32);
			if ((e %= 32) > 0)
				detail::LargeMul<Units>(out, uint32_t(0x01) << e);
			return out;
		}
		template <size_t Units>
		constexpr detail::LargeInt<Units> LargePowTen(const detail::LargeInt<Units>& v, uint32_t e) {
			detail::LargeInt<Units> out = v, temp = detail::LargeLoad<Units>(0, v.capacity);

			do {
				/* break up the power-computation (but limit it by the precomputed amount of data, might require multiple iterations for very large exponents) */
				uint32_t large = std::min<uint32_t>(e / detail::MaxPowerTenSplit, detail::MaxPowerTenEntries - 1), small = (e % detail::MaxPowerTenSplit);

				/* perform the large multiplication */
				if (large > 0) {
					const detail::PreComputedEntry& entry = detail::PreComputedPowerTen[large];

					/* populate the intermediate large-int */
					temp.size = std::min<int32_t>(temp.capacity, entry.size);
					int32_t skipped = (entry.size - temp.size);
					temp.nulls = entry.nulls + skipped;
					std::memcpy(&temp.data[0], &detail::PreComputedPowerTenData[entry.offset + skipped], sizeof(uint32_t) * temp.size);

					/* perform the multiplication */
					out = detail::LargeMul<Units>(out, temp);
					e -= large * detail::MaxPowerTenSplit;
				}

				/* perform the small multiplication */
				if (small > 0) {
					/* populate the intermediate large-int */
					temp.nulls = 0;
					temp.size = 2;
					std::memcpy(&temp.data[0], &detail::PreComputedPowerTenSmall[small], sizeof(uint32_t) * temp.size);
					if (temp.data[1] == 0)
						--temp.size;

					/* perform the multiplication */
					out = detail::LargeMul<Units>(out, temp);
					e -= small;
				}
			} while (e > 0);
			return out;
		}
		template <size_t Units>
		constexpr detail::LargeInt<Units> LargePow(const detail::LargeInt<Units>& v, uint32_t b, uint32_t e) {
			if (e == 0)
				return v;

			/* check if the base is a power of ten */
			if (b == 10) {
				if (e < detail::MaxPowerTenExponentDegrade)
					return detail::LargePowTen<Units>(v, e);
				return detail::LargePowDef<Units>(v, b, e);
			}

			/* check if the base is a power of two */
			if (b == 2)
				return detail::LargePowTwo<Units>(v, e);
			if ((b & (b - 1)) == 0)
				return detail::LargePowTwo<Units>(v, uint32_t(e * detail::LogBase2[b]));

			/* check if the exponent is considered small */
			if (e <= detail::DigitsIn32Bit[b])
				return detail::LargePowSmall<Units>(v, b, e);

			/* perform the default square-multiply algorithm */
			return detail::LargePowDef<Units>(v, b, e);
		}

		struct PrefixParseOut {
			size_t radix = 0;
			size_t signConsumed = 0;
			size_t prefixConsumed = 0;
			bool negative = false;
		};
		template <class Type, class ChType>
		constexpr detail::PrefixParseOut ParseSignAndPrefix(const std::basic_string_view<ChType>& view, bool signOnly) {
			enum class PrState : uint8_t {
				preSign,
				preZero,
				prePrefix
			} state = PrState::preSign;
			detail::PrefixParseOut out;

			size_t prefixConsumed = 0;
			while (true) {
				/* decode the next character */
				auto [cp, consumed] = str::GetAscii<err::Nothing>(view.substr(out.signConsumed + prefixConsumed));
				if (cp == str::Invalid)
					return out;

				/* check if a sign has been encountered */
				if (state == PrState::preSign) {
					if (cp == U'+' || (std::is_signed_v<Type> && cp == U'-')) {
						out.negative = (cp == U'-');
						out.signConsumed += consumed;
					}
					if (signOnly)
						return out;
					state = PrState::preZero;
					continue;
				}

				/* check if the leading null of the prefix has been encountered */
				prefixConsumed += consumed;
				if (state == PrState::preSign || state == PrState::preZero) {
					if (cp != U'0')
						return out;
					state = PrState::prePrefix;
					continue;
				}

				/* decode the prefix */
				if (cp == U'x' || cp == U'X')
					out.radix = 16;
				else if (cp == U'b' || cp == U'B')
					out.radix = 2;
				else if (cp == U'd' || cp == U'D')
					out.radix = 10;
				else if (cp == U'q' || cp == U'Q')
					out.radix = 4;
				else if (cp == U'o' || cp == U'O')
					out.radix = 8;
				else
					prefixConsumed = 0;
				out.prefixConsumed = prefixConsumed;
				return out;
			}
		}

		template <class Type, class ChType>
		constexpr std::tuple<Type, size_t, bool> ParseRawInteger(const std::basic_string_view<ChType>& view, size_t radix, str::Decoded& dec, bool negative) {
			size_t totalConsumed = 0;
			bool overflow = false;

			/* setup the overflow detection values */
			using UType = std::make_unsigned_t<Type>;
			UType valueLimit = std::numeric_limits<Type>::max();
			if constexpr (std::is_signed_v<Type>) {
				/* ensure that the largest absolute negative value is larger by one than its positive largest value */
				static_assert(-std::numeric_limits<Type>::max() == std::numeric_limits<Type>::min() + 1, "Signed type is expected to use the two's complement");
				if (negative)
					++valueLimit;
			}
			UType valueLastDigit = valueLimit % radix;
			valueLimit /= UType(radix);

			/* iterate over the digits and parse them */
			UType value = 0;
			while (dec.cp != str::Invalid) {
				/* check if the codepoint is a valid digit */
				size_t digit = detail::AsciiDigitMap[dec.cp];
				if (digit >= radix)
					break;

				/* update the value and check for an overflow */
				if (value < valueLimit || (value == valueLimit && digit <= valueLastDigit))
					value = value * UType(radix) + UType(digit);
				else
					overflow = true;

				/* mark the characters as consumed and decode the next character */
				totalConsumed += dec.consumed;
				dec = str::GetAscii<err::Nothing>(view.substr(totalConsumed));
			}

			/* apply the sign and return the value */
			if constexpr (std::is_signed_v<Type>)
				return { (negative ? -Type(value) : Type(value)), totalConsumed, overflow };
			else
				return { value, totalConsumed, overflow };
		}

		template <class Type, class ChType>
		constexpr str::ParsedNum ParseInteger(Type& num, const std::basic_string_view<ChType>& view, size_t radix, bool negative) {
			/* parse the raw value */
			str::Decoded dec = str::GetAscii<err::Nothing>(view);
			auto [value, totalConsumed, overflow] = detail::ParseRawInteger<Type, ChType>(view, radix, dec, negative);

			/* check if an overflow occurred and setup the overflow value */
			if (!overflow)
				num = value;
			else if (std::is_signed_v<Type> && negative)
				num = std::numeric_limits<Type>::min();
			else
				num = std::numeric_limits<Type>::max();

			/* finalize the output structure (string cannot be empty, or will only be empty if a prefix
			*	has already been parsed, in which case the empty string will be considered an error as well) */
			str::ParsedNum out{};
			out.consumed = totalConsumed;
			if (totalConsumed == 0)
				out.result = str::NumResult::invalid;
			else if (overflow)
				out.result = str::NumResult::range;
			else
				out.result = str::NumResult::valid;
			return out;
		}

		template <class NumType>
		constexpr void PrintInteger(auto& sink, NumType num, size_t radix, size_t digitCount, bool addPrefix, bool upperCase) {
			static_assert(sizeof(NumType) <= 8, "Type must be smaller than/equal to 64-bit");

			/* digit map to contain all digits */
			uint8_t digits[sizeof(num) * 8] = { 0 };
			uint8_t* next = digits;

			/* check if a sign needs to be added */
			uint64_t act = 0;
			bool negative = false;
			if constexpr (std::is_signed_v<NumType>) {
				if ((negative = (num < 0)))
					act = uint64_t(-int64_t(num));
				else
					act = uint64_t(int64_t(num));
			}
			else
				act = uint64_t(num);

			/* extract the separate digits */
			do {
				*(next++) = uint8_t(act % radix);
				act /= radix;
			} while (act > 0);

			/* write the sign out */
			if (negative)
				str::CodepointTo<err::Nothing>(sink, U'-', 1);

			/* check if a prefix should be added */
			if (addPrefix) {
				char32_t c = (upperCase ? detail::PrefixUpper : detail::PrefixLower)[radix];
				if (c != U'_') {
					str::CodepointTo<err::Nothing>(sink, U'0', 1);
					str::CodepointTo<err::Nothing>(sink, c, 1);
				}
			}

			/* write the additional null-digits out */
			if (size_t(next - digits) < digitCount)
				str::CodepointTo<err::Nothing>(sink, U'0', digitCount - size_t(next - digits));


			/* write the digits out */
			const char32_t* digitSet = (upperCase ? detail::DigitUpper : detail::DigitLower);
			while (next != digits)
				str::CodepointTo<err::Nothing>(sink, digitSet[*(--next)], 1);
		}

		struct SpecialOut {
			size_t consumed = 0;
			bool nan = false;
		};
		template <class ChType>
		constexpr detail::SpecialOut ParseFloatSpecial(const std::basic_string_view<ChType>& view, size_t radix, str::Decoded dec) {
			size_t consumed = 0;

			/* check if a special character is required */
			if (radix > detail::MaxRadixBeforeSpecialChar) {
				if (dec.cp != U'#')
					return detail::SpecialOut{};
				dec = str::GetAscii<err::Nothing>(view.substr(consumed += dec.consumed));
			}

			/* check if it could be a nan or inf */
			bool nan = false;
			if (dec.cp == U'n' || dec.cp == U'N')
				nan = true;
			else if (dec.cp != U'i' && dec.cp != U'I')
				return detail::SpecialOut{};

			/* fetch and validate the remaining characters */
			const char32_t* mask = U"nfanNFAN";
			for (size_t i = 0; i < 2; ++i) {
				dec = str::GetAscii<err::Nothing>(view.substr(consumed += dec.consumed));
				if (dec.cp != mask[i + (nan ? 2 : 0)] && dec.cp != mask[i + (nan ? 2 : 0) + 4])
					return detail::SpecialOut{};
			}
			return { consumed + dec.consumed, nan };
		}

		struct MantissaOut {
			size_t consumed = 0;
			uint64_t mantissa = 0;
			int64_t dotOffset = 0;
			uint8_t shift = 0;
			int8_t range = 0;
			bool invalid = true;
			bool trailIsNonNull = false;
		};
		template <class ChType>
		constexpr detail::MantissaOut ParseFloatMantissa(const std::basic_string_view<ChType>& view, size_t radix, str::Decoded& dec) {
			detail::MantissaOut out;

			/* setup the number of digits to be processed (maximum plus one, where the last digit is used to fill remaining bits) */
			uint64_t valueLimit = std::numeric_limits<uint64_t>::max() / radix;
			uint64_t valueLastDigit = std::numeric_limits<uint64_t>::max() % radix;

			/* parse the entire mantissa (integer component and fractional part) */
			bool inFraction = false, hasValue = false, valueClosed = false;
			while (dec.cp != str::Invalid) {
				size_t digit = 0;

				/* extract the digit or check if its the dot */
				if ((digit = detail::AsciiDigitMap[dec.cp]) >= radix) {
					if (dec.cp != U'.' || inFraction)
						break;
					inFraction = true;
				}
				else {
					/* mark the result as valid as it contains at least one digit */
					out.invalid = false;

					/* update the dot-offset as either the ignored digits of the integer part or the considered digits of the fractional part */
					if (inFraction) {
						if (!valueClosed && --out.dotOffset == 0)
							out.range = -1;
					}
					else if (valueClosed && ++out.dotOffset == 0)
						out.range = 1;

					/* check if an actual digit has been encountered and add it to the mantissa-accumulation */
					if (!valueClosed && (hasValue || dec.cp != U'0')) {
						hasValue = true;

						/* check if the digit can be added without the chance of an overflow */
						if (out.mantissa < valueLimit || (out.mantissa == valueLimit && digit <= valueLastDigit))
							out.mantissa = radix * out.mantissa + digit;

						else {
							/* the digit must be the last digit, in which case the multiplication must be split up as it might
							*	overflow the mantissa (the addition of the digit/upper part of the low mantissa cannot overflow
							*	the mantissa as a 32x32 bit multiplication with a small radix-value cannot overflow the 64bit value) */
							uint64_t lMantissa = uint64_t(radix) * uint64_t(uint32_t(out.mantissa)) + digit;
							uint64_t uMantissa = uint64_t(radix) * uint64_t(out.mantissa >> 32) + uint32_t(lMantissa >> 32);

							/* find the number of bits in the upper mantissa and construct the final mantissa value */
							out.shift = detail::BitsForNumber(uMantissa >> 32);
							out.mantissa = (uMantissa << (32 - out.shift)) | (uint64_t(uint32_t(lMantissa)) >> out.shift);

							/* mark the value as closed as any additional information will not affect the
							*	accumulated mantissa and update the trailing-flag for the discarded bits */
							valueClosed = true;
							out.trailIsNonNull = ((lMantissa & ~(~uint64_t(0) << out.shift)) != 0);
						}
					}

					/* check if the value is closed and the tail is non-zero (necessary for rounding) */
					else if (valueClosed && dec.cp != U'0')
						out.trailIsNonNull = true;
				}

				/* decode the next character */
				dec = str::GetAscii<err::Nothing>(view.substr(out.consumed += dec.consumed));
			}
			return out;
		}

		struct ExponentOut {
			size_t consumed = 0;
			int64_t exponent = 0;
			int8_t range = 0;
			bool invalid = false;
		};
		template <class ChType>
		constexpr detail::ExponentOut ParseFloatExponent(const std::basic_string_view<ChType>& view, size_t radix, str::Decoded& dec) {
			detail::ExponentOut out;

			/* extract a potential sign of the exponent */
			bool expNegative = false;
			if (dec.cp == U'+' || dec.cp == U'-') {
				expNegative = (dec.cp == U'-');
				dec = str::GetAscii<err::Nothing>(view.substr(out.consumed += dec.consumed));
			}

			/* parse the exponent and check if an invalid exponent has been encountered */
			auto [value, consumed, overflow] = detail::ParseRawInteger<int64_t, ChType>(view.substr(out.consumed), radix, dec, expNegative);
			if (consumed == 0) {
				out.invalid = true;
				return out;
			}
			out.consumed += consumed;
			out.exponent = value;

			/* check if an overflow error occurred and return the value */
			if (overflow)
				out.range = (expNegative ? -1 : 1);
			return out;
		}

		template <class Type, size_t Units>
		constexpr std::pair<Type, int8_t> ConstructFloat(uint64_t mantissa, uint8_t manShift, int32_t exponent, uint32_t radix, bool hexFloat, bool trailIsNull) {
			uint64_t flMantissa = 0;
			int flExponent = 0;
			bool roundUp = false;

			/* check if this is a hex-float, in which case the exponent only needs to be corrected by the mantissa-shift
			*	(exponent is guaranteed to fit into an integer) and otherwise use the large-integers to produce the bits */
			if (hexFloat) {
				flExponent = int(exponent + manShift);
				flMantissa = mantissa;

				/* setup the rounding information */
				int32_t bits = int32_t(detail::BitsForNumber(flMantissa)) - std::numeric_limits<Type>::digits;
				if (bits >= 0)
					roundUp = detail::RoundToNearestRoundUp(flMantissa >> bits, (flMantissa << (64 - bits)) | (trailIsNull ? 0x00 : 0x01));
			}
			else {
				/* compute the binary exponent of the number and the number of bits of the mantissa (exponent is guaranteed to fit into an integer;
				*	manShift can be ignored, as it cancels out when scaling the mantissa, compared to dividing it by the mantissa-bits) */
				flExponent = int(std::ceil(exponent * detail::LogBase2[radix]));
				int32_t manDigits = int32_t(detail::BitsForNumber(mantissa));

				/* construct the numerator and denominator of the large type used to compute the final bits */
				detail::LargeInt<Units> numerator = detail::LargeLoad<Units>(mantissa, 0), denominator = detail::LargeLoad<Units>(1, 0);
				flExponent += manDigits;
				if (flExponent > 0)
					denominator = detail::LargePow<Units>(denominator, 2, uint32_t(flExponent));
				else if (flExponent < 0)
					numerator = detail::LargePow<Units>(numerator, 2, uint32_t(-flExponent));
				if (exponent > 0)
					numerator = detail::LargePow<Units>(numerator, radix, uint32_t(exponent));
				else if (exponent < 0)
					denominator = detail::LargePow<Units>(denominator, radix, uint32_t(-exponent));

				/* loop until the required number of mantissa-bits have been extracted */
				uint32_t fetchedBits = 0, requiredBits = std::numeric_limits<Type>::digits;
				do {
					/* compute the number of bits to extract this iteration and perform the multiply/division */
					uint32_t numberOfBits = std::min<uint32_t>(requiredBits - fetchedBits, 24);
					detail::LargeMul<Units>(numerator, uint32_t(0x01) << numberOfBits);
					uint32_t temp = detail::LargeDiv<Units>(numerator, denominator);

					/* check if this is the initial number, in which case any leading bits need to be corrected for */
					if (fetchedBits == 0) {
						uint32_t actual = detail::BitsForNumber(temp);
						flExponent -= (numberOfBits - actual);
						numberOfBits = actual;
					}

					/* update the overall fetched bits and the mantissa with the newly fetched bits */
					fetchedBits += numberOfBits;
					flMantissa = (flMantissa << numberOfBits) | temp;
				} while (fetchedBits < requiredBits);
				flExponent = flExponent + manShift - std::numeric_limits<Type>::digits;

				/* extract the rounding information */
				detail::LargeMul<Units>(numerator, 0x02);
				bool tailBit = (detail::LargeDiv<Units>(numerator, denominator) != 0);
				roundUp = detail::RoundToNearestRoundUp(flMantissa, (uint64_t(tailBit ? 0x01 : 0x00) << 63) | ((trailIsNull && numerator.size == 0) ? 0x00 : 0x01));
			}

			/* check if the mantissa needs to be rounded up and update it and the exponent accordingly */
			if (roundUp) {
				if (flMantissa == std::numeric_limits<uint64_t>::max()) {
					flMantissa >>= 1;
					++flExponent;
				}
				++flMantissa;
			}

			/* construct the final value and check if an overflow/underflow occurred */
			errno = 0;
			Type value = std::ldexp(Type(flMantissa), flExponent);
			if (errno == ERANGE)
				return { Type(), (exponent < 0 ? -1 : 1) };
			return { value, 0 };
		}

		template <class Type, class ChType>
		constexpr str::ParsedNum ParseFloat(Type& num, const std::basic_string_view<ChType>& view, size_t radix, bool negative, bool hexFloat) {
			using Limits = std::numeric_limits<Type>;

			/* validate the float-type is usable (to ensure the exponent cannot trigger any overflow in the power-operations) */
			static_assert(Limits::digits <= 64, "Type must have mantissa smaller than/equal to 64-bit");
			static_assert(Limits::radix == 2, "Type must use exponent-base two");
			static_assert(Limits::min_exponent >= -detail::LargeIntSafeExponentLimit, "Type must have an exponent safe to be handled by large integers");
			static_assert(Limits::max_exponent <= detail::LargeIntSafeExponentLimit, "Type must have an exponent safe to be handled by large integers");
			static_assert(sizeof(int) >= sizeof(int32_t), "Large integer requires the integer type to be at least 32-bit");
			size_t totalConsumed = 0;

			/* check if the value is an inf/nan number (ignore any errors as the characters can otherwise just be parsed as normal numbers) */
			str::Decoded dec = str::GetAscii<err::Nothing>(view);
			detail::SpecialOut special = detail::ParseFloatSpecial<ChType>(view, radix, dec);
			if (special.consumed > 0) {
				Type value = (special.nan ? (Limits::has_quiet_NaN ? Limits::quiet_NaN() : Limits::signaling_NaN()) : Limits::infinity());
				num = (negative ? -value : value);
				return str::ParsedNum{ special.consumed, str::NumResult::valid };
			}

			/* parse the mantissa and check if an error occurred (ignore range errors for now; radix will already be 16 for hex-floats) */
			detail::MantissaOut mantissa = detail::ParseFloatMantissa<ChType>(view, radix, dec);
			totalConsumed += mantissa.consumed;
			if (mantissa.invalid) {
				num = (negative ? -Type(0) : Type(0));
				return str::ParsedNum{ totalConsumed, str::NumResult::invalid };
			}

			/* check if an exponent has been detected and parse it */
			detail::ExponentOut exponent;
			if (hexFloat ? (dec.cp == U'p' || dec.cp == U'P') : (dec.cp == U'e' || dec.cp == U'E' || dec.cp == U'^')) {
				dec = str::GetAscii<err::Nothing>(view.substr(totalConsumed += dec.consumed));

				/* parse the exponent and check if an error occurred */
				exponent = detail::ParseFloatExponent<ChType>(view.substr(totalConsumed), (hexFloat ? 10 : radix), dec);
				totalConsumed += exponent.consumed;
				if (exponent.invalid) {
					num = (negative ? -Type(0) : Type(0));
					return str::ParsedNum{ totalConsumed, str::NumResult::invalid };
				}
			}

			/* check if a range-error occurred */
			int8_t range = mantissa.range;
			if (range == 0)
				range = exponent.range;

			/* apply the shift to the exponent, based on the encountered fractional digits, and check if a range-error occurred */
			if (range == 0 && mantissa.dotOffset != 0) {
				int64_t old = exponent.exponent;
				exponent.exponent += (hexFloat ? mantissa.dotOffset * 4 : mantissa.dotOffset);
				if (mantissa.dotOffset < 0 ? (old <= exponent.exponent) : (old >= exponent.exponent))
					range = (mantissa.dotOffset < 0 ? -1 : 1);
			}

			/* validate the range of the exponent */
			if (range == 0 && std::abs(exponent.exponent * detail::LogBase2[radix]) > detail::LargeIntSafeExponentLimit)
				range = (exponent.exponent < 0 ? -1 : 1);

			/* check if the mantissa is null, in which case any range-errors are ignored */
			if (mantissa.mantissa == 0) {
				num = 0;
				return str::ParsedNum{ totalConsumed, str::NumResult::valid };
			}

			/* construct the final float value */
			if (range == 0) {
				std::pair<Type, int8_t> result = detail::ConstructFloat<Type, 6>(mantissa.mantissa, mantissa.shift, int32_t(exponent.exponent), uint32_t(radix), hexFloat, !mantissa.trailIsNonNull);

				/* check if a valid result has been found and return it */
				if (result.second == 0) {
					num = (negative ? -result.first : result.first);
					return str::ParsedNum{ totalConsumed, str::NumResult::valid };
				}
				range = result.second;
			}

			/* setup the range-error value and return the response */
			Type value = (range < 0 ? Type(0.0) : Limits::infinity());
			num = (negative ? -value : value);
			return str::ParsedNum{ totalConsumed, str::NumResult::range };
		}

		constexpr void FlushFloatDigits(auto& sink, char32_t digit, size_t count, intptr_t& digitsBeforePoint) {
			/* check if the point will be inserted within this iteration */
			if (digitsBeforePoint >= 0 && size_t(digitsBeforePoint) < count) {
				str::CodepointTo<err::Nothing>(sink, digit, size_t(digitsBeforePoint));
				str::CodepointTo<err::Nothing>(sink, U'.', 1);
				str::CodepointTo<err::Nothing>(sink, digit, count - size_t(digitsBeforePoint));
			}

			/* insert all digits */
			else
				str::CodepointTo<err::Nothing>(sink, digit, count);
			digitsBeforePoint -= count;
		}

		constexpr void PrintHexFloat(auto& sink, intptr_t totalDigits, int32_t flExponent, uint64_t flMantissa, str::FloatStyle style, bool upperCase) {
			/* write out the first implicit 1 and patch the digit count */
			str::CodepointTo<err::Nothing>(sink, U'1', 1);
			--totalDigits;

			/* find the topmost bit to be to the right of the point (must exist as the mantissa cannot be null) and check if the value should be rounded up */
			int32_t topBit = detail::BitsForNumber(flMantissa) - 1, dropped = int32_t(topBit - totalDigits * 4);
			if (dropped > 0) {
				/* remove the bits to be dropped */
				uint64_t droppedBits = (flMantissa << (64 - dropped));
				flMantissa >>= dropped;
				flExponent += dropped;
				topBit -= dropped;

				/* check if the value should be rounded up and if the rounding will overflow the topmost bit */
				if (detail::RoundToNearestRoundUp(flMantissa, droppedBits)) {
					++flMantissa;
					if (((flMantissa >> topBit) & 0x01) == 0)
						++topBit;
				}
			}

			/* adjust the exponent based on the actual position of the point */
			flExponent += topBit;

			/* write the digits out (no need to keep track of last-digits, only of nulls) */
			const char32_t* digitSet = (upperCase ? detail::DigitUpper : detail::DigitLower);
			intptr_t delayed = 0, digitsBeforePoint = 0;
			for (intptr_t i = 0; i < totalDigits; ++i) {
				topBit -= 4;

				/* extract the next hex-digit */
				uint32_t digit = 0;
				if (topBit >= 0)
					digit = uint32_t(flMantissa >> topBit) & 0x0f;
				else if (topBit > -4)
					digit = uint32_t(flMantissa << -topBit) & 0x0f;

				/* check if the digit should be delayed and otherwise write it out */
				if (digit == 0)
					++delayed;
				else {
					detail::FlushFloatDigits(sink, U'0', delayed, digitsBeforePoint);
					detail::FlushFloatDigits(sink, digitSet[digit], 1, digitsBeforePoint);
					delayed = 0;
				}
			}

			/* check if remaining nulls need to be written out (style can only be one of the scientific styles) */
			if (style == str::FloatStyle::scientificFull)
				detail::FlushFloatDigits(sink, U'0', delayed, digitsBeforePoint);
			else if (style == str::FloatStyle::scientificTrim && digitsBeforePoint >= 0)
				detail::FlushFloatDigits(sink, U'0', digitsBeforePoint + 1, digitsBeforePoint);

			/* write the exponent out */
			str::CodepointTo<err::Nothing>(sink, upperCase ? U'P' : U'p', 1);
			str::CodepointTo<err::Nothing>(sink, flExponent < 0 ? U'-' : U'+', 1);
			if (flExponent < 0)
				flExponent = -flExponent;

			/* write the exponent to the sink */
			detail::PrintInteger<uint32_t>(sink, uint32_t(flExponent), 10, 1, false, upperCase);
		}

		template <class Type, size_t Units>
		constexpr void PrintNormalFloat(auto& sink, intptr_t totalDigits, int32_t rawExponent, uint64_t flMantissa, str::FloatStyle style, uint32_t radix, bool upperCase, uint32_t capacity) {
			/* compute the exponent, which can be off by one due to it being computed on the largest potential value, based
			*	on the exponent, but the logarithm might be imprecise (cannot overflow as the value can at most shrink) */
			int32_t flExponent = int32_t(std::ceil(rawExponent / detail::LogBase2[radix]));

			/* decrease the exponent already by one, as the upcoming test for the first digits to
			*	correct the exponent would otherwise have to perform an additional multiplication */
			--flExponent;

			/* setup the numerator and denominator to be used for the digit generation (one of the exponents will be positive */
			detail::LargeInt<Units> numerator = detail::LargeLoad<Units>(flMantissa, capacity), denominator = detail::LargeLoad<Units>(1, capacity);
			rawExponent -= std::numeric_limits<Type>::digits;
			if (rawExponent > 0)
				numerator = detail::LargePow<Units>(numerator, 2, uint32_t(rawExponent));
			else if (rawExponent < 0)
				denominator = detail::LargePow<Units>(denominator, 2, uint32_t(-rawExponent));
			if (flExponent > 0)
				denominator = detail::LargePow<Units>(denominator, radix, uint32_t(flExponent));
			else if (flExponent < 0)
				numerator = detail::LargePow<Units>(numerator, radix, uint32_t(-flExponent));

			/* digit-state to hold all of the produced digits (already insert the
			*	intermediate digits, which are produced while correcting the exponent) */
			uint32_t digits[32] = { 0 };
			uint32_t next = detail::DigitsIn32Bit[radix];

			/* produce the first digits to check if the exponent needs to be corrected (no need
			*	for initial multiplication, as the exponent has already been decreased by one) */
			uint32_t expPatchTest = detail::LargeDiv<Units>(numerator, denominator);
			if (expPatchTest != 0) {
				flExponent += (expPatchTest >= radix ? 2 : 1);

				/* extract the already produced digits */
				digits[--next] = (expPatchTest % radix);
				if (expPatchTest >= radix)
					digits[--next] = (expPatchTest / radix);
			}

			/* check whether or not the scientific mode should be used */
			bool scientific = (style == str::FloatStyle::scientificShort || style == str::FloatStyle::scientificTrim || style == str::FloatStyle::scientificFull);
			if (style == str::FloatStyle::generalShort || style == str::FloatStyle::generalTrim || style == str::FloatStyle::generalFull)
				scientific = (flExponent >= totalDigits || -flExponent > totalDigits);

			/* setup the visual properties of the string formatting from the style */
			intptr_t digitsBeforePoint = (scientific ? 1 : std::max<intptr_t>(1, flExponent));
			const char32_t* digitSet = (upperCase ? detail::DigitUpper : detail::DigitLower);

			/* produce the given number of digits and keep track of trailing lowest/highest digits, in order to remove trailing nulls/handle
			*	rounding (initialize the state to consider the initial nulls to be added to the output as already encountered) */
			uint32_t lastDigit = 0, roundingDigit = 0;
			intptr_t delayed = (scientific || flExponent > 0 ? 0 : intptr_t(flExponent - 1));
			for (intptr_t i = 0;; ++i) {
				/* check if the next digits need to be computed and extract the next digit to be used */
				if (next >= detail::DigitsIn32Bit[radix]) {
					/* perform the large multiply-division */
					detail::LargeMul<Units>(numerator, detail::MagnitudeIn32Bit[radix]);
					uint32_t intDigits = detail::LargeDiv<Units>(numerator, denominator);

					/* check if the remaining digits are null */
					if (intDigits == 0 && numerator.size == 0 && delayed < 0) {
						delayed -= (totalDigits - i);
						roundingDigit = 0;
						break;
					}

					/* extract the digits from the integer */
					while (next > 0) {
						digits[--next] = intDigits % radix;
						intDigits /= radix;
					}
				}
				uint32_t digit = digits[next++];

				/* check if this is the last digit in which case it will only be used for rounding */
				if (i >= totalDigits) {
					roundingDigit = digit;
					break;
				}

				/* check if a null-digit has been encountered, in which case all previously delayed digits can be flushed */
				if (digit == 0) {
					/* check if a last-digit exists to be flushed and write it out and the chain of max-digits */
					if (delayed >= 0) {
						if (i > 0)
							detail::FlushFloatDigits(sink, digitSet[lastDigit], 1, digitsBeforePoint);
						detail::FlushFloatDigits(sink, digitSet[radix - 1], delayed, digitsBeforePoint);
						delayed = -1;
					}
					else
						--delayed;
				}

				/* check if a maximum digit has been encountered, in which case a new chain with a cached last-value can be started */
				else if (digit >= radix - 1 && (i > 0 || delayed < 0)) {
					/* check if the last chain was a chain of nulls */
					if (delayed < 0) {
						detail::FlushFloatDigits(sink, U'0', -delayed - 1, digitsBeforePoint);
						lastDigit = 0;
						delayed = 1;
					}
					else
						++delayed;
				}

				/* flush the last digits and keep the current digit as last digit */
				else {
					if (delayed < 0)
						detail::FlushFloatDigits(sink, U'0', -delayed, digitsBeforePoint);
					else {
						if (i > 0)
							detail::FlushFloatDigits(sink, digitSet[lastDigit], 1, digitsBeforePoint);
						detail::FlushFloatDigits(sink, digitSet[radix - 1], delayed, digitsBeforePoint);
					}
					lastDigit = digit;
					delayed = 0;
				}
			}

			/* check if the value should be rounded up or down (trivial if last digit is not exactly 0.5,
			*	otherwise it will be decided based on the last digit being odd/the remainder being null) */
			bool roundValueUp = false;
			if (2 * roundingDigit > radix)
				roundValueUp = true;
			else if (2 * roundingDigit == radix) {
				/* check if the remainder of the tail is zero */
				while (next < detail::DigitsIn32Bit[radix] && digits[next] == 0)
					++next;
				bool tailIsZero = (numerator.size == 0 && next == detail::DigitsIn32Bit[radix]);

				/* check if the value should be rounded */
				uint32_t lowerMantissa = (delayed == 0 ? lastDigit : (delayed > 0 ? radix - 1 : 0));
				roundValueUp = detail::RoundToNearestRoundUp(lowerMantissa, (uint64_t(0x01) << 63) | (tailIsZero ? 0x00 : 0x01));
			}

			/* add the delayed non-zero digits and apply the rounding */
			if (delayed >= 0) {
				if (roundValueUp) {
					/* check for the edge-case of all digits being the highest-digit, in which case the rounding will be carried
					*	out of the digits (can only occur for fixed-style floats with at least one digit on the integer side) */
					if (lastDigit == radix - 1) {
						++flExponent;
						if (!scientific)
							++digitsBeforePoint;
						lastDigit = 0;
					}

					/* write the last value out (increased by one) and update the delayed-count as it now contains all flipped nulls */
					detail::FlushFloatDigits(sink, digitSet[lastDigit + 1], 1, digitsBeforePoint);
					delayed = -delayed;
				}

				/* no rounding needs to be performed, in which case all values can just be flushed */
				else {
					detail::FlushFloatDigits(sink, digitSet[lastDigit], 1, digitsBeforePoint);
					detail::FlushFloatDigits(sink, digitSet[radix - 1], delayed, digitsBeforePoint);
				}
			}
			else if (roundValueUp) {
				/* the last value was part of a chain of nulls, in which case the last digit is only be affected by the rounding */
				detail::FlushFloatDigits(sink, U'0', -delayed - 1, digitsBeforePoint);
				detail::FlushFloatDigits(sink, U'1', 1, digitsBeforePoint);
				delayed = 0;
			}

			/* check if remaining nulls need to be written out (as they have either been delayed or are missing to the decimal point) */
			intptr_t nulls = 0;
			if (delayed < 0 && (style == str::FloatStyle::fixedFull || style == str::FloatStyle::generalFull || style == str::FloatStyle::scientificFull))
				nulls = -delayed;
			if (nulls <= digitsBeforePoint && style != str::FloatStyle::fixedShort && style != str::FloatStyle::generalShort && style != str::FloatStyle::scientificShort)
				nulls = digitsBeforePoint + 1;
			else if (!scientific && nulls < digitsBeforePoint)
				nulls = digitsBeforePoint;
			detail::FlushFloatDigits(sink, U'0', nulls, digitsBeforePoint);

			/* check if the exponent needs to be written out */
			if (!scientific)
				return;
			str::CodepointTo<err::Nothing>(sink, U"eE^^"[(upperCase ? 0x01 : 0x00) + (radix > 12 ? 0x02 : 0x00)], 1);
			str::CodepointTo<err::Nothing>(sink, --flExponent < 0 ? U'-' : U'+', 1);
			flExponent = (flExponent < 0 ? -flExponent : flExponent);

			/* write the exponent to the sink */
			detail::PrintInteger<uint32_t>(sink, uint32_t(flExponent), radix, 2, false, upperCase);
		}

		template <class Type>
		constexpr void PrintFloat(auto& sink, Type num, str::FloatStyle style, size_t radix, size_t precision, bool addPrefix, bool upperCase, bool hexFloat) {
			using Limits = std::numeric_limits<Type>;

			/* validate the float-type is usable (to ensure the exponent cannot trigger any overflow in the power-operations) */
			static_assert(Limits::digits <= 64, "Type must have mantissa smaller than/equal to 64-bit");
			static_assert(Limits::radix == 2, "Type must use exponent-base two");
			static_assert(Limits::min_exponent >= -detail::LargeIntSafeExponentLimit, "Type must have an exponent safe to be handled by large integers");
			static_assert(Limits::max_exponent <= detail::LargeIntSafeExponentLimit, "Type must have an exponent safe to be handled by large integers");
			static_assert(sizeof(int) >= sizeof(int32_t), "Large integer requires the integer type to be at least 32-bit");

			/* check if the value is negative and extract the sign */
			if (num < 0) {
				str::CodepointTo<err::Nothing>(sink, U'-', 1);
				num = -num;
			}

			/* check if a prefix should be added */
			if (addPrefix) {
				char32_t c = (upperCase ? detail::PrefixUpper : detail::PrefixLower)[radix];
				if (c != U'_') {
					str::CodepointTo<err::Nothing>(sink, U'0', 1);
					str::CodepointTo<err::Nothing>(sink, c, 1);
				}
			}

			/* check if the value is a special value and if a special-char needs to be inserted to differentiate it from ordinary numbers */
			if (!std::isfinite(num)) {
				if (radix > detail::MaxRadixBeforeSpecialChar)
					str::CodepointTo<err::Nothing>(sink, U'#', 1);
				if (std::isinf(num))
					str::FastcodeAllTo<err::Nothing>(sink, upperCase ? U"INF" : U"inf");
				else
					str::FastcodeAllTo<err::Nothing>(sink, upperCase ? U"NAN" : U"nan");
				return;
			}

			/* check if the precision needs to be default-initialized and compute it as the number of digits covered by the mantissa-bits
			*	and otherwise limit the precision to allow it to not overflow a signed intptr_t multiplied by at least 4 */
			intptr_t totalDigits = 0;
			if (precision > 0)
				totalDigits = intptr_t(std::min<size_t>(precision, (std::numeric_limits<intptr_t>::max() / 16) + 1));
			else
				totalDigits = std::max<intptr_t>(1, intptr_t(std::ceil(Limits::digits / detail::LogBase2[radix])));

			/* check if a hex-float with general style has been selected, which will be defaulted to scientific, and fixed-styles will simply result in radix=16 floats */
			if (hexFloat) {
				if (style == str::FloatStyle::generalFull)
					style = str::FloatStyle::scientificFull;
				else if (style == str::FloatStyle::generalTrim)
					style = str::FloatStyle::scientificTrim;
				else if (style == str::FloatStyle::generalShort)
					style = str::FloatStyle::scientificShort;
				else if (style == str::FloatStyle::fixedFull || style == str::FloatStyle::fixedShort || style == str::FloatStyle::fixedTrim) {
					hexFloat = false;
					radix = 16;
				}
			}

			/* decompose the float into its exponent and mantissa as a pure integer (the exponent must fit into a 32-bit float, as the type cannot have exponent outside of the safe-are) */
			int32_t flExponent = 0;
			uint64_t flMantissa = 0;
			if constexpr (Limits::digits < 64) {
				int exp = 0;
				flMantissa = uint64_t(std::frexp(num, &exp) * Type(uint64_t(0x01) << Limits::digits));
				flExponent = int32_t(exp);
			}
			else {
				int exp = 0;
				flMantissa = uint64_t(std::frexp(num, &exp) * Type(uint64_t(0x01) << 32) * Type(uint64_t(0x01) << 32));
				flExponent = int32_t(exp);
			}

			/* check if the value is null and print the null-representation as fast way out */
			if (flMantissa == 0) {
				/* produce the null-string */
				str::CodepointTo<err::Nothing>(sink, U'0', 1);
				if (style == str::FloatStyle::generalShort || style == str::FloatStyle::fixedShort)
					return;

				/* add the decimal point and chain of remaining nulls */
				if (style != str::FloatStyle::scientificShort) {
					str::CodepointTo<err::Nothing>(sink, U'.', 1);
					if (totalDigits <= 1 || style == str::FloatStyle::generalTrim || style == str::FloatStyle::fixedTrim || style == str::FloatStyle::scientificTrim)
						totalDigits = 2;
					str::CodepointTo<err::Nothing>(sink, U'0', totalDigits - 1);
				}

				/* check if an exponent needs to be added */
				if (style == str::FloatStyle::scientificShort || style == str::FloatStyle::scientificTrim || style == str::FloatStyle::scientificFull) {
					str::CodepointTo<err::Nothing>(sink, U"eE^^pPpP"[(upperCase ? 0x01 : 0x00) + (radix > 12 ? 0x02 : 0x00) + (hexFloat ? 0x04 : 0x00)], 1);
					str::CodepointTo<err::Nothing>(sink, U'+', 1);
					str::CodepointTo<err::Nothing>(sink, U'0', 2);
				}
				return;
			}

			/* check if a hex-string should be produced and dispatch the call to the handler */
			if (hexFloat) {
				detail::PrintHexFloat(sink, totalDigits, flExponent - Limits::digits, flMantissa, style, upperCase);
				return;
			}

			/* select the resolution to be used (add two additional data-packages for scratch-pad and imprecisions/accumulating errors,
			*	and make it dependent on the digits and rounding-digit, as well as an imprecision-correctuion for larger exponents) */
			uint32_t expBits = uint32_t(detail::BitsForNumber(std::abs(flExponent))) * 2;
			uint32_t digitsBits = uint32_t(std::ceil((totalDigits + 1) * detail::LogBase2[radix]));
			uint32_t dataPackages = ((expBits + digitsBits + 31) / 32) + 2;
			if (dataPackages <= 4)
				detail::PrintNormalFloat<Type, 4>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 6)
				detail::PrintNormalFloat<Type, 6>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 8)
				detail::PrintNormalFloat<Type, 8>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 12)
				detail::PrintNormalFloat<Type, 12>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 16)
				detail::PrintNormalFloat<Type, 16>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 32)
				detail::PrintNormalFloat<Type, 32>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 64)
				detail::PrintNormalFloat<Type, 64>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 96)
				detail::PrintNormalFloat<Type, 96>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else if (dataPackages <= 128)
				detail::PrintNormalFloat<Type, 128>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, 0);
			else
				detail::PrintNormalFloat<Type, 0>(sink, totalDigits, flExponent, flMantissa, style, uint32_t(radix), upperCase, dataPackages);
		}
	}

	/*
	*	Parse the next integer/float with an optional leading sign and optional prefix for the radix (prefixes: [0b/0q/0o/0d/0x])
	*	Use the radix for the mantissa, exponent, and base of floats. (Use str::HexFloat-radix to parse hex-floats)
	*		=> Fox hex-floats, overwrite/detect-prefix modes will be mapped to the other modes
	*
	*	Returns maximum possible value on range-errors (sign will be preserved)
	*
	*	r: any valid digit for given radix (lower or upper case)
	*	integer: [\+\-]?(0[bBqQoOdDxX])?r+
	*	float: [\+\-]?(0[bBqQoOdDxX])?(r*(r\.|\.r)r*)([eEpP^][\+\-]?r+)?
	*	hex-floats: [\+\-]?(0[xX])?(h*(h\.|\.h)h*)([pP][\+\-]?d+)?
	*		- with h any hex-digit, and d any decimal-digit
	*/
	template <str::IsNumber Type>
	constexpr str::ParsedNum ParseNumTo(const str::IsStr auto& source, Type& num, size_t radix = 10, str::PrefixMode prefix = str::PrefixMode::none) {
		using ChType = str::StringChar<decltype(source)>;
		str::ParsedNum out{};

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty()) {
			num = 0;
			return out;
		}

		/* ensure the radix is valid (set it even for hex-floats in order to ensure the prefix is parsed correctly) */
		bool hexFloat = (radix == str::HexFloat);
		if (std::is_floating_point_v<Type> && hexFloat) {
			radix = 16;
			if (prefix == str::PrefixMode::overwrite)
				prefix = str::PrefixMode::optional;
			else if (prefix == str::PrefixMode::detect)
				prefix = str::PrefixMode::mandatory;
		}
		else if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;

		/* parse the sign and prefix and check if the parsed prefix is valid */
		detail::PrefixParseOut prefixParsed = detail::ParseSignAndPrefix<Type, ChType>(view, prefix == str::PrefixMode::none);
		if (prefix == str::PrefixMode::mandatory || prefix == str::PrefixMode::detect) {
			if (prefixParsed.prefixConsumed == 0 || (prefix == str::PrefixMode::mandatory && prefixParsed.radix != radix)) {
				out.result = str::NumResult::invalid;
				num = 0;
				return out;
			}

			/* radix cannot be null, as prefix-consumed must be greater than zero */
			radix = prefixParsed.radix;
		}

		/* check if the prefix might change the radix or check that it matches the actual radix */
		else if (prefix == str::PrefixMode::overwrite && prefixParsed.radix != 0)
			radix = prefixParsed.radix;
		else if (prefixParsed.radix != radix)
			prefixParsed.prefixConsumed = 0;
		size_t prefixSize = prefixParsed.prefixConsumed + prefixParsed.signConsumed;

		/* parse the integer or float and add the sign/prefix consumed characters to the overall consumed characters */
		if constexpr (std::is_integral_v<Type>)
			out = detail::ParseInteger<Type, ChType>(num, view.substr(prefixSize), radix, prefixParsed.negative);
		else
			out = detail::ParseFloat<Type, ChType>(num, view.substr(prefixSize), radix, prefixParsed.negative, hexFloat);
		out.consumed += prefixSize;
		return out;
	}

	/* parse the next integer/float using str::ParseNumTo and return it as one structure */
	template <str::IsNumber Type>
	constexpr str::ParsedNumValue<Type> ParseNum(const str::IsStr auto& source, size_t radix = 10, str::PrefixMode prefix = str::PrefixMode::none) {
		Type num = 0;
		auto [consumed, result] = str::ParseNumTo<Type>(source, num, radix, prefix);
		return str::ParsedNumValue<Type>{ num, consumed, result };
	}

	/* parse the entire string as integer/float using str::ParseNumTo and return the value, if the
	*	string was fully consumed and fit into the type, and otherwise return the [otherwise] value */
	template <str::IsNumber Type>
	constexpr Type ParseNumAll(const str::IsStr auto& source, Type otherwise = std::numeric_limits<Type>::max(), size_t radix = 10, str::PrefixMode prefix = str::PrefixMode::none) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };

		/* parse the number as far as possible */
		Type num = 0;
		auto [consumed, result] = str::ParseNumTo<Type>(view, num, radix, prefix);

		/* check if no error occurred and the entire string has been consumed */
		if (result == str::NumResult::valid && consumed == view.size())
			return num;
		return otherwise;
	}

	/* check for the prefix on the potentially signed string (i.e. leading +/-, but - only if type permits)
	*	and return defRadix for invalid prefixes or the radix (prefixes: [0b/0q/0o/0d/0x]) */
	template <str::IsNumber Type = unsigned>
	constexpr size_t PeekPrefix(const str::IsStr auto& source, size_t defRadix = 10) {
		using ChType = str::StringChar<decltype(source)>;

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return defRadix;

		/* parse the sign and prefix and return the parsed radix */
		detail::PrefixParseOut out = detail::ParseSignAndPrefix<Type, ChType>(view, false);
		return (out.radix == 0 ? defRadix : out.radix);
	}

	/* return an empty local string or a string containg the prefix for the corresponding radix (hex-float radix will result in radix-16) */
	template <str::IsChar ChType>
	constexpr str::Local<ChType, 2> MakePrefix(size_t radix, bool upperCase = false) {
		str::Local<ChType, 2> out;
		if (radix == str::HexFloat)
			radix = 16;
		else if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;

		/* lookup the prefix-character and check if it exists */
		char32_t c = (upperCase ? detail::PrefixUpper : detail::PrefixLower)[radix];
		if (c == U'_')
			return out;

		/* add the characters to the string and return it */
		out.push_back(U'0');
		out.push_back(c);
		return out;
	}

	/* print integer with optional leading [-] for the given radix to the sink and return the sink */
	constexpr auto& IntTo(str::IsSink auto&& sink, const str::IsInteger auto& num, size_t radix = 10, str::NumStyle numStyle = str::NumStyle::lower) {
		using NumType = std::remove_cvref_t<decltype(num)>;

		/* ensure the radix is valid and print the integer */
		if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;
		bool addPrefix = (numStyle == str::NumStyle::lowerWithPrefix || numStyle == str::NumStyle::upperWithPrefix);
		bool upperCase = (numStyle == str::NumStyle::upper || numStyle == str::NumStyle::upperWithPrefix);
		detail::PrintInteger<NumType>(sink, num, radix, 0, addPrefix, upperCase);
		return sink;
	}

	/* print float with optional leading [-] for the given radix to the sink and return the sink (use str::HexFloat-radix to print hex-floats) */
	constexpr auto& FloatTo(str::IsSink auto&& sink, const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, str::NumStyle numStyle = str::NumStyle::lower) {
		using NumType = std::remove_cvref_t<decltype(num)>;

		/* check if a hex-float has been requested and ensure the radix is valid */
		bool hexFloat = (radix == str::HexFloat);
		if (hexFloat)
			radix = 16;
		else if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;
		bool addPrefix = (numStyle == str::NumStyle::lowerWithPrefix || numStyle == str::NumStyle::upperWithPrefix);
		bool upperCase = (numStyle == str::NumStyle::upper || numStyle == str::NumStyle::upperWithPrefix);
		detail::PrintFloat<NumType>(sink, num, style, radix, precision, addPrefix, upperCase, hexFloat);
		return sink;
	}

	/* write the integer to an object of the given sink-type using str::IntTo and return it */
	template <str::IsSink SinkType>
	constexpr SinkType Int(const str::IsInteger auto& num, size_t radix = 10, str::NumStyle numStyle = str::NumStyle::lower) {
		SinkType sink{};
		str::IntTo(sink, num, radix, numStyle);
		return sink;
	}

	/* write the float to an object of the given sink-type using str::FloatTo and return it */
	template <str::IsSink SinkType>
	constexpr SinkType Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, str::NumStyle numStyle = str::NumStyle::lower) {
		SinkType sink{};
		str::FloatTo(sink, num, style, precision, radix, numStyle);
		return sink;
	}
}
