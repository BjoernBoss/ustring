#pragma once

#include "str-common.h"
#include "str-convert.h"

#include <type_traits>
#include <cinttypes>
#include <limits>
#include <cmath>
#include <utility>
#include <tuple>
#include <vector>
#include <algorithm>

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
	enum class FloatStyle : uint8_t {
		general,
		generalFull,
		fixed,
		fixedShort,
		scientific,
		scientificShort
	};

	template <str::IsNumber Type>
	struct NumParseOut {
		Type value = 0;
		size_t consumed = 0;
		str::NumResult result = str::NumResult::empty;
	};

	/* valid radix range as well as radix indicating float is a hex-float */
	static constexpr size_t MinRadix = 2;
	static constexpr size_t MaxRadix = 36;
	static constexpr size_t HexFloat = (size_t)-1;

	namespace detail {
		template <std::unsigned_integral Type, size_t Units>
			requires(Units > 0)
		class BitStream {
		public:
			using BtType = std::make_signed_t<Type>;
			static constexpr BtType UnitBits = sizeof(Type) * 8;
			static constexpr BtType TotalBits = Units * UnitBits;

		private:
			Type pBuffer[Units] = { 0 };

		public:
			void bset(BtType bt) {
				if (bt >= 0 && bt < TotalBits)
					pBuffer[bt / UnitBits] |= (Type(0x1) << (bt % UnitBits));
			}
			void bclear(BtType bt) {
				if (bt >= 0 && bt < TotalBits)
					pBuffer[bt / UnitBits] &= ~(Type(0x1) << (bt % UnitBits));
			}
			bool btest(BtType bt) const {
				if (bt >= TotalBits || bt < 0)
					return false;
				return ((pBuffer[bt / UnitBits] >> (bt % UnitBits)) & 0x01);
			}
			template <std::unsigned_integral TType>
			void set(BtType bt, TType v) {
				static constexpr BtType TTypeBits = sizeof(TType) * 8;
				static_assert(TTypeBits <= UnitBits, "setter-type must not be larger than unit-type");

				/* check if the access is out-of-bounds (otherwise full must be a valid index) */
				if (bt >= TotalBits || bt + TTypeBits <= 0)
					return;

				/* check if the address is negative set the lower portion accordingly */
				if (bt < 0) {
					pBuffer[0] = (pBuffer[0] & (~Type(0) << (TTypeBits + bt))) | Type(v >> -bt);
					return;
				}
				BtType full = (bt / UnitBits), off = (bt % UnitBits);

				/* check if a full group is being set */
				if (off == 0) {
					if constexpr (TTypeBits == UnitBits)
						pBuffer[full] = v;
					else
						pBuffer[full] = (pBuffer[full] & (~Type(0) << TTypeBits)) | v;
					return;
				}

				/* patch the lower value and the upper value, if not out of bounds */
				pBuffer[full] = (pBuffer[full] & ~(~Type(0) << off)) | (Type(v) << off);
				if ((TTypeBits == UnitBits || off + TTypeBits > UnitBits) && ++full < Units)
					pBuffer[full] = (pBuffer[full] & (~Type(0) << (off + TTypeBits - UnitBits))) | (Type(v) >> (UnitBits - off));
			}
			template <std::unsigned_integral TType>
			TType get(BtType bt) const {
				static constexpr BtType TTypeBits = sizeof(TType) * 8;
				static_assert(TTypeBits <= UnitBits, "getter-type must not be larger than unit-type");

				/* check if the access is out-of-bounds (otherwise full must be a valid index) */
				if (bt >= TotalBits || bt + TTypeBits <= 0)
					return 0;
				BtType use = std::max<BtType>(bt, 0);

				/* check if a full group is being fetched (and adjust for negative requests) */
				BtType full = (use / UnitBits), off = (use % UnitBits);
				if (off == 0) {
					if (bt >= 0)
						return TType(pBuffer[full]);
					return TType(pBuffer[full] << -bt);
				}

				/* fetch the two components and construct the requested value (bt must be
				*	greater or equal to zero, as offset would otherwise have been null) */
				Type l = pBuffer[full++];
				Type h = (((TTypeBits == UnitBits || off + TTypeBits > UnitBits) && full < Units) ? pBuffer[full] : 0);
				return TType((h << (UnitBits - off)) | (l >> off));
			}
			BtType upper() const {
				BtType count = 0, index = Units - 1;

				/* count all full null groups */
				while (pBuffer[index] == 0) {
					count += UnitBits;
					if (index-- == 0)
						return count;
				}

				/* count the remaining bits (there must be a non-zero bit) */
				BtType bit = UnitBits;
				while (((pBuffer[index] >> --bit) & 0x01) == 0)
					++count;
				return count;
			}
			BtType lower() const {
				BtType count = 0, index = 0;

				/* count all full null-groups */
				while (pBuffer[index] == 0) {
					count += UnitBits;
					if (++index == Units)
						return count;
				}

				/* count the remaining bits (there must be a non-zero bit) */
				BtType bit = 0;
				while (((pBuffer[index] >> bit++) & 0x01) == 0)
					++count;
				return count;
			}
		};

		/* positive non-null float (exponent type equal to signed l-type) */
		template <std::unsigned_integral SType, std::unsigned_integral LType, size_t Units>
			requires(Units > 0)
		class LargeFloat {
		public:
			using ExpType = std::make_signed_t<LType>;
			static constexpr ExpType SmallBits = sizeof(SType) * 8;
			static constexpr ExpType LargeBits = sizeof(LType) * 8;
			static constexpr ExpType TotalBits = LargeBits * Units;

		private:
			static_assert(SmallBits * 2 == LargeBits, "Small type must exactly be half the size of the large type");
			using ThisType = detail::LargeFloat<SType, LType, Units>;

		private:
			detail::BitStream<LType, Units> pMantissa;
			ExpType pExponent = 0;

		public:
			LargeFloat() = default;
			explicit LargeFloat(LType n) {
				pMantissa.set(0, n);
			}

		private:
			ThisType fMul(const ThisType& r) const {
				static constexpr size_t MaxCarries = Units * 4;
				detail::BitStream<LType, Units * 2> prod{};
				SType carry[MaxCarries] = { 0 };

				/* multiply the mantissas together */
				size_t lCarryIndex = 0;
				for (ExpType lo = 0; lo < TotalBits; lo += SmallBits) {
					size_t rCarryIndex = lCarryIndex++;

					/* check if the entire operation can be skipped */
					LType lVal = LType(pMantissa.get<SType>(lo));
					if (lVal == 0)
						continue;

					/* multiply the current value of left with all values from the right and update the carry counts */
					for (ExpType ro = 0; ro < TotalBits; ro += SmallBits) {
						size_t carryIndex = rCarryIndex++;

						/* check if the component is null, in which case nothing needs to be done */
						LType rVal = LType(r.pMantissa.get<SType>(ro));
						if (rVal == 0)
							continue;

						/* extract the current value and perform the multiplication */
						LType old = prod.get<LType>(lo + ro);
						LType next = old + lVal * rVal;

						/* update the result and check for an overflow (cannot carry out of the last group) */
						prod.set<LType>(lo + ro, next);
						if (old > next && carryIndex + 2 < MaxCarries)
							++carry[carryIndex + 2];
					}
				}

				/* apply all carry counts to the result (can trigger carries themselves) */
				for (size_t i = 0; i < MaxCarries; ++i) {
					if (carry[i] == 0)
						continue;
					SType old = prod.get<SType>(i * SmallBits);
					SType next = old + carry[i];
					prod.set<SType>(i * SmallBits, next);

					/* check if another overflow has occurred (cannot leave the last group) */
					if (old > next)
						++carry[i + 1];
				}

				/* count the number of nulls in the result to be skipped (to ensure as much information as
				*	possible is kept) and round the result depending on the highest bit of the cut part */
				ExpType nulls = prod.upper();
				if (prod.btest(TotalBits - nulls - 1)) {
					for (ExpType i = TotalBits - nulls; i < 2 * TotalBits; i += LargeBits) {
						LType old = prod.get<LType>(i);
						prod.set<LType>(i, ++old);
						if (old != 0)
							break;
					}
				}

				/* initialize the output and copy the product into the mantissa */
				ThisType out{};
				out.pExponent = pExponent + r.pExponent + TotalBits - nulls;
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					out.pMantissa.set<LType>(i, prod.get<LType>(TotalBits - nulls + i));
				return out;
			}
			ThisType fDiv(const ThisType& r) const {
				static constexpr ExpType AdditionalBits = TotalBits + LargeBits;
				detail::BitStream<LType, Units * 2 + 1> dividend{}, result{};
				detail::BitStream<LType, Units> divisor{};

				/* setup the dividend mantissa with the upper bits of the dividend and highest bit set */
				ExpType lOff = pMantissa.upper();
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					dividend.set<LType>(AdditionalBits + i, pMantissa.get<LType>(i - lOff));

				/* setup the divisor with the highest bit set */
				ExpType rOff = r.pMantissa.upper();
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					divisor.set<LType>(i, r.pMantissa.get<LType>(i - rOff));

				/* check if the lower bits of the divisor are null, in which case the increase by one is not necessary and otherwise
				*	ensure each partial division uses a divisor increased by one to ensure (dividend >= res * divisor) at all times */
				LType lowerOffset = (divisor.lower() < (TotalBits - SmallBits) ? 1 : 0);

				/* perform the long division */
				ExpType nullBits = 0;
				while (nullBits <= TotalBits + SmallBits) {
					ExpType btIndex = (TotalBits + AdditionalBits - LargeBits - nullBits);

					/* divide the upper large-number of bits of the dividend by the upper small-number of bits of the divisor */
					LType res = dividend.get<LType>(btIndex) / LType(divisor.get<SType>(TotalBits - SmallBits) + lowerOffset);

					/* add the result to the output and carry any overflow through (cannot leave the result-mantissa) */
					LType old = result.get<LType>(btIndex);
					LType next = old + res;
					result.set<LType>(btIndex, next);
					if (old > next) {
						for (ExpType i = btIndex + LargeBits; i < TotalBits; i += LargeBits) {
							old = result.get<LType>(i);
							result.set<LType>(i, ++old);
							if (old != 0)
								break;
						}
					}

					/* perform the mul-sub on the dividend (borrow cannot carry out, as guaranteed by the lower-offset) */
					uint8_t borrow = 0;
					for (ExpType i = 0; i < TotalBits; i += SmallBits) {
						bool lowBorrow = ((borrow >>= 1) & 0x01);
						bool highBorrow = (borrow & 0x02);
						borrow &= ~0x03;

						/* divide the multipication into two steps (as large result but [small]x[small] multiplication) */
						LType fst = LType(SType(res)) * LType(divisor.get<SType>(i));
						LType snd = LType(SType(res >> SmallBits)) * LType(divisor.get<SType>(i));

						/* subtract the first value from the dividend */
						if (fst != 0 || lowBorrow) {
							LType old = dividend.get<LType>(AdditionalBits - SmallBits - nullBits + i);
							LType next = old - fst - (lowBorrow ? 1 : 0);
							dividend.set<LType>(AdditionalBits - SmallBits - nullBits + i, next);
							borrow |= (next > old ? 0x04 : 0x00);
						}

						/* subtract the second value from the dividend */
						if (snd != 0 || highBorrow) {
							LType old = dividend.get<LType>(AdditionalBits - nullBits + i);
							LType next = old - snd - (highBorrow ? 1 : 0);
							dividend.set<LType>(AdditionalBits - nullBits + i, next);
							borrow |= (next > old ? 0x08 : 0x00);
						}
					}

					/* compute the new number of null-bits (will implicitly end if the dividend becomes zero, i.e. perfect division) */
					nullBits = dividend.upper();
				}

				/* count the number of leading nulls to keep as much information as possible in the result */
				ExpType nulls = result.upper();

				/* round the result (in case of the division not being perfect, add one to the result to cleanly stop infinite repeating sequences) */
				if (nullBits < AdditionalBits + TotalBits) {
					for (ExpType i = AdditionalBits - nulls; i < AdditionalBits + TotalBits; i += LargeBits) {
						LType old = result.get<LType>(i);
						result.set<LType>(i, ++old);
						if (old != 0)
							break;
					}
				}

				/* initialize the output and copy the result to the mantissa  */
				ThisType out{};
				out.pExponent = (pExponent - lOff) - (r.pExponent - rOff) - TotalBits + (SmallBits - nulls);
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					out.pMantissa.set<LType>(i, result.get<LType>(AdditionalBits - nulls + i));
				return out;
			}
			detail::BitStream<LType, Units + 1> fMulSingle(LType r) const {
				static constexpr size_t MaxCarries = 4;
				LType l = SType(r), h = SType(r >> SmallBits);

				/* cyclic buffer for the carry counts and the result-product */
				SType carryCounts[MaxCarries] = { 0 };
				size_t carryIndex = 0;
				detail::BitStream<LType, Units + 1> prod;

				/* multiply the values together (cannot carry out of the last group) */
				for (size_t i = 0; i < TotalBits; i += SmallBits) {
					LType val = LType(pMantissa.get<SType>(i));

					/* extract the carry count to be added to this value */
					SType carry = carryCounts[carryIndex % MaxCarries];
					carryCounts[carryIndex++ % MaxCarries] = 0;

					/* perform the multiplication with the lower value */
					if (l != 0 && val != 0) {
						LType old = prod.get<LType>(i);
						LType next = old + val * l;

						/* update the value and check if an overflow has occurred */
						prod.set<LType>(i, next);
						if (old > next)
							++carryCounts[(carryIndex + 1) % MaxCarries];
					}

					/* perform the multiplication with the upper value */
					if (h != 0 && val != 0) {
						LType old = prod.get<LType>(i + SmallBits);
						LType next = old + val * h;

						/* update the value and check if an overflow has occurred */
						prod.set<LType>(i + SmallBits, next);
						if (old > next)
							++carryCounts[(carryIndex + 2) % MaxCarries];
					}

					/* apply the carry */
					if (carry > 0) {
						LType old = prod.get<LType>(i);
						LType next = old + carry;

						/* update the value and check if it itself triggered an overflow */
						prod.set<LType>(i, next);
						if (old > next)
							++carryCounts[(carryIndex + 1) % MaxCarries];
					}
				}
				return prod;
			}

		public:
			/* val * 2^exp, overflow */
			static std::pair<ThisType, bool> MulPow2(const ThisType& val, ExpType exp) {
				ThisType out{ val };
				out.pExponent += exp;
				bool overflow = (exp < 0 ? out.pExponent > val.pExponent : out.pExponent < val.pExponent);
				return { out, overflow };
			}

			/* val * base^exp, overflow */
			static std::pair<ThisType, bool> MulPow(const ThisType& val, const ThisType& base, ExpType exp) {
				ThisType out{ 1 };
				ThisType walk{ base };

				/* check for the fast way out */
				if (exp == 0)
					return { val, false };

				/* check if a division needs to be performed, in which case the power will first be computed,
				*	and then divide the value, otherwise the value can be multiplied up immediately */
				bool div = (exp < 0);
				if (div)
					exp = -exp;
				else
					out = val;

				/* square-multiply algorithm */
				while (exp > 0) {
					if (exp & 0x01) {
						ThisType next = walk.fMul(out);

						/* check if the exponent has overflown (should at most continuously grow) */
						if (out.pExponent > TotalBits && next.pExponent < -TotalBits)
							return { next, true };
						out = next;
					}
					if ((exp >>= 1) > 0)
						walk = walk.fMul(walk);
				}

				/* check if the result needs to be divided and perform the division */
				if (div)
					out = val.fDiv(out);
				return { out, false };
			}

			/* multiply val by f and return everything to left of point as integer and everything to right of the point as large-float (if too small/too large: undefined) */
			static std::pair<ThisType, LType> IntMulMod(const ThisType& val, LType f) {
				detail::BitStream<LType, Units + 1> prod = val.fMulSingle(f);

				/* extract the modulo-result (i.e. everything to the left of the fractional point) */
				LType intVal = prod.get<LType>(-val.pExponent);

				/* write the remaining bits of the product (i.e. everything to the right of the fractional point) to the result */
				ThisType out{};
				out.pExponent = -TotalBits;
				for (size_t i = 0; i < TotalBits; i += LargeBits)
					out.pMantissa.set<LType>(i, prod.get<LType>(i - TotalBits - val.pExponent));
				return { out, intVal };
			}

			/* multiply val by f and add a to it (undefined behavior for f null) */
			static ThisType MulAdd(const ThisType& val, LType f, LType a) {
				ThisType out{};

				/* check for the fast way out (i.e. value so small such that multiplied with factor still no effect on addend bits or value is null) */
				if (val.pExponent <= 2 * -TotalBits - LargeBits || (val.pExponent == 0 && val.pMantissa.upper() == TotalBits)) {
					out.pMantissa.set<LType>(0, a);
					return out;
				}

				/* perform the multiplication */
				detail::BitStream<LType, Units + 1> prod = val.fMulSingle(f);

				/* setup the exponent of the out-value to hold the most information as possible form the
				*	multiplication result and check for the fast way out by the addend being null */
				out.pExponent = val.pExponent + LargeBits - prod.upper();
				if (a == 0) {
					for (ExpType i = 0; i < TotalBits; i += LargeBits)
						out.pMantissa.set<LType>(i, prod.get<LType>(i + out.pExponent));
					return out;
				}

				/* compute the magnitude of the addend and update the output exponent to at least
				*	hold the topmost bit of the addend (a must have at least one non-zero bit) */
				ExpType aHigh = LargeBits;
				while (((a >> (aHigh - 1)) & 0x01) == 0)
					--aHigh;
				out.pExponent = std::max<ExpType>(out.pExponent, aHigh - TotalBits);

				/* perform the addition and simultaneously copy the data over (carry might overflow the last group) */
				bool carry = false;
				for (ExpType i = 0; i < TotalBits; i += LargeBits) {
					LType old = prod.get<LType>(i + out.pExponent - val.pExponent);

					/* cache the current carry-addition */
					bool addSingleCarry = carry;
					carry = false;

					/* extract the part of the addend to be added to the current bits */
					ExpType off = i + out.pExponent;
					if (off > -LargeBits && off < LargeBits) {
						LType next = old + (off < 0 ? (a << -off) : (a >> off));
						out.pMantissa.set<LType>(i, next);

						/* check if an overflow occurred */
						if (old > next)
							carry = true;

						/* check if a carry has to be added or if the upcoming step can be skipped */
						if (!addSingleCarry)
							continue;
						old = next;
					}

					/* apply the carry and copy the value to the output and check if an overflow occurred (cannot occur
					*	two at once, as the first carry implies that value has at least space for one increment) */
					if (addSingleCarry) {
						out.pMantissa.set<LType>(i, ++old);
						carry = (old == 0);
					}
				}

				/* check if a final carry has occurred (rare in mul-add loops) and apply it by shifting the mantissa to the right and setting the highest bit */
				if (carry) {
					for (size_t i = 0; i < TotalBits; i += LargeBits)
						out.pMantissa.set<LType>(i, out.pMantissa.get<LType>(i + 1));
					out.pMantissa.bset(TotalBits - 1);
					++out.pExponent;
				}
				return out;
			}

			/* return closest float describing the correponsing value, overflow/underflow */
			template <class FlType>
			static std::pair<FlType, int8_t> ReadFloat(const ThisType& val) {
				static_assert(LargeBits <= std::numeric_limits<FlType>::max_exponent && std::numeric_limits<FlType>::min_exponent <= 0, "Exponent of FlType must at least be able to hold one LType");
				FlType out{};

				/* extract the highest (most relevant) bit such that the mantissa can be positioned to hold the highest bit to the right of the fractional-point */
				ExpType highest = val.pMantissa.upper();

				/* write all bits to the float (cannot overflow, and ignore underflow) */
				for (ExpType i = TotalBits - highest - LargeBits; i > -LargeBits; i -= LargeBits) {
					FlType temp = FlType(val.pMantissa.get<LType>(i));
					if (temp == 0)
						continue;

					/* scale the temporary value such that the highest bit lies to the right of the fractional-point (this
					*	ensures that the must significant bits will not trigger an underflow/overflow while writing them and
					*	std::ldexp will at most trigger an underflow, in which case the result will be rounded towards null) */
					out += std::ldexp(temp, static_cast<int>(i - TotalBits));
				}

				/* compute the final exponent to be applied to the result and check if it has overflown */
				ExpType exponent = val.pExponent + TotalBits - highest;
				if (exponent < 0 && val.pExponent > 0)
					return { out, true };

				/* check if the exponent can even fit into an integer */
				if (exponent < std::numeric_limits<int>::min() || exponent > std::numeric_limits<int>::max())
					return { FlType{}, (exponent < 0 ? -1 : 1) };

				/* apply the exponent to the result and check for a range-error */
				out = std::ldexp(out, static_cast<int>(exponent));
				if (errno != ERANGE)
					return { out, 0 };
				return { out, (exponent < 0 ? -1 : 1) };
			}
		};

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
		static constexpr uint64_t MagnitudeIn64Bit[str::MaxRadix + 1] = {
			0x0000000000000000, 0x0000000000000000, 0x4000000000000000, 0x383d9170b85ff80b,
			0x4000000000000000, 0x6765c793fa10079d, 0x41c21cb8e1000000, 0x3642798750226111,
			0x1000000000000000, 0x12bf307ae81ffd59, 0x0de0b6b3a7640000, 0x4d28cb56c33fa539,
			0x1eca170c00000000, 0x780c7372621bd74d, 0x1e39a5057d810000, 0x5b27ac993df97701,
			0x1000000000000000, 0x27b95e997e21d9f1, 0x5da0e1e53c5c8000, 0x0b16a458ef403f19,
			0x16bcc41e90000000, 0x2d04b7fdd9c0ef49, 0x5658597bcaa24000, 0x06feb266931a75b7,
			0x0c29e98000000000, 0x14adf4b7320334b9, 0x226ed36478bfa000, 0x383d9170b85ff80b,
			0x5a3c23e39c000000, 0x04e900abb53e6b71, 0x07600ec618141000, 0x0aee5720ee830681,
			0x1000000000000000, 0x172588ad4f5f0981, 0x211e44f7d02c1000, 0x2ee56725f06e5c71,
			0x41c21cb8e1000000
		};
		static constexpr uint8_t DigitsIn64Bit[str::MaxRadix + 1] = {
			00, 00, 62, 39, 31, 27, 24, 22, 20, 19, 18, 18, 17, 17, 16, 16,
			15, 15, 15, 14, 14, 14, 14, 13, 13, 13, 13, 13, 13, 12, 12, 12,
			12, 12, 12, 12, 12
		};
		static constexpr uint8_t MaxDigitsIn64BitAnyRadix = 62;
		static constexpr const char32_t* DigitLower = U"0123456789abcdefghijklmnopqrstuvwxyz";
		static constexpr const char32_t* DigitUpper = U"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		static constexpr const char32_t* ExponentMap = U"eeeeeeeeeeeeee^^^^^^^^^^^^^^^^^^^^^^";
		static constexpr const size_t MaxDigitMap = 128;
		static constexpr uint8_t CPDigitMap[detail::MaxDigitMap] = {
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
			0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
			0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0xff, 0xff, 0xff, 0xff, 0xff
		};

		struct PrefixParseOut {
			size_t radix = 0;
			size_t signConsumed = 0;
			size_t prefixConsumed = 0;
			bool negative = false;
		};
		template <class Type, class ChType, class Mode>
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
				auto [consumed, cp, result] = str::Decode<Mode>(view.substr(out.signConsumed + prefixConsumed), true);
				if (result != str::DecResult::valid)
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

		template<class Type, class ChType, class Mode>
		constexpr std::tuple<Type, size_t, bool> ParseRawInteger(const std::basic_string_view<ChType>& view, size_t radix, str::DecodeOut& dec, bool negative) {
			size_t totalConsumed = 0;
			bool overflow = false;

			/* setup the overflow detection values */
			using UType = std::make_unsigned_t<Type>;
			UType valueLimit = std::numeric_limits<Type>::max();
			if constexpr (std::is_signed_v<Type>) {
				if (negative)
					valueLimit = UType(-std::numeric_limits<Type>::min());
			}
			UType valueLastDigit = valueLimit % radix;
			valueLimit /= radix;

			/* iterate over the digits and parse them */
			UType value = 0;
			while (dec.result == str::DecResult::valid && dec.cp >= 0 && dec.cp < detail::MaxDigitMap) {
				/* check if the codepoint is a valid digit */
				size_t digit = detail::CPDigitMap[dec.cp];
				if (digit >= radix)
					break;

				/* update the value and check for an overflow */
				if (value < valueLimit || (value == valueLimit && digit <= valueLastDigit))
					value = value * radix + digit;
				else
					overflow = true;

				/* mark the characters as consumed and decode the next character */
				totalConsumed += dec.consumed;
				dec = str::Decode<Mode>(view.substr(totalConsumed), true);
			}

			/* apply the sign and return the value */
			if constexpr (std::is_signed_v<Type>)
				return { (negative ? -Type(value) : Type(value)), totalConsumed, overflow };
			else
				return { value, totalConsumed, overflow };
		}

		template<class Type, class ChType, class Mode>
		constexpr str::NumParseOut<Type> ParseInteger(const std::basic_string_view<ChType>& view, size_t radix, bool negative) {
			/* parse the raw value */
			str::DecodeOut dec = str::Decode<Mode>(view, true);
			auto [value, totalConsumed, overflow] = detail::ParseRawInteger<Type, ChType, Mode>(view, radix, dec, negative);

			/* check if an overflow occurred and setup the overflow value */
			str::NumParseOut<Type> out{};
			if (!overflow)
				out.value = value;
			else if (std::is_signed_v<Type> && negative)
				value = std::numeric_limits<Type>::min();
			else
				value = std::numeric_limits<Type>::max();

			/* finalize the output structure (string cannot be empty, or will only be empty if a prefix
			*	has already been parsed, in which case the empty string will be considered an error as well) */
			out.consumed = totalConsumed;
			if (totalConsumed == 0)
				out.result = str::NumResult::invalid;
			else if (overflow)
				out.result = str::NumResult::range;
			else
				out.result = str::NumResult::valid;
			return out;
		}

		template<class Type, class Mode>
		constexpr void PrintInteger(auto& sink, Type num, size_t radix, bool upperCase) {
			static_assert(sizeof(Type) <= 8, "Type must be smaller than/equal to 64-bit");

			/* digit map to contain all digits */
			uint8_t digits[sizeof(num) * 8] = { 0 };
			uint8_t* next = digits;

			/* check if a sign needs to be added */
			uint64_t act = 0;
			bool negative = false;
			if constexpr (std::is_signed_v<Type>) {
				if (negative = (num < 0))
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
				str::EncodeInto<Mode>(sink, U'-');

			/* write the digits out */
			const char32_t* digitSet = (upperCase ? detail::DigitUpper : detail::DigitLower);
			while (next != digits)
				str::EncodeInto<Mode>(sink, digitSet[*(--next)]);
		}

		struct MantissaOut {
			size_t consumed = 0;
			uint64_t mantissa = 0;
			uint64_t exponent = 0;
			int64_t dotOffset = 0;
			int8_t range = 0;
			bool invalid = false;
		};
		template<class ChType, class Mode>
		constexpr detail::MantissaOut ParseFloatMantissa(const std::basic_string_view<ChType>& view, size_t radix, str::DecodeOut& dec) {
			detail::MantissaOut out;

			/* setup the number of digits to be processed (maximum plus one, where the last digit is used to fill remaining bits) */
			uint64_t valueLimit = std::numeric_limits<uint64_t>::max() / radix;
			uint64_t valueLastDigit = std::numeric_limits<uint64_t>::max() % radix;

			/* parse the entire mantissa (integer component and fractional part) */
			bool inFraction = false, hasValue = false, valueClosed = false;
			while (dec.result == str::DecResult::valid && dec.cp >= 0) {
				size_t digit = 0;

				/* extract the digit or check if its the dot */
				if (dec.cp >= detail::MaxDigitMap || (digit = detail::CPDigitMap[dec.cp]) >= radix) {
					if (dec.cp != U'.' || inFraction)
						break;
					inFraction = true;
				}
				else {
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
							while (uMantissa >> (out.exponent + 32))
								++out.exponent;
							out.mantissa = (uMantissa << (32 - out.exponent)) | (uint64_t(uint32_t(lMantissa)) >> out.exponent);

							/* mark the value as closed as any additional information will not affect the accumulated mantissa */
							valueClosed = true;
						}
					}
				}

				/* decode the next character */
				dec = str::Decode<Mode>(view.substr(out.consumed += dec.consumed), true);
			}

			/* check if an invalid mantissa has been encountered (string cannot be empty, or will only be empty if a
			*	prefix has already been parsed, in which case the empty string will be considered an error as well) */
			if (out.consumed == 0)
				out.invalid = true;
			return out;
		}

		struct ExponentOut {
			size_t consumed = 0;
			int64_t exponent = 0;
			int8_t range = 0;
			bool invalid = false;
		};
		template<class ChType, class Mode>
		constexpr detail::ExponentOut ParseFloatExponent(const std::basic_string_view<ChType>& view, size_t radix, str::DecodeOut& dec) {
			detail::ExponentOut out;

			/* extract a potential sign of the exponent */
			bool expNegative = false;
			if (dec.result == str::DecResult::valid && (dec.cp == U'+' || dec.cp == U'-')) {
				expNegative = (dec.cp == U'-');
				dec = str::Decode<Mode>(view.substr(out.consumed += dec.consumed), true);
			}

			/* parse the exponent and check if an invalid exponent has been encountered */
			auto [value, consumed, overflow] = detail::ParseRawInteger<int64_t, ChType, Mode>(view.substr(out.consumed), radix, dec, expNegative);
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

		template<class Type>
		constexpr std::pair<Type, int8_t> ConstructHexFloat(uint64_t mantissa, uint64_t manExponent, int64_t dotOffset, int64_t exponent) {
			/* apply the shift to the exponent, based on the encountered fractional digits, and check if a range-error occurred */
			if (dotOffset != 0) {
				int64_t old = exponent;
				exponent += 4 * dotOffset;
				if (dotOffset < 0 ? (old <= exponent) : (old >= exponent))
					return { Type(), (dotOffset < 0 ? -1 : 1) };
			}

			/* check if the exponent can even fit into an integer */
			if (exponent < std::numeric_limits<int>::min() || exponent > std::numeric_limits<int>::max())
				return { Type(), (exponent < 0 ? -1 : 1) };
			Type value = Type(mantissa) * Type(uint64_t(0x01) << manExponent);
			value = std::ldexp(value, static_cast<int>(exponent));

			/* check if the exponent was valid */
			if (errno == ERANGE)
				return { Type(), (exponent < 0 ? -1 : 1) };
			return { value, 0 };
		}

		template<class Type>
		constexpr std::pair<Type, int8_t> ConstructNormalFloat(uint64_t mantissa, uint64_t manExponent, int64_t dotOffset, int64_t exponent, size_t radix) {
			using FlType = detail::LargeFloat<uint32_t, uint64_t, 1>;

			/* apply the shift to the exponent, based on the encountered fractional digits, and check if a range-error occurred */
			if (dotOffset != 0) {
				int64_t old = exponent;
				exponent += dotOffset;
				if (dotOffset < 0 ? (old <= exponent) : (old >= exponent))
					return { Type(), (dotOffset < 0 ? -1 : 1) };
			}

			/* construct the large-float with the corresponding shift (mantissa alone cannot overflow) and apply the exponent to it */
			auto [num, mulPowRangeError] = FlType::MulPow(FlType::MulPow2(FlType(mantissa), manExponent).first, FlType{ radix }, exponent);
			FlType flVal = num;
			if (mulPowRangeError)
				return { Type(), (exponent < 0 ? -1 : 1) };

			/* read the final float value */
			auto [value, readRangeError] = FlType::ReadFloat<Type>(flVal);
			if (readRangeError != 0)
				return { Type(), readRangeError };
			return { value, 0 };
		}

		template<class Type, class ChType, class Mode>
		constexpr str::NumParseOut<Type> ParseFloat(const std::basic_string_view<ChType>& view, size_t radix, bool negative, bool hexFloat) {
			static_assert(std::numeric_limits<Type>::digits <= 64, "Type must have mantissa smaller than/equal to 64-bit");
			static_assert(std::numeric_limits<Type>::radix == 2, "Type must use exponent-base two");
			static constexpr auto LowerPlay = (std::numeric_limits<Type>::min_exponent - std::numeric_limits<int64_t>::min());
			static constexpr auto UpperPlay = (std::numeric_limits<int64_t>::max() - std::numeric_limits<Type>::max_exponent);
			static_assert(LowerPlay >= 0 && UpperPlay >= 0, "Type must have an exponent smaller than/equal to 64-bit");
			size_t totalConsumed = 0;

			/* parse the mantissa and check if an error occurred (ignore range errors for now) */
			str::DecodeOut dec = str::Decode<Mode>(view, true);
			detail::MantissaOut mantissa = detail::ParseFloatMantissa<ChType, Mode>(view, (hexFloat ? 16 : radix), dec);
			totalConsumed += mantissa.consumed;
			if (mantissa.invalid)
				return str::NumParseOut<Type>{ (negative ? -Type(0) : Type(0)), totalConsumed, str::NumResult::invalid };

			/* check if an exponent has been detected and parse it */
			detail::ExponentOut exponent;
			if (dec.result == str::DecResult::valid && (hexFloat ? (dec.cp == U'p' || dec.cp == U'P') : (dec.cp == U'e' || dec.cp == U'E' || dec.cp == U'^'))) {
				dec = str::Decode<Mode>(view.substr(totalConsumed += dec.consumed), true);

				/* parse the exponent and check if an error occurred */
				exponent = detail::ParseFloatExponent<ChType, Mode>(view.substr(totalConsumed), (hexFloat ? 10 : radix), dec);
				totalConsumed += exponent.consumed;
				if (exponent.invalid)
					return str::NumParseOut<Type>{ (negative ? -Type(0) : Type(0)), totalConsumed, str::NumResult::invalid };
			}

			/* check if a range-error occurred */
			int8_t range = mantissa.range;
			if (range == 0)
				range = exponent.range;

			/* construct the hex-float or the normal float */
			if (range == 0) {
				std::pair<Type, int8_t> result;
				if (hexFloat)
					result = detail::ConstructHexFloat<Type>(mantissa.mantissa, mantissa.exponent, mantissa.dotOffset, exponent.exponent);
				else
					result = detail::ConstructNormalFloat<Type>(mantissa.mantissa, mantissa.exponent, mantissa.dotOffset, exponent.exponent, radix);

				/* check if a valid result has been found and return it */
				if (result.second == 0) {
					if (negative)
						result.first = -result.first;
					return str::NumParseOut<Type>{ result.first, totalConsumed, str::NumResult::valid };
				}
				range = result.second;
			}

			/* setup the range-error value and return the response */
			Type value = (range < 0 ? Type(0.0) : std::numeric_limits<Type>::infinity());
			return str::NumParseOut<Type>{ (negative ? -value : value), totalConsumed, str::NumResult::range };
		}

		template<class Mode>
		constexpr void FlushFloatDigits(auto& sink, size_t digit, size_t count, intptr_t& digitsBeforePoint, const char32_t* digitSet) {
			for (size_t j = 0; j < count; ++j) {
				if (digitsBeforePoint-- == 0)
					str::EncodeInto<Mode>(sink, U'.');
				str::EncodeInto<Mode>(sink, digitSet[digit]);
			}
		}

		template <class FlType>
		class FloatMulModDigits {
		private:
			uint8_t pDigits[detail::MaxDigitsIn64BitAnyRadix] = { 0 };
			FlType pValue;
			size_t pNext = 0;
			size_t pRadix = 0;

		public:
			FloatMulModDigits(const FlType& v, size_t r) : pValue(v), pRadix(r) {
				pNext = detail::DigitsIn64Bit[pRadix];
			}

		private:
			void fPrepareNext() {
				/* check if the next digits have to be fetched */
				if (pNext < detail::DigitsIn64Bit[pRadix])
					return;
				auto [_float, intDigits] = FlType::IntMulMod(pValue, detail::MagnitudeIn64Bit[pRadix]);
				pValue = _float;

				/* extract all of the new digits */
				while (pNext > 0) {
					pDigits[--pNext] = uint8_t(intDigits % pRadix);
					intDigits /= pRadix;
				}
			}

		public:
			uint8_t peek() {
				fPrepareNext();
				return pDigits[pNext];
			}
			uint8_t next() {
				fPrepareNext();
				return pDigits[pNext++];
			}
		};

		template<class Mode>
		constexpr void PrintHexFloat(auto& sink, intptr_t totalDigits, intptr_t flExponent, uint64_t flMantissa, size_t expDigits, bool clipTrailing, bool upperCase) {
			/* write out the first implicit 1 and patch the digit count */
			str::EncodeInto<Mode>(sink, U'1');
			--totalDigits;

			/* find the topmost bit to be to the left of the point (must exist as the mantissa cannot be null) */
			intptr_t topBit = 63;
			while (((flMantissa >> topBit) & 0x01) == 0)
				--topBit;

			/* check if the first bit past the digits is set and round the entire mantissa up */
			intptr_t bitsToDrop = (topBit - totalDigits * 4 - 1);
			if (bitsToDrop > 0 && (((flMantissa >> (bitsToDrop - 1)) & 0x01) != 0)) {
				/* remove the bits to be dropped and apply the rounding (cannot overflow out of
				*	the mantissa, as at least one bit will be dropped from the beginning) */
				flMantissa = (flMantissa >> bitsToDrop) + 1;
				flExponent += bitsToDrop;
				topBit -= bitsToDrop;

				/* check if an overflow occurred and the top bit has changed and update it accordingly */
				if (((flMantissa >> topBit) & 0x01) == 0)
					++topBit;
			}

			/* adjust the exponent based on the actual position of the point */
			flExponent += topBit;

			/* write the digits out (no need to keep track of last-digits, only of nulls) */
			const char32_t* digitSet = (upperCase ? detail::DigitUpper : detail::DigitLower);
			intptr_t delayed = 0, digitsBeforePoint = 0;
			for (intptr_t i = 0; i < totalDigits; ++i) {
				topBit -= 4;

				/* extract the next hex-digit */
				size_t digit = 0;
				if (topBit >= 0)
					digit = (flMantissa >> topBit);
				else if (topBit > -4)
					digit = (flMantissa << -topBit);

				/* check if the digit should be delayed and otherwise write it out */
				if (digit == 0)
					++delayed;
				else {
					detail::FlushFloatDigits<Mode>(sink, 0, delayed, digitsBeforePoint, digitSet);
					detail::FlushFloatDigits<Mode>(sink, (digit & 0x0f), 1, digitsBeforePoint, digitSet);
					delayed = 0;
				}
			}

			/* check if remaining nulls need to be written out */
			if (!clipTrailing)
				detail::FlushFloatDigits<Mode>(sink, 0, delayed, digitsBeforePoint, digitSet);

			/* write the exponent out */
			str::EncodeInto<Mode>(sink, upperCase ? U'P' : U'p');
			str::EncodeInto<Mode>(sink, flExponent < 0 ? U'-' : U'+');
			if (flExponent < 0)
				flExponent = -flExponent;

			/* write the exponent to a temporary buffer (cannot overflow the string-buffer) */
			str::U32Small<64> buffer;
			detail::PrintInteger<unsigned int, Mode>(buffer, static_cast<unsigned int>(flExponent), 10, upperCase);

			/* insert the required padding-nulls and the exponent-number */
			for (size_t i = buffer.size(); i < expDigits; ++i)
				str::EncodeInto<Mode>(sink, U'0');
			for (size_t i = 0; i < buffer.size(); ++i)
				str::EncodeInto<Mode>(sink, buffer[i]);
		}

		template<class Type, class Mode, size_t Units>
		constexpr void PrintNormalFloat(auto& sink, intptr_t totalDigits, intptr_t rawExponent, uint64_t flMantissa, str::FloatStyle style, size_t radix, size_t expDigits, bool upperCase) {
			using FlType = detail::LargeFloat<uint32_t, uint64_t, Units>;

			/* compute the exponent, which can be too large by one, as it is computed based on the largest
			*	potential number based on the binary exponent (cannot overflow as the value can at most shrink) */
			intptr_t flExponent = intptr_t(std::ceil((rawExponent + std::numeric_limits<Type>::digits) / detail::LogBase2[radix]));

			/* prepare the large-float to contain the decomposed float (cannot lead to any range errors)
			*	and extract the first set of digits, in order to also correct the exponent, if necessary */
			FlType flVal = FlType::MulPow(FlType::MulPow2(FlType(flMantissa), rawExponent).first, FlType(radix), -flExponent).first;
			detail::FloatMulModDigits<FlType> digits{ flVal, radix };
			if (digits.peek() == 0) {
				--flExponent;
				digits.next();
			}

			/* check if the entire raw float is an integer and compute the index as of which only
			*	nulls should be produced (to circumvent inaccuracies of the computation) */
			bool integer = (rawExponent >= 0 || (rawExponent > -64 && (flMantissa << (64 + rawExponent)) == 0));
			intptr_t digitsUntilNulls = (integer ? flExponent : totalDigits + 1);

			/* check whether or not the scientific mode should be used */
			bool scientific = (style == str::FloatStyle::scientific || style == str::FloatStyle::scientificShort);
			if (style == str::FloatStyle::general || style == str::FloatStyle::generalFull)
				scientific = (flExponent >= totalDigits || -flExponent > totalDigits);

			/* setup the visual properties of the string formatting from the style */
			intptr_t digitsBeforePoint = (scientific ? 1 : std::max<intptr_t>(1, flExponent));
			bool clipTrailing = (style == str::FloatStyle::general || style == str::FloatStyle::fixedShort || style == str::FloatStyle::scientificShort);
			const char32_t* digitSet = (upperCase ? detail::DigitUpper : detail::DigitLower);

			/* produce the given number of digits and keep track of trailing lowest/highest digits, in order to remove trailing nulls/handle
			*	rounding (initialize the state to consider the initial nulls to be added to the output as already encountered) */
			size_t lastDigit = 0;
			intptr_t delayed = (scientific || flExponent > 0 ? 0 : (flExponent - 1));
			bool roundValueUp = false;
			for (intptr_t i = 0;; ++i) {
				size_t digit = (i >= digitsUntilNulls ? 0 : digits.next());

				/* check if this is the last digit and determine if the second to last value should be increased */
				if (i >= totalDigits) {
					roundValueUp = (digit >= radix / 2);
					break;
				}

				/* check if a null-digit has been encountered, in which case all previously delayed digits can be flushed */
				if (digit == 0) {
					/* check if a last-digit exists to be flushed and write it out and the chain of max-digits */
					if (delayed >= 0) {
						if (i > 0)
							detail::FlushFloatDigits<Mode>(sink, lastDigit, 1, digitsBeforePoint, digitSet);
						detail::FlushFloatDigits<Mode>(sink, radix - 1, delayed, digitsBeforePoint, digitSet);
						delayed = -1;
					}
					else
						--delayed;
				}

				/* check if a maximum digit has been encountered, in which case a new chain with a cached last-value can be started */
				else if (digit >= radix - 1 && (i > 0 || delayed < 0)) {
					/* check if the last chain was a chain of nulls */
					if (delayed < 0) {
						detail::FlushFloatDigits<Mode>(sink, 0, -delayed - 1, digitsBeforePoint, digitSet);
						lastDigit = 0;
						delayed = 1;
					}
					else
						++delayed;
				}

				/* flush the last digits and keep the current digit as last digit */
				else {
					if (delayed < 0)
						detail::FlushFloatDigits<Mode>(sink, 0, -delayed, digitsBeforePoint, digitSet);
					else {
						if (i > 0)
							detail::FlushFloatDigits<Mode>(sink, lastDigit, 1, digitsBeforePoint, digitSet);
						detail::FlushFloatDigits<Mode>(sink, radix - 1, delayed, digitsBeforePoint, digitSet);
					}
					lastDigit = digit;
					delayed = 0;
				}
			}

			/* check if the last value was part of a chain of nulls, in which case the last digit is only be affected by the rounding */
			if (delayed < 0) {
				if (roundValueUp) {
					detail::FlushFloatDigits<Mode>(sink, 0, -delayed - 1, digitsBeforePoint, digitSet);
					detail::FlushFloatDigits<Mode>(sink, 1, 1, digitsBeforePoint, digitSet);
				}
				else if (!clipTrailing)
					detail::FlushFloatDigits<Mode>(sink, 0, -delayed, digitsBeforePoint, digitSet);
			}

			/* check if no rounding needs to be performed, in which case all values can just be flushed (cannot end with a null) */
			else if (!roundValueUp) {
				detail::FlushFloatDigits<Mode>(sink, lastDigit, 1, digitsBeforePoint, digitSet);
				detail::FlushFloatDigits<Mode>(sink, radix - 1, delayed, digitsBeforePoint, digitSet);
			}

			/* check for the edge-case of all digits being the highest-digit, in which case the rounding will be carried
			*	out of the digits (can only occur for fixed-style floats with at least one digit on the integer side) */
			else if (lastDigit == radix - 1) {
				++digitsBeforePoint;
				detail::FlushFloatDigits<Mode>(sink, 1, 1, digitsBeforePoint, digitSet);
				if (!clipTrailing)
					detail::FlushFloatDigits<Mode>(sink, 0, delayed, digitsBeforePoint, digitSet);
				++flExponent;
			}

			/* write the last value out (increased by one) and check if the chain of maximum-values should be written out as nulls as well */
			else {
				detail::FlushFloatDigits<Mode>(sink, lastDigit + 1, 1, digitsBeforePoint, digitSet);
				if (!clipTrailing)
					detail::FlushFloatDigits<Mode>(sink, 0, delayed, digitsBeforePoint, digitSet);
			}

			/* check if remaining nulls need to be inserted */
			if (!scientific && digitsBeforePoint > 0)
				detail::FlushFloatDigits<Mode>(sink, 0, digitsBeforePoint, digitsBeforePoint, digitSet);

			/* check if the exponent needs to be written out */
			if (!scientific)
				return;
			str::EncodeInto<Mode>(sink, U"eE^^"[(upperCase ? 0x01 : 0x00) + (radix > 12 ? 0x02 : 0x00)]);
			str::EncodeInto<Mode>(sink, --flExponent < 0 ? U'-' : U'+');
			flExponent = (flExponent < 0 ? -flExponent : flExponent);

			/* write the exponent to a temporary buffer (cannot overflow the string-buffer) */
			str::U32Small<64> buffer;
			detail::PrintInteger<unsigned int, Mode>(buffer, static_cast<unsigned int>(flExponent), radix, upperCase);

			/* insert the required padding-nulls and the exponent-number */
			for (size_t i = buffer.size(); i < expDigits; ++i)
				str::EncodeInto<Mode>(sink, U'0');
			for (size_t i = 0; i < buffer.size(); ++i)
				str::EncodeInto<Mode>(sink, buffer[i]);
		}

		template<class Type, class Mode>
		constexpr void PrintFloat(auto& sink, Type num, str::FloatStyle style, size_t radix, size_t precision, size_t expDigits, bool upperCase, bool hexFloat) {
			/* validate the float-type is usable (assume large-float uses uint64_t as LType, otherwise exponent boundaries needs to be adjusted) */
			static_assert(std::numeric_limits<Type>::digits <= 64, "Type must have mantissa smaller than/equal to 64-bit");
			static_assert(std::numeric_limits<Type>::radix == 2, "Type must use exponent-base two");
			static constexpr auto LowerPlay = (std::numeric_limits<Type>::min_exponent - std::numeric_limits<int64_t>::min());
			static constexpr auto UpperPlay = (std::numeric_limits<int64_t>::max() - std::numeric_limits<Type>::max_exponent);
			static_assert(UpperPlay >= 0 && LowerPlay >= std::numeric_limits<Type>::digits, "Exponent of Type must be small enough to be held by a large float");

			/* check if the value is negative and extract the sign */
			if (num < 0) {
				str::EncodeInto<Mode>(sink, U'-');
				num = -num;
			}

			/* check if the value is a special value */
			if (!std::isfinite(num)) {
				if (std::isinf(num))
					str::ConvertInto<Mode>(sink, upperCase ? U"INF" : U"inf", 0);
				else
					str::ConvertInto<Mode>(sink, upperCase ? U"NAN" : U"nan", 0);
				return;
			}

			/* check if the precision needs to be default-initialized and compute it as the number of digits covered by the mantissa-bits and otherwise
			*	limit the precision to allow it to not overflow a signed intptr_t and ensure the exp-digits must at least contain one digit */
			intptr_t totalDigits = 0;
			if (precision == 0)
				totalDigits = std::max<intptr_t>(1, intptr_t(std::floor(std::numeric_limits<Type>::digits / detail::LogBase2[radix])));
			else
				totalDigits = intptr_t(std::min<size_t>(precision, std::numeric_limits<intptr_t>::max() / 4 + 1));
			if (expDigits == 0)
				expDigits = 1;

			/* check if a hex-float with general style has been selected, which will be defaulted to scientific, or fixed-styles will simply result in radix=16 floats */
			if (hexFloat) {
				if (style == str::FloatStyle::general)
					style = str::FloatStyle::scientificShort;
				else if (style == str::FloatStyle::generalFull)
					style = str::FloatStyle::scientific;
				else if (style == str::FloatStyle::fixed || style == str::FloatStyle::fixedShort) {
					hexFloat = false;
					radix = 16;
				}
			}

			/* decompose the float into its exponent and mantissa as a pure integer */
			int flExponent = 0;
			uint64_t flMantissa = 0;
			if constexpr (std::numeric_limits<Type>::digits < 64)
				flMantissa = uint64_t(std::frexp(num, &flExponent) * Type(uint64_t(0x01) << std::numeric_limits<Type>::digits));
			else
				flMantissa = uint64_t(std::frexp(num, &flExponent) * Type(uint64_t(0x01) << 32) * Type(uint64_t(0x01) << 32));
			flExponent -= std::numeric_limits<Type>::digits;

			/* check if the value is null and print the null-representation as fast way out */
			if (flMantissa == 0) {
				/* produce the null-string */
				str::EncodeInto<Mode>(sink, U'0');
				if (style == str::FloatStyle::general || style == str::FloatStyle::fixedShort)
					return;

				/* generate the chain of nulls */
				if (style != str::FloatStyle::scientificShort && totalDigits > 1) {
					str::EncodeInto<Mode>(sink, U'.');
					for (intptr_t i = 1; i < totalDigits; ++i)
						str::EncodeInto<Mode>(sink, U'0');
				}

				/* check if an exponent needs to be added */
				if (style == str::FloatStyle::scientific || style == str::FloatStyle::scientificShort) {
					str::EncodeInto<Mode>(sink, U"eE^^pPpP"[(upperCase ? 0x01 : 0x00) + (radix > 12 ? 0x02 : 0x00) + (hexFloat ? 0x04 : 0x00)]);
					str::EncodeInto<Mode>(sink, U'+');
					for (size_t i = 0; i < expDigits; ++i)
						str::EncodeInto<Mode>(sink, U'0');
				}
				return;
			}

			/* check if a hex-string should be produced and dispatch the call to the handler */
			if (hexFloat) {
				detail::PrintHexFloat<Mode>(sink, totalDigits, flExponent, flMantissa, expDigits, (style == str::FloatStyle::scientificShort), upperCase);
				return;
			}

			/* select the method with the corresponding precision (any precision greater than 512 bits will result in potentially
			*	imprecise digits; ensure the bits do not match exactly due to propagating errors in the computations) */
			if (std::ceil(totalDigits * detail::LogBase2[radix]) < 64 - 4)
				detail::PrintNormalFloat<Type, Mode, 1>(sink, totalDigits, flExponent, flMantissa, style, radix, expDigits, upperCase);
			else if (std::ceil(totalDigits * detail::LogBase2[radix]) < 128 - 4)
				detail::PrintNormalFloat<Type, Mode, 2>(sink, totalDigits, flExponent, flMantissa, style, radix, expDigits, upperCase);
			else if (std::ceil(totalDigits * detail::LogBase2[radix]) < 192 - 4)
				detail::PrintNormalFloat<Type, Mode, 3>(sink, totalDigits, flExponent, flMantissa, style, radix, expDigits, upperCase);
			else if (std::ceil(totalDigits * detail::LogBase2[radix]) < 256 - 4)
				detail::PrintNormalFloat<Type, Mode, 4>(sink, totalDigits, flExponent, flMantissa, style, radix, expDigits, upperCase);
			else if (std::ceil(totalDigits * detail::LogBase2[radix]) < 512 - 4)
				detail::PrintNormalFloat<Type, Mode, 8>(sink, totalDigits, flExponent, flMantissa, style, radix, expDigits, upperCase);
			else
				detail::PrintNormalFloat<Type, Mode, 20>(sink, totalDigits, flExponent, flMantissa, style, radix, expDigits, upperCase);
		}
	}

	/*
	*	Parse the integer/float with an optional leading sign and optional prefix for the radix (prefixes: [0b/0q/0o/0d/0x])
	*	Use the radix for the mantissa, exponent, and base of floats. (Use str::HexFloat-radix to parse hex-floats)
	*
	*	r: any valid digit for given radix (lower or upper case)
	*	integer: [\+\-]?(0[bBqQoOdDxX])?r+
	*	float: [\+\-]?(0[bBqQoOdDxX])?(r*(r|\.)r*)([eEpP^][\+\-]?r+)?
	*	hex-floats: [\+\-]?(0[xX])?(h*(h|\.)h*)([pP][\+\-]?d+)?
	*		- with h any hex-digit, and d any decimal-digit
	*/
	template <str::IsNumber Type, str::IsMode Mode = str::Relaxed>
	constexpr str::NumParseOut<Type> ParseNum(const str::AnyString auto& source, size_t radix = 10, bool noPrefix = false) {
		using ChType = str::StringChar<decltype(source)>;

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return str::NumParseOut<Type>();

		/* ensure the radix is valid (set it even for hex-floats in order to ensure the prefix is parsed correctly) */
		bool hexFloat = (radix == str::HexFloat);
		if (std::is_floating_point_v<Type> && hexFloat)
			radix = 16;
		else if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;

		/* parse the sign and prefix and check if the parsed prefix is valid */
		detail::PrefixParseOut prefix = detail::ParseSignAndPrefix<Type, ChType, Mode>(view, noPrefix);
		if (prefix.radix != radix)
			prefix.prefixConsumed = 0;
		size_t prefixSize = prefix.prefixConsumed + prefix.signConsumed;

		/* parse the integer or float and add the sign/prefix consumed characters to the overall consumed characters */
		str::NumParseOut<Type> out;
		if constexpr (std::is_integral_v<Type>)
			out = detail::ParseInteger<Type, ChType, Mode>(view.substr(prefixSize), radix, prefix.negative);
		else
			out = detail::ParseFloat<Type, ChType, Mode>(view.substr(prefixSize), radix, prefix.negative, hexFloat);
		out.consumed += prefixSize;
		return out;
	}

	/* check for the prefix on the potentially signed string (i.e. leading +/-, but - only if type permits)
	*	and return defRadix for invalid prefixes or the radix (prefixes: [0b/0q/0o/0d/0x]) */
	template <str::IsNumber Type, str::IsMode Mode = str::Relaxed>
	constexpr size_t PeekPrefix(const str::AnyString auto& source, size_t defRadix = 10) {
		using ChType = str::StringChar<decltype(source)>;

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return defRadix;

		/* parse the sign and prefix and return the parsed radix */
		detail::PrefixParseOut out = detail::ParseSignAndPrefix<Type, ChType, Mode>(view, false);
		return (out.radix == 0 ? defRadix : out.radix);
	}

	/* print integer with optional leading [-] for the given radix to the sink and return the sink */
	template <str::IsMode Mode = str::Relaxed>
	constexpr auto& IntInto(str::AnySink auto& sink, const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		using Type = std::remove_cvref_t<decltype(num)>;

		/* ensure the radix is valid and print the integer */
		if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;
		detail::PrintInteger<Type, Mode>(sink, num, radix, upperCase);
		return sink;
	}

	/* print the integer to a string of the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType, str::IsMode Mode = str::Relaxed>
	constexpr std::basic_string<ChType> Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		std::basic_string<ChType> out{};
		return str::IntInto<Mode>(out, num, radix, upperCase);
	}

	/* print the integer to a string of the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType, intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::Small<ChType, Capacity> Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		str::Small<ChType, Capacity> out{};
		return str::IntInto<Mode>(out, num, radix, upperCase);
	}

	/* convenience for fast integer-printing to a std::basic_string */
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::string ChInt(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char, Mode>(num, radix, upperCase);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::wstring WdInt(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<wchar_t, Mode>(num, radix, upperCase);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u8string U8Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char8_t, Mode>(num, radix, upperCase);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u16string U16Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char16_t, Mode>(num, radix, upperCase);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u32string U32Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char32_t, Mode>(num, radix, upperCase);
	}

	/* convenience for fast integer-printing to a str::Small<Capacity> */
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::ChSmall<Capacity> ChInt(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char, Capacity, Mode>(num, radix, upperCase);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::WdSmall<Capacity> WdInt(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<wchar_t, Capacity, Mode>(num, radix, upperCase);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U8Small<Capacity> U8Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char8_t, Capacity, Mode>(num, radix, upperCase);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U16Small<Capacity> U16Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char16_t, Capacity, Mode>(num, radix, upperCase);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U32Small<Capacity> U32Int(const str::IsInteger auto& num, size_t radix = 10, bool upperCase = false) {
		return str::Int<char32_t, Capacity, Mode>(num, radix, upperCase);
	}

	/* print float with optional leading [-] for the given radix to the sink and return the sink (use str::HexFloat-radix to print hex-floats) */
	template <str::IsMode Mode = str::Relaxed>
	constexpr auto& FloatInto(str::AnySink auto& sink, const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		using Type = std::remove_cvref_t<decltype(num)>;

		/* check if a hex-float has been requested and ensure the radix is valid */
		bool hexFloat = (radix == str::HexFloat);
		if (radix < str::MinRadix || radix > str::MaxRadix)
			radix = 10;
		detail::PrintFloat<Type, Mode>(sink, num, style, radix, precision, expDigits, upperCase, hexFloat);
		return sink;
	}

	/* print the float to a string of the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType, str::IsMode Mode = str::Relaxed>
	constexpr std::basic_string<ChType> Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		std::basic_string<ChType> out{};
		return str::FloatInto<Mode>(out, num, style, precision, radix, upperCase, expDigits);
	}

	/* print the float to a string of the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType, intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::Small<ChType, Capacity> Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		str::Small<ChType, Capacity> out{};
		return str::FloatInto<Mode>(out, num, style, precision, radix, upperCase, expDigits);
	}

	/* convenience for fast float-printing to a std::basic_string */
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::string ChFloat(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::wstring WdFloat(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<wchar_t, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u8string U8Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char8_t, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u16string U16Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char16_t, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u32string U32Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char32_t, Mode>(num, style, precision, radix, upperCase, expDigits);
	}

	/* convenience for fast float-printing to a str::Small<Capacity> */
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::ChSmall<Capacity> ChFloat(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char, Capacity, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::WdSmall<Capacity> WdFloat(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<wchar_t, Capacity, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U8Small<Capacity> U8Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char8_t, Capacity, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U16Small<Capacity> U16Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char16_t, Capacity, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U32Small<Capacity> U32Float(const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, bool upperCase = false, size_t expDigits = 2) {
		return str::Float<char32_t, Capacity, Mode>(num, style, precision, radix, upperCase, expDigits);
	}
}
