#pragma once

#include "str-common.h"
#include "str-convert.h"

#include <type_traits>
#include <cinttypes>
#include <limits>
#include <cmath>

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
		overflow
	};
	struct NumParseOut {
		size_t consumed = 0;
		str::NumResult result = str::NumResult::empty;
	};
	struct FloatRadix {
	public:
		size_t manRadix = 0;
		size_t expRadix = 0;
		size_t expBase = 0;

	public:
		constexpr FloatRadix(size_t r = 10) : manRadix(r), expRadix(r), expBase(r) {}
		constexpr FloatRadix(size_t rad, size_t base) : manRadix(rad), expRadix(rad), expBase(base) {}
		constexpr FloatRadix(size_t man, size_t exp, size_t base) : manRadix(man), expRadix(exp), expBase(base) {}
	};
	static constexpr str::FloatRadix HexFloat = str::FloatRadix{ 16, 10, 2 };

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

				/* check if the address is negative and clip the value accordingly */
				if (bt < 0) {
					v >>= -bt;
					bt = 0;
				}
				size_t full = (bt / UnitBits), off = (bt % UnitBits);

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
				size_t full = (use / UnitBits), off = (use % UnitBits);
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
				size_t bit = UnitBits;
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
				size_t bit = 0;
				while (((pBuffer[index] >> bit++) & 0x01) == 0)
					++count;
				return count;
			}
		};

		/* positive non-null float */
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
						if (old > next)
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
				detail::BitStream<LType, Units * 2> dividend{}, result{};
				detail::BitStream<LType, Units> divisor{};

				/* setup the dividend mantissa with the upper bits of the dividend and highest bit set */
				ExpType lOff = pMantissa.upper();
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					dividend.set<LType>(TotalBits + i, pMantissa.get<LType>(i - lOff));

				/* setup the divisor with the highest bit set */
				ExpType rOff = r.pMantissa.upper();
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					divisor.set<LType>(i, r.pMantissa.get<LType>(i - rOff));

				/* check if the lower bits of the divisor are null, in which case the increase by one is not necessary and otherwise
				*	ensure each partial division uses a divisor increased by one to ensure (dividend >= res * divisor) at all times */
				LType lowerOffset = (divisor.lower() < (TotalBits - SmallBits) ? 1 : 0);

				/* perform the long division */
				ExpType nullBits = 0;
				while (nullBits < TotalBits) {
					ExpType btIndex = (2 * TotalBits - nullBits - LargeBits);

					/* divide the upper large-number of bits of the dividend by the upper small-number of bits of the divisor */
					LType res = dividend.get<LType>(btIndex) / (LType(divisor.get<SType>(TotalBits - SmallBits)) + lowerOffset);

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
							LType old = dividend.get<LType>(TotalBits - nullBits - SmallBits + i);
							LType next = old - fst - (lowBorrow ? 1 : 0);
							dividend.set<LType>(TotalBits - nullBits - SmallBits + i, next);
							borrow |= (next > old ? 0x04 : 0x00);
						}

						/* subtract the second value from the dividend */
						if (snd != 0 || highBorrow) {
							LType old = dividend.get<LType>(TotalBits - nullBits + i);
							LType next = old - snd - (highBorrow ? 1 : 0);
							dividend.set<LType>(TotalBits - nullBits + i, next);
							borrow |= (next > old ? 0x08 : 0x00);
						}
					}

					/* compute the new number of null-bits (will implicitly end if the dividend becomes zero, i.e. perfect division) */
					nullBits = dividend.upper();
				}

				/* count the number of leading nulls to keep as much information as possible in the result */
				ExpType nulls = result.upper();

				/* round the result (in case of the division not being perfect, add one to the result to cleanly stop infinite repeating sequences) */
				if (nullBits < 2 * TotalBits) {
					for (ExpType i = TotalBits - nulls; i < 2 * TotalBits; i += LargeBits) {
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
					out.pMantissa.set<LType>(i, result.get<LType>(TotalBits - nulls + i));
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

				/* extract the modulo-result (i.e. everything to the left of the decimal point) */
				LType intVal = prod.get<LType>(-val.pExponent);

				/* write the remaining bits of the product (i.e. everything to the right of the decimal point) to the result */
				ThisType out{};
				out.pExponent = -TotalBits;
				for (size_t i = 0; i < TotalBits; i += LargeBits)
					out.pMantissa.set<LType>(i, prod.get<LType>(i - TotalBits - val.pExponent));
				return { out, intVal };
			}

			/* divide val by d and return result as large-float and remainder as integer (val expected to be integer, highest bit of d must not be set, else: undefined) */
			static std::pair<ThisType, LType> IntDivMod(const ThisType& val, LType d) {
				/* highest bit of [d] must not be set */
				detail::BitStream<LType, Units> result{}, dividend{};

				/* setup the dividend */
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					dividend.set<LType>(i, val.pMantissa.get<LType>(i - val.pExponent));

				/* perform the division */
				while (true) {
					ExpType btIndex = std::max<ExpType>(0, TotalBits - LargeBits - dividend.upper());

					/* extract the next value and perform the division and extract the rest (cannot be null) */
					LType localDividend = dividend.get<LType>(btIndex);
					LType div = localDividend / d;
					LType mod = localDividend % d;

					/* check if the end has been reached because the division yielded no result and otherwise update the dividend */
					if (div == 0)
						break;
					dividend.set<LType>(btIndex, mod);

					/* add the result to the output and carry any overflow through (cannot leave the result-mantissa) */
					LType old = result.get<LType>(btIndex);
					LType next = old + div;
					result.set<LType>(btIndex, next);
					if (old > next) {
						for (ExpType i = btIndex + LargeBits; i < TotalBits; i += LargeBits) {
							old = result.get<LType>(i);
							result.set<LType>(i, ++old);
							if (old != 0)
								break;
						}
					}
				}

				/* extract the modulo-result (i.e. everything remaining in the dividend) */
				LType intVal = dividend.get<LType>(0);

				/* setup the result containing the current result */
				ThisType out{};
				out.pExponent = 0;
				for (ExpType i = 0; i < TotalBits; i += LargeBits)
					out.pMantissa.set<LType>(i, result.get<LType>(i));
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

			/* return closest float describing the correponsing value, overflow */
			template <class FlType>
			static std::pair<FlType, bool> ReadFloat(const ThisType& val) {
				static_assert(LargeBits <= std::numeric_limits<FlType>::max_exponent && std::numeric_limits<FlType>::min_exponent <= 0, "Exponent of FlType must at least be able to hold one LType");
				FlType out{};

				/* extract the highest (most relevant) bit such that the mantissa can be positioned to hold the highest bit to the right of the decimal-point */
				ExpType highest = val.pMantissa.upper();

				/* write all bits to the float (cannot overflow) */
				for (ExpType i = TotalBits - highest - LargeBits; i > -LargeBits; i -= LargeBits) {
					FlType temp = FlType(val.pMantissa.get<LType>(i));
					if (temp == 0)
						continue;

					/* scale the temporary value such that the highest bit lies to the right of the decimal-point (this
					*	ensures that the must significant bits will not trigger an underflow/overflow while writing them and
					*	std::ldexp will at most trigger an underflow, in which case the result will be rounded towards null) */
					out += std::ldexp(temp, static_cast<int>(i - TotalBits));
				}

				/* compute the final exponent to be applied to the result and check if it has overflown */
				intptr_t exponent = val.pExponent + TotalBits - highest;
				if (exponent < 0 && val.pExponent > 0)
					return { out, true };

				/* check if the exponent can even fit into an integer */
				if (exponent < std::numeric_limits<int>::min() || exponent > std::numeric_limits<int>::max())
					return { FlType{}, (exponent > 0) };

				/* apply the exponent to the result and check for an overflow */
				out = std::ldexp(out, static_cast<int>(exponent));
				return { out, (errno == ERANGE) };
			}
		};

		static constexpr size_t MaxRadix = 36;
		static constexpr long double LogBase2[MaxRadix + 1] = {
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
		static constexpr uint64_t MagnitudeIn64Bit[MaxRadix + 1] = {
			0x0000000000000000, 0x0000000000000000, 0x8000000000000000, 0xa8b8b452291fe821,
			0x4000000000000000, 0x6765c793fa10079d, 0x41c21cb8e1000000, 0x3642798750226111,
			0x8000000000000000, 0xa8b8b452291fe821, 0x8ac7230489e80000, 0x4d28cb56c33fa539,
			0x1eca170c00000000, 0x780c7372621bd74d, 0x1e39a5057d810000, 0x5b27ac993df97701,
			0x1000000000000000, 0x27b95e997e21d9f1, 0x5da0e1e53c5c8000, 0xd2ae3299c1c4aedb,
			0x16bcc41e90000000, 0x2d04b7fdd9c0ef49, 0x5658597bcaa24000, 0xa0e2073737609371,
			0x0c29e98000000000, 0x14adf4b7320334b9, 0x226ed36478bfa000, 0x383d9170b85ff80b,
			0x5a3c23e39c000000, 0x8e65137388122bcd, 0xdd41bb36d259e000, 0x0aee5720ee830681,
			0x1000000000000000, 0x172588ad4f5f0981, 0x211e44f7d02c1000, 0x2ee56725f06e5c71,
			0x41c21cb8e1000000
		};
		static constexpr const char32_t* DigitLower = U"0123456789abcdefghijklmnopqrstuvwxyz";
		static constexpr const char32_t* DigitUpper = U"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		static constexpr const char32_t* ExponentMap = U"eeeeeeeeeeeeeeppppppppppp^^^^^^^^^^^";
		static constexpr const char32_t* PrefixMap = U"__b_q___o_d_____x___________________";
		static constexpr const size_t MaxDigitMap = 128;
		static constexpr uint8_t CPDigitMap[MaxDigitMap] = {
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
		template <class Type, class ChType>
		detail::PrefixParseOut ParseSignAndPrefix(const std::basic_string_view<ChType>& view, bool signOnly) {
			enum class PrState : uint8_t {
				preSign,
				preZero,
				prePrefix
			} state = PrState::preSign;
			detail::PrefixParseOut out{};

			size_t prefixConsumed = 0;
			while (true) {
				/* decode the next character */
				auto [consumed, cp, result] = str::Decode(view.substr(out.signConsumed + prefixConsumed), true);
				if (result != str::DecResult::valid)
					return out;

				/* check if a sign has been encountered */
				if (state == PrState::preSign) {
					if (cp == U'+' || (std::is_signed_v<Type> && cp == U'-')) {
						out.negative = (cp == U'-');
						out.signConsumed += consumed;
						state = PrState::preZero;
					}
					if (signOnly)
						return out;
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

		template<class Type, class ChType>
		std::pair<Type, str::NumParseOut> ParseInteger(const std::basic_string_view<ChType>& view, size_t radix, bool negative) {
			static_assert(sizeof(uint64_t) >= sizeof(Type), "Type must be smaller/equal to 64-bit type of corresponding signedness");

			std::conditional_t<std::is_signed_v<Type>, int64_t, uint64_t> value = 0;
			size_t totalConsumed = 0;
			bool overflow = false;

			/* iterate over the digits and parse them */
			while (true) {
				/* decode the next character and check if a valid character has been found */
				auto [consumed, cp, res] = str::Decode(view.substr(totalConsumed), true);
				if (res != str::DecResult::valid || cp < 0 || cp >= detail::MaxDigitMap)
					break;

				/* check if the codepoint is a valid digit */
				size_t digit = detail::CPDigitMap[cp];
				if (digit >= radix)
					break;

				/* update the value and consumed characters */
				auto old = value;
				value = value * radix + digit;
				totalConsumed += consumed;

				/* check if the value has overflown or apply the sign */
				if constexpr (std::is_signed_v<Type>) {
					if ((old > 0) != (value > 0))
						overflow = true;
					else if (old == 0 && value != 0 && negative)
						value = -value;
				}
				else if (old > value)
					overflow = true;
			}

			/* check if the destination can hold the value and clamp it */
			if (value < std::numeric_limits<Type>::min() || value > std::numeric_limits<Type>::max())
				overflow = true;
			if (overflow)
				value = (negative ? std::numeric_limits<Type>::min() : std::numeric_limits<Type>::max());

			/* finalize the output structure (string cannot be empty, or will only be empty if a prefix
			*	has already been parsed, in which case the empty string will be considered an error as well) */
			str::NumParseOut out{};
			out.consumed = totalConsumed;
			if (totalConsumed == 0)
				out.result = str::NumResult::invalid;
			else if (overflow)
				out.result = str::NumResult::overflow;
			else
				out.result = str::NumResult::valid;
			return { static_cast<Type>(value), out };
		}

		template<class Type, class ChType>
		std::pair<Type, str::NumParseOut> ParseFloat(const std::basic_string_view<ChType>& view, const str::FloatRadix& radix, bool negative) {
			using FlType = std::conditional_t<(sizeof(Type) < sizeof(uint64_t)), detail::LargeFloat<uint32_t, uint64_t, 1>, detail::LargeFloat<uint32_t, uint64_t, 2>>;
			static_assert(std::numeric_limits<Type>::digits <= FlType::TotalBits, "Type must have mantissa smaller than the mantissa of large float");
			static_assert(std::numeric_limits<Type>::radix == 2, "Type must use exponent-base two");
			static_assert(std::numeric_limits<Type>::min_exponent >= std::numeric_limits<typename FlType::ExpType>::min() && std::numeric_limits<Type>::max_exponent <= std::numeric_limits<typename FlType::ExpType>::max(), "Type must have an exponent included by the large float's exponent");

			FlType flVal{};
			int64_t exponent = 0, mantissaOffset = 0;
			uint64_t mantissa = 0, magnitude = 1;
			size_t totalConsumed = 0;
			enum class FlState : uint8_t {
				preNumber,
				prePoint,
				postPoint,
				preExpSign,
				preExpNeg,
				preExpPos,
				inExpNeg,
				inExpPos
			} state = FlState::preNumber;
			enum class OfState : uint8_t {
				none,
				positive,
				negative
			} overflow = OfState::none;
			bool isNull = true, expDigit = false;

			/* compute the maximum number of digits to consider for the mantissa (approximate by computing the number
			*	of digits required to hold the given mantissa's number of bits; cannot overflow the large-float exponent,
			*	as the requirement for the mantissa itself ensures that the exponent can hold the given bits) */
			size_t digitsLeft = size_t(std::ceil(std::numeric_limits<Type>::digits / detail::LogBase2[radix.manRadix]));

			/* iterate over the digits and parse the mantissa/exponent */
			while (true) {
				/* decode the next character and check if a valid character has been found */
				auto [consumed, cp, res] = str::Decode(view.substr(totalConsumed), true);
				if (res != str::DecResult::valid || cp < 0)
					break;

				/* check if the character is a digit */
				if (cp < detail::MaxDigitMap && detail::CPDigitMap[cp] < (expDigit ? radix.expRadix : radix.manRadix)) {
					size_t digit = detail::CPDigitMap[cp];

					/* check if the start of the number has been encountered or the start of the exponent */
					if (state == FlState::preNumber)
						state = FlState::prePoint;
					else if (state == FlState::preExpPos || state == FlState::preExpSign)
						state = FlState::inExpPos;
					else if (state == FlState::preExpNeg)
						state = FlState::inExpNeg;

					/* update the shift to the mantissa-counter, depending on the digits after the decimal-point or the ignored digits before the decimal-point */
					if (state == FlState::prePoint && digitsLeft == 0) {
						if (++mantissaOffset < 0)
							overflow = OfState::positive;
					}
					else if (state == FlState::postPoint && digitsLeft > 0) {
						if (--mantissaOffset > 0)
							overflow = OfState::negative;
					}

					/* update the mantissa (only if further digits should be consumed and the value is not null anymore) */
					if ((state == FlState::prePoint || state == FlState::postPoint) && digitsLeft > 0 && (!isNull || cp != U'0')) {
						--digitsLeft;
						isNull = false;
						mantissa = radix.manRadix * mantissa + digit;

						/* check if the value should be flushed to the large float */
						if ((magnitude *= radix.manRadix) == detail::MagnitudeIn64Bit[radix.manRadix]) {
							flVal = FlType::MulAdd(flVal, magnitude, mantissa);
							mantissa = 0;
							magnitude = 1;
						}
					}

					/* update the exponent (only if there has not occurred an overflow yet and the mantissa is not null) */
					else if ((state == FlState::inExpPos || state == FlState::inExpNeg) && overflow == OfState::none && !isNull) {
						int64_t old = exponent;
						if (state == FlState::inExpPos)
							exponent = radix.expRadix * exponent + digit;
						else
							exponent = radix.expRadix * exponent - digit;

						/* check if an overflow occurred (exponent can only grow in one direction) */
						if (old != 0 && (old > 0) != (exponent > 0))
							overflow = (old < 0 ? OfState::negative : OfState::positive);
					}
				}

				/* check if the exponent sign has been encountered */
				else if ((cp == U'+' || cp == U'-') && state == FlState::preExpSign)
					state = (cp == U'-' ? FlState::preExpNeg : FlState::preExpPos);

				/* check if the decimal point has been encountered */
				else if (cp == U'.' && (state == FlState::preNumber || state == FlState::prePoint))
					state = FlState::postPoint;

				/* check if the exponent indicator has been encountered (no need to check if the exponent-indicator
				*	might be a digit, as it would otherwise already have been handled as a digit) */
				else if ((cp == U'p' || cp == U'P' || cp == U'e' || cp == U'E' || cp == U'^') && (state == FlState::prePoint || state == FlState::postPoint)) {
					state = FlState::preExpSign;
					expDigit = true;
				}

				/* no valid character has been encountered */
				else
					break;

				/* mark the character as consumed */
				totalConsumed += consumed;
			}

			/* check if a valid final-state has been reached (string cannot be empty, or will only be empty if a
			*	prefix has already been parsed, in which case the empty string will be considered an error as well) */
			if (state == FlState::preNumber || state == FlState::preExpSign || state == FlState::preExpNeg || state == FlState::preExpPos)
				return { Type{}, str::NumParseOut{ totalConsumed, str::NumResult::invalid } };

			/* check if the mantissa is null, in which case null can just be returned */
			if (isNull)
				return { Type{}, str::NumParseOut{ totalConsumed,str::NumResult::valid } };

			/* flush the remainder to the large float (cannot overflow due to the maximum number of digits) */
			if (magnitude > 1)
				flVal = FlType::MulAdd(flVal, magnitude, mantissa);

			/* apply the shift to the exponent, based on the encountered decimal digits or perform a separate mul-power, in case of
			*	the exponent-base not matching the mantissa-radix, as the post-point digits otherwise have a different weighting) */
			if (mantissaOffset != 0 && overflow == OfState::none) {
				bool overflown = false;

				if (radix.expBase == radix.manRadix) {
					int64_t old = exponent;
					exponent += mantissaOffset;
					overflown = (mantissaOffset < 0 ? (old <= exponent) : (old >= exponent));
				}
				else {
					auto [num, ov] = FlType::MulPow(flVal, FlType{ radix.manRadix }, mantissaOffset);
					flVal = num;
					overflown = ov;
				}

				/* check if an overflow occurred */
				if (overflown)
					overflow = (mantissaOffset < 0 ? OfState::negative : OfState::positive);
			}

			/* apply the exponent to the accumulated mantissa and check if an overflow occurred */
			if (overflow == OfState::none) {
				auto [num, ov] = FlType::MulPow(flVal, FlType{ radix.expBase }, exponent);
				flVal = num;
				if (ov)
					overflow = (exponent < 0 ? OfState::negative : OfState::positive);
			}

			/* read the final float value */
			if (overflow == OfState::none) {
				auto [value, ov] = FlType::ReadFloat<Type>(flVal);
				if (!ov) {
					if (negative)
						value = -value;
					return { value, str::NumParseOut{ totalConsumed, str::NumResult::valid } };
				}
				overflow = OfState::positive;
			}

			/* setup the overflown value and return the response */
			Type value = (overflow == OfState::negative ? Type(0.0) : std::numeric_limits<Type>::infinity());
			if (negative)
				value = -value;
			return { value, str::NumParseOut{ totalConsumed, str::NumResult::overflow } };
		}
	}

	/* parse the integer/float with an optional leading sign and optional prefix for the radix (prefixes: [0b/0q/0o/0d/0x])
	*	Use the radix for the mantissa, exponent, and base of floats. (Use str::ParseNum with explicit str::HexFloat-radix to parse hex-floats correctly)
	*
	*	r: any valid digit for given radix (lower or upper case)
	*	integer: [\+\-]?(0[bBqQoOdDxX])?r+
	*	float: [\+\-]?(0[bBqQoOdDxX])?(r+|(r*\.r*))([eEpP^][\+\-]?r+)?
	*/
	template <str::IsNumber Type>
	std::pair<Type, str::NumParseOut> ParseNum(const str::AnyString auto& source, size_t radix = 10, bool noPrefix = false) {
		using ChType = str::StringChar<decltype(source)>;

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return { Type{}, str::NumParseOut{} };

		/* ensure the radix is valid */
		if (radix < 2 || radix > detail::MaxRadix)
			radix = 10;

		/* parse the sign and prefix and check if the parsed prefix is valid */
		detail::PrefixParseOut prefix = detail::ParseSignAndPrefix<Type, ChType>(view, noPrefix);
		if (prefix.radix != radix)
			prefix.prefixConsumed = 0;
		size_t prefixSize = prefix.prefixConsumed + prefix.signConsumed;

		/* parse the integer or float and add the sign/prefix consumed characters to the overall consumed characters */
		std::pair<Type, str::NumParseOut> out{};
		if constexpr (std::is_integral_v<Type>)
			out = detail::ParseInteger<Type, ChType>(view.substr(prefixSize), radix, prefix.negative);
		else
			out = detail::ParseFloat<Type, ChType>(view.substr(prefixSize), str::FloatRadix{ radix, radix, radix }, prefix.negative);
		out.second.consumed += prefixSize;
		return out;
	}

	/* Parse floats but specifically specify the radix/base to be used for the corresponding float */
	template <str::IsFloat Type>
	std::pair<Type, str::NumParseOut> ParseNum(const str::AnyString auto& source, const str::FloatRadix& radix, bool noPrefix = false) {
		using ChType = str::StringChar<decltype(source)>;

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return { Type{}, str::NumParseOut{} };

		/* ensure the radix is valid */
		str::FloatRadix actual{ radix };
		if (actual.manRadix < 2 || actual.manRadix > detail::MaxRadix)
			actual.manRadix = 10;
		if (actual.expRadix < 2 || actual.expRadix > detail::MaxRadix)
			actual.expRadix = 10;
		if (actual.expBase < 2 || actual.expBase > detail::MaxRadix)
			actual.expBase = 10;

		/* parse the sign and prefix and check if the parsed prefix is valid */
		detail::PrefixParseOut prefix = detail::ParseSignAndPrefix<Type, ChType>(view, noPrefix);
		if (prefix.radix != actual.manRadix)
			prefix.prefixConsumed = 0;
		size_t prefixSize = prefix.prefixConsumed + prefix.signConsumed;

		/* parse the float and add the sign/prefix consumed characters to the overall consumed characters */
		std::pair<Type, str::NumParseOut> out = detail::ParseFloat<Type, ChType>(view.substr(prefixSize), actual, prefix.negative);
		out.second.consumed += prefixSize;
		return out;
	}

	/* check for the prefix on the potentially signed string (i.e. leading +/-, but - only if type permits)
	*	and return defRadix for invalid prefixes or the radix (prefixes: [0b/0q/0o/0d/0x]) */
	template <str::IsNumber Type>
	size_t ParsePrefix(const str::AnyString auto& source, size_t defRadix = 10) {
		using ChType = str::StringChar<decltype(source)>;

		/* check if the string is empty */
		std::basic_string_view<ChType> view{ source };
		if (view.empty())
			return defRadix;

		/* parse the sign and prefix and return the parsed radix */
		detail::PrefixParseOut out = detail::ParseSignAndPrefix<Type, ChType>(view, false);
		return (out.radix == 0 ? defRadix : out.radix);
	}

	enum class FloatStyle : uint8_t {
		dynamic,
		decimal,
		scientific
	};

	void NumInto(str::AnySink auto& sink, const str::IsInteger auto& num, size_t radix = 10) {

	}
	void NumInto(str::AnySink auto& sink, const str::IsFloat auto& num, str::FloatStyle style = str::FloatStyle::dynamic, size_t radix = 10) {

	}
	void NumInto(str::AnySink auto& sink, const str::IsFloat auto& num, size_t precision, str::FloatStyle style = str::FloatStyle::dynamic, size_t radix = 10) {

	}
}
