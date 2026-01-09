/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2025-2026 Bjoern Boss Henrichsen */
#pragma once

#include "str-number.h"

/*
*	Note: While si-values respect all boundaries and rounding constraints,
*		it does not guarantee as precise conversions, as str-number does.
*	It is not designed for accurate value handling, but rather for convenience.
*	Specifically: normal double multiplications and divisions are used for applying the scale.
*		This may result in minor loss of precision in the least significant digits.
*		In conclusion:
*			str::SiParseNum(str::SiValue(x)) is not always x (for given precision)
*		while:
*			str::ParseNum(str::Float(x)) will always be x (for given precision)
*
*	str::SiParseNumTo can be used for values of highest precision, as it will check for si-scaling.
*	If no scaling is found, it will use the default str::ParseNum functions.
*	Only if a scale factor exists, the value will be parsed as double and converted.
*
*
*	Coding-Rules:
*	 - decoding rules of str-number
*	 - encoding using str::CodepointTo<str::CodeError::nothing>/str::FastcodeAllTo<str::CodeError::nothing> and of str-number
*		(character set is so small and essential that every codepage should support it)
*/
namespace str {
	/*
	*	decimal: decimal si-scale is expected
	*	binary: binary si-scale is expected
	*	detect: check if it might be a binary si-scale, otherwise only detect a decimal si-scale
	*/
	enum class SiScaleMode : uint8_t {
		decimal,
		binary,
		detect
	};

	/* si scale description (empty prefix implies no prefix, scale will be 1, otherwise always
	*	size of 1 for decimal, and size of 2 for binary, and scale will be set accordingly) */
	struct SiScale {
		str::Local<char32_t, 2> prefix;
		double scale = 1.0;
	};

	/* parsed si scale (consumed is zero if no prefix is detected,
	*	scale will be 1, otherwise scale will be set accordingly) */
	struct ParsedSiScale {
		size_t consumed = 0;
		double scale = 1.0;
	};

	namespace detail {
		static constexpr size_t SiFirstGreater1 = 10;
		static constexpr size_t SiNumScales = 20;
		static constexpr const char32_t* SiPrefixCharAscii = U"qryzafpnumkMGTPEZRQ";
		static constexpr const char32_t* SiPrefixCharUnicode = U"qryzafpn\u00b5mkMGTPEZRQ";
		static constexpr const char32_t SiPrefixBinaryIndicator = U'i';
		static constexpr const char32_t SiPrefixCharDetectU[] = { U"qrYzAFPNUmKMGTPEZRQ\u039c" };
		static constexpr const char32_t SiPrefixCharDetectL[] = { U"qryzafpnumkMgtpeZRQ\u00b5" };
		static constexpr const char32_t SiPrefixBinaryDetectU = U'I';
		static constexpr const char32_t SiPrefixBinaryDetectL = U'i';
		static constexpr double SiScale10[] = {
			1.0e-30, 1.0e-27, 1.0e-24, 1.0e-21, 1.0e-18, 1.0e-15, 1.0e-12, 1.0e-9, 1.0e-6, 1.0e-3,
			1.0e+3, 1.0e+6, 1.0e+9, 1.0e+12, 1.0e+15, 1.0e+18, 1.0e+21, 1.0e+24, 1.0e+27, 1.0e-6
		};
		static constexpr double SiScale2[] = {
			0x1p-100, 0x1p-90, 0x1p-80, 0x1p-70, 0x1p-60, 0x1p-50, 0x1p-40, 0x1p-30, 0x1p-20, 0x1p-10,
			0x1p+10, 0x1p+20, 0x1p+30, 0x1p+40, 0x1p+50, 0x1p+60, 0x1p+70, 0x1p+80, 0x1p+90, 0x1p-20
		};

		template <class ChType>
		constexpr str::ParsedSiScale ParseSiScale(const std::basic_string_view<ChType>& source, str::SiScaleMode scale) {
			/* extract the first token (implicitly checks for empty strings) */
			auto [cp, consumed] = str::GetCodepoint<str::CodeError::nothing>(source);
			if (cp == str::Invalid)
				return str::ParsedSiScale{};

			/* find the matching token and check if any have been found */
			size_t index = 0;
			while (index < std::size(detail::SiPrefixCharDetectU) && cp != detail::SiPrefixCharDetectU[index] && cp != detail::SiPrefixCharDetectL[index])
				++index;
			if (index == std::size(detail::SiPrefixCharDetectU))
				return str::ParsedSiScale{};

			/* check if a binary scale is to be found */
			bool binary = false;
			if (scale != str::SiScaleMode::decimal) {
				auto [_cp, _consumed] = str::GetCodepoint<str::CodeError::nothing>(source.substr(consumed));

				/* check if a valid indicator was found and consume it */
				if (_cp == detail::SiPrefixBinaryDetectU || _cp == detail::SiPrefixBinaryDetectL) {
					consumed += _consumed;
					binary = true;
				}
				else if (scale == str::SiScaleMode::binary)
					return str::ParsedSiScale{};
			}

			/* return the proper scaling */
			return str::ParsedSiScale{ consumed, (binary ? detail::SiScale2 : detail::SiScale10)[index] };
		}

		template <class Type>
		str::SiScale GetSiScale(const Type& num, bool asciiOnly, bool binarySystem) {
			/* check if the value is invalid */
			if constexpr (str::IsFloat<Type>) {
				if (!std::isfinite(num))
					return str::SiScale{};
			}

			/* convert the value and ensure its unsigned */
			double value = detail::NumAbs(static_cast<double>(num));

			/* fetch the scale to use (equal to number of scales is equal to scale of 1) */
			const double* scale = (binarySystem ? detail::SiScale2 : detail::SiScale10);
			size_t indexToUse = 0;

			/* check if the value is greater-equal to 1 */
			if (value >= 1.0) {
				indexToUse = detail::SiNumScales;
				for (size_t i = detail::SiFirstGreater1; i < detail::SiNumScales && value >= scale[i]; ++i)
					indexToUse = i;
			}
			else {
				indexToUse = detail::SiFirstGreater1 - 1;
				while (indexToUse > 0 && value < scale[indexToUse])
					--indexToUse;
			}

			/* setup the prefix type */
			if (indexToUse == detail::SiNumScales)
				return str::SiScale{};
			const char32_t* prefix = (asciiOnly ? detail::SiPrefixCharAscii : detail::SiPrefixCharUnicode);
			str::SiScale out = { .prefix = { 1, prefix[indexToUse] }, .scale = scale[indexToUse] };
			if (binarySystem)
				out.prefix.push_back(detail::SiPrefixBinaryIndicator);
			return out;
		}
	}

	/* to be used for si-value arguments */
	struct ArgsSiMake {
		bool asciiOnly = false;
		bool binarySystem = false;
	};
	struct ArgsSiParse {
		size_t radix = 10;
		str::PrefixMode prefix = str::PrefixMode::none;
		str::SiScaleMode scale = str::SiScaleMode::decimal;
	};
	struct ArgsSiValue {
		size_t precision = 0;
		size_t radix = 10;
		str::FloatStyle fltStyle = str::FloatStyle::general;
		str::NumStyle numStyle = str::NumStyle::lower;
		bool asciiOnly = false;
		bool binarySystem = false;
	};

	/* match the number to the corresponding si-scale type, such that it has between [1,3] digits before the decimal point
	*	(supported units: Q, R, Y, Z, E, P, T, G, M, k, m, u, n, p, f, a, z, y, r, q; if ascii-only, 'u' is used for micro,
	*	instead of U+00b5; if binary-system, use [2^10, 2^20, ...], otherwise use normal scale [10^3, 10^6, ...]) */
	constexpr str::SiScale SiMakeScale(const str::IsNumber auto& num, str::ArgsSiMake args = {}) {
		using NumType = std::remove_cvref_t<decltype(num)>;
		return detail::GetSiScale<NumType>(num, args.asciiOnly, args.binarySystem);
	}

	/* decode the si-scale at the string (note: unique characters are case-insensitive) */
	constexpr str::ParsedSiScale SiPeekScale(const str::IsStr auto& source, str::SiScaleMode scale = str::SiScaleMode::decimal) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		return detail::ParseSiScale<ChType>(view, scale);
	}

	/* parse the next number as a float and respect and apply the corresponding si-scale (if no si-ending is detected, it will parse the number as its
	*	target type, which can potentially result in more details; otherwise it will be parsed as a double and converted to the destination type) */
	template <str::IsNumber Type>
	constexpr str::ParsedNum SiParseNumTo(const str::IsStr auto& source, Type& num, str::ArgsSiParse args = {}) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		str::ArgsParse numArgs{ .radix = args.radix, .prefix = args.prefix };

		/* check if the number actually has a prefix, and if not, simply parse it as is by itself (to preserve
		*	as much information as possible; skip double, as the si number would be parsed as double) */
		size_t skipped = detail::SkipNum<double>(source, numArgs);
		str::ParsedSiScale siScale = detail::ParseSiScale<ChType>(view.substr(skipped), args.scale);
		if (siScale.consumed == 0)
			return str::ParseNumTo(view, num, numArgs);

		/* parse the number itself */
		double value = 0.0;
		auto [consumed, result] = str::ParseNumTo(view, value, numArgs);
		if (result != str::NumResult::valid && result != str::NumResult::range) {
			num = 0;
			return str::ParsedNum{ consumed, result };
		}
		consumed += siScale.consumed;

		/* check if the value cannot be represented by the corresponding type */
		bool finite = std::isfinite(value);
		if (!finite && str::IsInteger<Type>) {
			num = 0;
			return str::ParsedNum{ consumed, str::NumResult::invalid };
		}

		/* check if a negative value has been used for an unsigned value */
		if (value < 0 && !std::is_signed_v<Type>)
			result = str::NumResult::range;

		/* check if the value is valid and can be scaled based on the si-value */
		if (result == str::NumResult::valid) {
			/* check if the value is trivial, or not finite (can only happen for
			*	floats), in which case the scale does not need to be applied */
			if (value == 0 || !finite) {
				num = Type(value);
				return str::ParsedNum{ consumed, str::NumResult::valid };
			}

			/* apply the scale and check if the value has not overflown/underflown */
			value *= siScale.scale;
			if (value != 0 && std::isfinite(value) && value >= static_cast<double>(std::numeric_limits<Type>::lowest()) && value <= static_cast<double>(std::numeric_limits<Type>::max())) {
				num = Type(value);

				/* check if the loss of precision still results in a non-zero value */
				if (num != 0)
					return str::ParsedNum{ consumed, str::NumResult::valid };
				value = 0;
			}
		}

		/* check if the value has underflown */
		if (value == 0) {
			num = 0;
			return str::ParsedNum{ consumed, str::NumResult::range };
		}

		/* setup the overflown range error */
		if constexpr (str::IsInteger<Type>)
			num = (value < 0 ? std::numeric_limits<Type>::min() : std::numeric_limits<Type>::max());
		else
			num = Type(value);
		return str::ParsedNum{ consumed, str::NumResult::range };
	}

	/* parse the next number using str::ParseNumTo and respect the corresponding si-scale and return it as one structure */
	template <str::IsNumber Type>
	constexpr str::ParsedNumValue<Type> SiParseNum(const str::IsStr auto& source, const str::ArgsSiParse& args = {}) {
		Type num = 0;
		auto [consumed, result] = str::SiParseNumTo<Type>(source, num, args);
		return str::ParsedNumValue<Type>{ num, consumed, result };
	}

	/* parse the entire string as number using str::ParseNumTo and respect the corresponding si-scale and return the value,
	*	if the string was fully consumed and fit into the type, and otherwise return the [otherwise] value */
	template <str::IsNumber Type>
	constexpr Type SiParseNumAll(const str::IsStr auto& source, Type otherwise = std::numeric_limits<Type>::max(), const str::ArgsSiParse& args = {}) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };

		/* parse the number as far as possible */
		Type num = 0;
		auto [consumed, result] = str::SiParseNumTo<Type>(view, num, args);

		/* check if no error occurred and the entire string has been consumed */
		if (result == str::NumResult::valid && consumed == view.size())
			return num;
		return otherwise;
	}

	/* print value with optional leading [-] for the given radix with si-scale prefix to the sink and return the sink (using str::FloatTo) */
	constexpr void SiValueTo(str::IsSink auto&& sink, const str::IsNumber auto& num, str::ArgsSiValue args = {}) {
		using NumType = std::remove_cvref_t<decltype(num)>;

		/* setup the scale to be used */
		str::SiScale scale = detail::GetSiScale<NumType>(num, args.asciiOnly, args.binarySystem);

		/* write the actual value out and append the si-scale */
		str::FloatTo(sink, static_cast<double>(num) / scale.scale, { .precision = args.precision, .radix = args.radix, .fltStyle = args.fltStyle, .numStyle = args.numStyle });
		str::FastcodeAllTo<str::CodeError::nothing>(sink, scale.prefix);
	}

	/* write the value to an object of the given sink-type using str::SiValueTo and return it */
	template <str::IsSink SinkType>
	constexpr SinkType SiValue(const str::IsNumber auto& num, const str::ArgsSiValue& args = {}) {
		SinkType sink{};
		str::SiValueTo(sink, num, args);
		return sink;
	}
}
