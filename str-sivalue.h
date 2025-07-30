/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2025 Bjoern Boss Henrichsen */
#pragma once

#include "str-number.h"

/*
*	Coding-Rules:
*	 - decoding rules of str-number
*	 - encoding using str::CodepointTo<err::Nothing>/str::FastcodeAllTo<err::Nothing> and of str-number
*		(character set is so small and essential that every codepage should support it)
*/
namespace str {
	/*
	*	decimal: decimal si-scale is expected
	*	binary: binary si-scale is expected
	*	optional: check if it might be a binary si-scale, otherwise only detect a decimal si-scale
	*/
	enum class SiScaleMode : uint8_t {
		decimal,
		binary,
		optional
	};

	/* si scale description (empty prefix implies no prefix, scale will be 1, otherwise always
	*	size of 1 for decimal, and size of 2 for binary, and scale will be set accordingly) */
	struct SiScale {
		str::Local<char32_t, 2> prefix;
		long double scale = 1.0;
	};

	/* parsed si scale (consumed is zero if no prefix is detected,
	*	scale will be 1, otherwise scale will be set accordingly) */
	struct ParsedSiScale {
		size_t consumed = 0;
		long double scale = 1.0;
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
		static constexpr long double SiScale10[] = {
			1.0e-30, 1.0e-27, 1.0e-24, 1.0e-21, 1.0e-18, 1.0e-15, 1.0e-12, 1.0e-9, 1.0e-6, 1.0e-3,
			1.0e+3, 1.0e+6, 1.0e+9, 1.0e+12, 1.0e+15, 1.0e+18, 1.0e+21, 1.0e+24, 1.0e+27, 1.0e-6
		};
		static constexpr long double SiScale2[] = {
			0x1p-100, 0x1p-90, 0x1p-80, 0x1p-70, 0x1p-60, 0x1p-50, 0x1p-40, 0x1p-30, 0x1p-20, 0x1p-10,
			0x1p+10, 0x1p+20, 0x1p+30, 0x1p+40, 0x1p+50, 0x1p+60, 0x1p+70, 0x1p+80, 0x1p+90, 0x1p-20
		};

		template <class ChType>
		constexpr str::ParsedSiScale ParseSiScale(const std::basic_string_view<ChType>& source, str::SiScaleMode scale) {
			/* extract the first token (implicitly checks for empty strings) */
			auto [cp, consumed] = str::GetCodepoint<err::Nothing>(source);
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
				auto [_cp, _consumed] = str::GetCodepoint<err::Nothing>(source.substr(consumed));

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
			long double value = static_cast<long double>(num);
			if (value < 0)
				value = -value;

			/* fetch the scale to use (equal to number of scales is equal to scale of 1) */
			const long double* scale = (binarySystem ? detail::SiScale2 : detail::SiScale10);
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

	/* match the number to the corresponding si-scale type, such that it has between [1,3] digits before the decimal point
	*	(supported units: Q, R, Y, Z, E, P, T, G, M, k, m, u, n, p, f, a, z, y, r, q; if ascii-only, 'u' is used for micro,
	*	instead of U+00b5; if binary-system, use [2^10, 2^20, ...], otherwise use normal scale [10^3, 10^6, ...]) */
	constexpr str::SiScale SiMakeScale(const str::IsNumber auto& num, bool asciiOnly = false, bool binarySystem = false) {
		using NumType = std::remove_cvref_t<decltype(num)>;
		return detail::GetSiScale<NumType>(num, asciiOnly, binarySystem);
	}

	/* decode the si-scale at the string (note: unique characters are case-insensitive) */
	constexpr str::ParsedSiScale SiPeekScale(const str::IsStr auto& source, str::SiScaleMode scale = str::SiScaleMode::decimal) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };
		return detail::ParseSiScale<ChType>(view, scale);
	}

	/* parse the next number as a float and respect and apply the corresponding si-scale */
	template <str::IsNumber Type>
	constexpr str::ParsedNum SiParseNumTo(const str::IsStr auto& source, Type& num, size_t radix = 10, str::PrefixMode prefix = str::PrefixMode::none, str::SiScaleMode scale = str::SiScaleMode::decimal) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };

		/* parse the number itself */
		long double value = 0.0;
		auto [consumed, result] = str::ParseNumTo(view, value, radix, prefix);
		if (result != str::NumResult::valid && result != str::NumResult::range) {
			num = 0;
			return str::ParsedNum{ 0, result };
		}

		/* check if the value cannot be represented by the corresponding type */
		bool finite = std::isfinite(value);
		if (!finite && str::IsInteger<Type>) {
			num = 0;
			return str::ParsedNum{ 0, str::NumResult::invalid };
		}

		/* check if a negative value has been used for an unsigned value */
		if (value < 0 && !std::is_signed_v<Type>)
			result = str::NumResult::range;

		/* parse the si-prefix (use the output type for the prefix) */
		str::ParsedSiScale parsed = detail::ParseSiScale<ChType>(view.substr(consumed), scale);
		consumed += parsed.consumed;

		/* check if the value is valid and can be scaled based on the si-value */
		if (result == str::NumResult::valid) {
			/* check if the value is trivial, or not finite (can only happen for
			*	floats), in which case the scale does not need to be applied */
			if (value == 0 || !finite) {
				num = Type(value);
				return str::ParsedNum{ consumed, str::NumResult::valid };
			}

			/* apply the scale and check if the value has not overflown/underflown */
			value *= parsed.scale;
			if (value != 0 && std::isfinite(value) && value >= Type(std::numeric_limits<Type>::lowest()) && value <= Type(std::numeric_limits<Type>::max())) {
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
	constexpr str::ParsedNumValue<Type> SiParseNum(const str::IsStr auto& source, size_t radix = 10, str::PrefixMode prefix = str::PrefixMode::none, str::SiScaleMode scale = str::SiScaleMode::decimal) {
		Type num = 0;
		auto [consumed, result] = str::SiParseNumTo<Type>(source, num, radix, prefix, scale);
		return str::ParsedNumValue<Type>{ num, consumed, result };
	}

	/* parse the entire string as number using str::ParseNumTo and respect the corresponding si-scale and return the value,
	*	if the string was fully consumed and fit into the type, and otherwise return the [otherwise] value */
	template <str::IsNumber Type>
	constexpr Type SiParseNumAll(const str::IsStr auto& source, Type otherwise = std::numeric_limits<Type>::max(), size_t radix = 10, str::PrefixMode prefix = str::PrefixMode::none, str::SiScaleMode scale = str::SiScaleMode::decimal) {
		using ChType = str::StringChar<decltype(source)>;
		std::basic_string_view<ChType> view{ source };

		/* parse the number as far as possible */
		Type num = 0;
		auto [consumed, result] = str::SiParseNumTo<Type>(view, num, radix, prefix, scale);

		/* check if no error occurred and the entire string has been consumed */
		if (result == str::NumResult::valid && consumed == view.size())
			return num;
		return otherwise;
	}

	/* print value with optional leading [-] for the given radix with si-scale prefix to the sink and return the sink (using str::FloatTo) */
	constexpr auto& SiValueTo(str::IsSink auto&& sink, const str::IsNumber auto& num, bool asciiOnly = false, bool binarySystem = false, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, str::NumStyle numStyle = str::NumStyle::lower) {
		using NumType = std::remove_cvref_t<decltype(num)>;

		/* setup the scale to be used */
		str::SiScale scale = detail::GetSiScale<NumType>(num, asciiOnly, binarySystem);

		/* write the actual value out and append the si-scale */
		str::FloatTo(sink, static_cast<long double>(num) / scale.scale, style, precision, radix, numStyle);
		return str::FastcodeAllTo<err::Nothing>(sink, scale.prefix);
	}

	/* write the value to an object of the given sink-type using str::SiValueTo and return it */
	template <str::IsSink SinkType>
	constexpr SinkType SiValue(const str::IsNumber auto& num, bool asciiOnly = false, bool binarySystem = false, str::FloatStyle style = str::FloatStyle::general, size_t precision = 0, size_t radix = 10, str::NumStyle numStyle = str::NumStyle::lower) {
		SinkType sink{};
		str::SiValueTo(sink, num, asciiOnly, binarySystem, style, precision, radix, numStyle);
		return sink;
	}
}
