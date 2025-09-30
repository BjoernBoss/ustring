/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2024-2025 Bjoern Boss Henrichsen */
#pragma once

#include "common/str-local.h"
#include "common/str-bytes.h"
#include "common/str-chars.h"
#include "coding/str-coding.h"
#include "format/str-number.h"
#include "format/str-sivalue.h"
#include "format/str-wire.h"
#include "format/str-escape.h"

#include "str-helper.h"
#include "str-specializations.h"

#include "format/str-format.h"
#include "unicode/cp-casing.h"
#include "unicode/cp-normalization.h"
#include "unicode/cp-property.h"
#include "unicode/cp-segmentation.h"
#include "str-string.h"

namespace str {
	/* convenience for fast formatting to std::cout */
	constexpr void Fmt(const str::IsStr auto& fmt, const str::IsFormattable auto&... args) {
		str::FormatTo(std::cout, fmt, args...);
	}
	constexpr void FmtLn(const str::IsStr auto& fmt, const str::IsFormattable auto&... args) {
		str::FormatTo(std::cout, fmt, args...);
		std::cout << '\n';
	}

	/* convenience for fast formatting to std::wcout */
	constexpr void FmtW(const str::IsStr auto& fmt, const str::IsFormattable auto&... args) {
		str::FormatTo(std::wcout, fmt, args...);
	}
	constexpr void FmtWLn(const str::IsStr auto& fmt, const str::IsFormattable auto&... args) {
		str::FormatTo(std::wcout, fmt, args...);
		std::wcout << L'\n';
	}

	/* convenience for fast building to std::cout */
	constexpr void Print(const str::IsFormattable auto&... args) {
		str::BuildTo(std::cout, args...);
	}
	constexpr void PrintLn(const str::IsFormattable auto&... args) {
		str::BuildTo(std::cout, args...);
		std::cout << '\n';
	}

	/* convenience for fast building to std::wcout */
	constexpr void PrintW(const str::IsFormattable auto&... args) {
		str::BuildTo(std::wcout, args...);
	}
	constexpr void PrintWLn(const str::IsFormattable auto&... args) {
		str::BuildTo(std::wcout, args...);
		std::wcout << L'\n';
	}

	namespace detail {
		template <class ChType>
		struct StrConvenience {
			template <intptr_t Capacity>
			using Local = str::Local<ChType, Capacity>;

			using View = str::View<ChType, str::CodeError::replace>;

			using String = str::String<ChType, str::CodeError::replace>;

			static constexpr std::basic_string<ChType> Build(const str::IsFormattable auto&... args) {
				return str::Build<std::basic_string<ChType>>(args...);
			}
			static constexpr std::basic_string<ChType> Format(const str::IsStr auto& fmt, const str::IsFormattable auto&... args) {
				return str::Format<std::basic_string<ChType>>(fmt, args...);
			}
			static constexpr std::basic_string<ChType> To(const str::IsStr auto& source) {
				return str::FastcodeAll<std::basic_string<ChType>>(source);
			}
			static constexpr std::basic_string<ChType> Safe(const str::IsStr auto& source) {
				return str::TranscodeAll<std::basic_string<ChType>>(source);
			}
			static constexpr std::basic_string<ChType> Int(const str::IsInteger auto& num, const str::ArgsInt& args = {}) {
				return str::Int<std::basic_string<ChType>>(num, args);
			}
			static constexpr std::basic_string<ChType> Float(const str::IsFloat auto& num, const str::ArgsFloat& args = {}) {
				return str::Float<std::basic_string<ChType>>(num, args);
			}
			static constexpr std::basic_string<ChType> SiValue(const str::IsNumber auto& num, const str::ArgsSiValue& args = {}) {
				return str::SiValue<std::basic_string<ChType>>(num, args);
			}
		};

		template <class ChType, intptr_t Capacity>
		struct LocConvenience {
			using Local = str::Local<ChType, Capacity>;

			static constexpr str::Local<ChType, Capacity> Build(const str::IsFormattable auto&... args) {
				return str::Build<str::Local<ChType, Capacity>>(args...);
			}
			static constexpr str::Local<ChType, Capacity> Format(const str::IsStr auto& fmt, const str::IsFormattable auto&... args) {
				return str::Format<str::Local<ChType, Capacity>>(fmt, args...);
			}
			static constexpr str::Local<ChType, Capacity> To(const str::IsStr auto& source) {
				return str::FastcodeAll<str::Local<ChType, Capacity>>(source);
			}
			static constexpr str::Local<ChType, Capacity> Safe(const str::IsStr auto& source) {
				return str::TranscodeAll<str::Local<ChType, Capacity>>(source);
			}
			static constexpr str::Local<ChType, Capacity> Int(const str::IsInteger auto& num, const str::ArgsInt& args = {}) {
				return str::Int<str::Local<ChType, Capacity>>(num, args);
			}
			static constexpr str::Local<ChType, Capacity> Float(const str::IsFloat auto& num, const str::ArgsFloat& args = {}) {
				return str::Float<str::Local<ChType, Capacity>>(num, args);
			}
			static constexpr str::Local<ChType, Capacity> SiValue(const str::IsNumber auto& num, const str::ArgsSiValue& args = {}) {
				return str::SiValue<str::Local<ChType, Capacity>>(num, args);
			}
		};
	}

	/* convenience for bound types */
	using ch = detail::StrConvenience<char>;
	using wd = detail::StrConvenience<wchar_t>;
	using u8 = detail::StrConvenience<char8_t>;
	using u16 = detail::StrConvenience<char16_t>;
	using u32 = detail::StrConvenience<char32_t>;
	template <intptr_t Capacity>
	using lch = detail::LocConvenience<char, Capacity>;
	template <intptr_t Capacity>
	using lwd = detail::LocConvenience<wchar_t, Capacity>;
	template <intptr_t Capacity>
	using lu8 = detail::LocConvenience<char8_t, Capacity>;
	template <intptr_t Capacity>
	using lu16 = detail::LocConvenience<char16_t, Capacity>;
	template <intptr_t Capacity>
	using lu32 = detail::LocConvenience<char32_t, Capacity>;

	/* convenience to build an runtime exception */
	struct BuildException : public str::RuntimeException {
		template <class... Args>
		constexpr BuildException(const Args&... args) : str::RuntimeException{ str::Build<std::wstring>(args...) } {}
	};

	/* default string-type for convenience [utf-16] */
	using ustring = str::String<char16_t, str::CodeError::replace>;

	/* default string-view-type for convenience [utf-16] */
	using uview = str::View<char16_t, str::CodeError::replace>;
}
