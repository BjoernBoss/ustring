#pragma once

#include "str-common.h"
#include "str-convert.h"

#include <string>
#include <type_traits>
#include <charconv>
#include <utility>
#include <vector>

namespace str {
	template <class Type>
	struct Buildable;

	/* does type specialize buildable struct for given character or for all characters */
	template <class Type, class ChType>
	concept IsBuildable = requires(const Type & t, std::basic_string<ChType> sink0, str::Small<ChType, 1, false> sink1) {
		typename str::Buildable<Type>;
		{ str::Buildable<Type>{}(t, sink0) };
		{ str::Buildable<Type>{}(t, sink1) };
	};
	template <class Type>
	concept AllBuildable = (str::IsBuildable<Type, char> && str::IsBuildable<Type, wchar_t> && str::IsBuildable<Type, char8_t> && str::IsBuildable<Type, char16_t> && str::IsBuildable<Type, char32_t>);

	/* default buildable-specialization for strings */
	template <str::AnyString Type>
	struct Buildable<Type> {
		constexpr void operator()(const Type& t, str::AnySink auto& sink) const {
			str::Append(sink, t);
		}
	};
	template <std::integral Type>
	struct Buildable<Type> {
		constexpr void operator()(Type t, str::AnySink auto& sink) const {
			char buffer[64] = { 0 };

			/* buffer is large enough to hold all numbers so no need to check for errors */
			char* end = std::to_chars(buffer, std::end(buffer), t, 10).ptr;
			str::Append(sink, std::basic_string_view<char>{ buffer, end });
		}
	};
	template <std::floating_point Type>
	struct Buildable<Type> {
		constexpr void operator()(Type t, str::AnySink auto& sink) const {
			if (std::isinf(t))
				str::Append(sink, (t < 0 ? "-Inf" : "Inf"));
			else if (std::isnan(t))
				str::Append(sink, "NaN");
			else {
				char buffer[64] = { 0 };

				/* buffer is large enough to hold all numbers so no need to check for errors */
				char* end = std::to_chars(buffer, std::end(buffer), t, std::chars_format::general).ptr;
				str::Append(sink, std::basic_string_view<char>{ buffer, end });
			}
		}
	};
	template <>
	struct Buildable<bool> {
		constexpr void operator()(bool t, str::AnySink auto& sink) const {
			str::Append(sink, t ? "true" : "false");
		}
	};
	template <>
	struct Buildable<void*> {
		constexpr void operator()(void* t, str::AnySink auto& sink) const {
			char buffer[64] = { 0 };

			/* buffer is large enough to hold all numbers so no need to check for errors */
			char* end = std::to_chars(buffer, std::end(buffer), reinterpret_cast<uintptr_t>(t), 16).ptr;
			for (size_t i = (end - buffer); i < sizeof(void*) * 2; ++i)
				str::Append(sink, '0');
			str::Append(sink, std::basic_string_view<char>{ buffer, end });
		}
	};
	template <>
	struct Buildable<char> {
		constexpr void operator()(char t, str::AnySink auto& sink) const {
			str::Append(sink, t);
		}
	};
	template <>
	struct Buildable<wchar_t> {
		constexpr void operator()(wchar_t t, str::AnySink auto& sink) const {
			str::Append(sink, t);
		}
	};
	template <>
	struct Buildable<char8_t> {
		constexpr void operator()(char8_t t, str::AnySink auto& sink) const {
			str::Append(sink, t);
		}
	};
	template <>
	struct Buildable<char16_t> {
		constexpr void operator()(char16_t t, str::AnySink auto& sink) const {
			str::Append(sink, t);
		}
	};
	template <>
	struct Buildable<char32_t> {
		constexpr void operator()(char32_t t, str::AnySink auto& sink) const {
			str::Append(sink, t);
		}
	};
	template <str::AllBuildable TypeA, str::AllBuildable TypeB>
	struct Buildable<std::pair<TypeA, TypeB>> {
		constexpr void operator()(const std::pair<TypeA, TypeB>& t, str::AnySink auto& sink) const {
			str::Buildable<TypeA>{}(t.first, sink);
			str::Append(sink, ", ");
			str::Buildable<TypeB>{}(t.second, sink);
		}
	};
	template <str::AllBuildable Type>
	struct Buildable<std::vector<Type>> {
		constexpr void operator()(const std::vector<Type>& t, str::AnySink auto& sink) const {
			for (size_t i = 0; i < t.size(); ++i) {
				if (i > 0)
					str::Append(sink, ", ");
				str::Buildable<Type>{}(t[i], sink);
			}
		}
	};

	namespace detail {
		template <class ChType, class Arg, class... Args>
		constexpr void Append(str::IsSink<ChType> auto& sink, const Arg& arg, const Args&... args) {
			str::Buildable<std::remove_cvref_t<Arg>>{}(arg, sink);
			if constexpr (sizeof...(args) > 0)
				detail::Append<ChType, Args...>(sink, args...);
		}
	}

	/* build the upcoming list of arguments into the give sink (appending to it, using the buildable-specializations) */
	template <str::IsChar ChType>
	void BuildInto(str::IsSink<ChType> auto& sink, const str::IsBuildable<ChType> auto&... args) {
		if constexpr (sizeof...(args) > 0)
			detail::Append<ChType>(sink, args...);
	}

	/* build the upcoming list of arguments into a small-string and return it */
	template <str::IsChar ChType, size_t Capacity, bool SilentError = false>
	str::Small<ChType, Capacity, SilentError> Build(const str::IsBuildable<ChType> auto&... args) {
		str::Small<ChType, Capacity, SilentError> out{};
		str::BuildInto<ChType>(out, args...);
		return out;
	}

	/* build the upcoming list of arguments into a std::string and return it */
	template <str::IsChar ChType, str::IsBuildable<ChType>... Args>
	std::basic_string<ChType> Build(const Args&... args) {
		std::basic_string<ChType> out{};
		str::BuildInto<ChType>(out, args...);
		return out;
	}

	/* convenience for fast usage of small-string building */
	template <size_t Capacity, bool SilentError = false>
	str::ChSmall<Capacity, SilentError> ChBuild(const str::IsBuildable<char> auto&... args) {
		return str::Build<char, Capacity, SilentError>(args...);
	}
	template <size_t Capacity, bool SilentError = false>
	str::WdSmall<Capacity, SilentError> WdBuild(const str::IsBuildable<wchar_t> auto&... args) {
		return str::Build<wchar_t, Capacity, SilentError>(args...);
	}
	template <size_t Capacity, bool SilentError = false>
	str::U8Small<Capacity, SilentError> U8Build(const str::IsBuildable<char8_t> auto&... args) {
		return str::Build<char8_t, Capacity, SilentError>(args...);
	}
	template <size_t Capacity, bool SilentError = false>
	str::U16Small<Capacity, SilentError> U16Build(const str::IsBuildable<char16_t> auto&... args) {
		return str::Build<char16_t, Capacity, SilentError>(args...);
	}
	template <size_t Capacity, bool SilentError = false>
	str::U32Small<Capacity, SilentError> U32Build(const str::IsBuildable<char32_t> auto&... args) {
		return str::Build<char32_t, Capacity, SilentError>(args...);
	}

	/* convenience for fast usage of std::string building */
	std::basic_string<char> ChBuild(const str::IsBuildable<char> auto&... args) {
		return str::Build<char>(args...);
	}
	std::basic_string<wchar_t> WdBuild(const str::IsBuildable<wchar_t> auto&... args) {
		return str::Build<wchar_t>(args...);
	}
	std::basic_string<char8_t> U8Build(const str::IsBuildable<char8_t> auto&... args) {
		return str::Build<char8_t>(args...);
	}
	std::basic_string<char16_t> U16Build(const str::IsBuildable<char16_t> auto&... args) {
		return str::Build<char16_t>(args...);
	}
	std::basic_string<char32_t> U32Build(const str::IsBuildable<char32_t> auto&... args) {
		return str::Build<char32_t>(args...);
	}
}
