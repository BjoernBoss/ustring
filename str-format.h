#pragma once

#include "str-common.h"
#include "str-convert.h"

#include <string>
#include <type_traits>
#include <charconv>
#include <utility>
#include <vector>
#include <limits>

namespace str {

	///* default buildable-specialization for strings */
	//template <str::AnyString Type>
	//struct Buildable<Type> {
	//	constexpr void operator()(const Type& t, str::AnySink auto& sink) const {
	//		str::Append(sink, t);
	//	}
	//};

	//template <std::floating_point Type>
	//struct Buildable<Type> {
	//	constexpr void operator()(Type t, str::AnySink auto& sink) const {
	//		if (std::isinf(t))
	//			str::Append(sink, (t < 0 ? "-Inf" : "Inf"));
	//		else if (std::isnan(t))
	//			str::Append(sink, "NaN");
	//		else {
	//			char buffer[64] = { 0 };

	//			/* buffer is large enough to hold all numbers so no need to check for errors */
	//			char* end = std::to_chars(buffer, std::end(buffer), t, std::chars_format::general).ptr;
	//			str::Append(sink, std::basic_string_view<char>{ buffer, end });
	//		}
	//	}
	//template <>
	//struct Buildable<void*> {
	//	constexpr void operator()(void* t, str::AnySink auto& sink) const {
	//		char buffer[64] = { 0 };

	//		/* buffer is large enough to hold all numbers so no need to check for errors */
	//		char* end = std::to_chars(buffer, std::end(buffer), reinterpret_cast<uintptr_t>(t), 16).ptr;
	//		for (size_t i = (end - buffer); i < sizeof(void*) * 2; ++i)
	//			str::Append(sink, '0');
	//		str::Append(sink, std::basic_string_view<char>{ buffer, end });
	//	}
	//};
	//template <str::AllBuildable TypeA, str::AllBuildable TypeB>
	//struct Buildable<std::pair<TypeA, TypeB>> {
	//	constexpr void operator()(const std::pair<TypeA, TypeB>& t, str::AnySink auto& sink) const {
	//		str::Buildable<TypeA>{}(t.first, sink);
	//		str::Append(sink, ", ");
	//		str::Buildable<TypeB>{}(t.second, sink);
	//	}
	//};
	//template <str::AllBuildable Type>
	//struct Buildable<std::vector<Type>> {
	//	constexpr void operator()(const std::vector<Type>& t, str::AnySink auto& sink) const {
	//		for (size_t i = 0; i < t.size(); ++i) {
	//			if (i > 0)
	//				str::Append(sink, ", ");
	//			str::Buildable<Type>{}(t[i], sink);
	//		}
	//	}
	//};



	template <class Type>
	struct Formatter;

	/* does type specialize buildable struct for given character or for all characters */
	template <class Type>
	concept IsFormattable = requires(const Type & t, const std::u32string_view & fmt, str::ChSmall<1> cs, str::WdSmall<1> ws, str::U8Small<1> u8s, str::U16Small<1> u16s, str::U32Small<1> u32s) {
		typename str::Formatter<Type>;
		{ str::Formatter<Type>{}(t, cs, fmt) };
		{ str::Formatter<Type>{}(t, ws, fmt) };
		{ str::Formatter<Type>{}(t, u8s, fmt) };
		{ str::Formatter<Type>{}(t, u16s, fmt) };
		{ str::Formatter<Type>{}(t, u32s, fmt) };
	};

	namespace detail {
		template <class _Unused>
		constexpr void Format(auto& sink, size_t index, const std::u32string_view& fmt) {}

		template <class _Unused, class Arg, class... Args>
		constexpr void Format(auto& sink, size_t index, const std::u32string_view& fmt, const Arg& arg, const Args&... args) {
			if (index > 0)
				detail::Format<_Unused, Args...>(sink, index - 1, fmt, args...);
			else
				str::Formatter<Arg>{}(arg, sink, fmt);
		}

		template <class ChType>
		constexpr bool FastFormattingPossible() {
			/* check if the type is a unicode-variation, in which case fast formatting is possible */
			if constexpr (!std::is_same_v<ChType, char>)
				return true;

			/* check if the single char is a unicode-variation, in which case the fast formatting is possible */
			if constexpr (!std::is_same_v<detail::MBEquivalence, char>)
				return true;

			/* check that char encodes the argument-bracket characters as the ascii brackets */
			char test[] = { "{}" };
			char32_t expected[] = { U"{}" };

			constexpr size_t expCount = (sizeof(expected) / sizeof(char32_t));
			if (sizeof(test) / sizeof(char) != expCount)
				return false;

			size_t i = 0;
			while (i < expCount && static_cast<uint8_t>(test[i]) == static_cast<uint32_t>(expected[i]))
				++i;

			return (i == expCount);
		}

		template <class FmtType, class SinkType, bool Strict>
		constexpr bool FormatPrintUntilArg(auto& sink, std::basic_string_view<FmtType>& fmt) {
			/* fast formatting relies on the '{' and '}' characters being single char-values using the ascii representations (true for all unicode-encodings) */
			constexpr bool fastMode = (std::is_same_v<FmtType, SinkType> && !Strict && detail::FastFormattingPossible<FmtType>());

			/* iterate until the entire format-string has been processed or until an argument has been encountered */
			bool openStarted = false, closeStarted = false;
			while (!fmt.empty()) {
				char32_t cp = 0;
				size_t len = 1;
				bool valid = false;

				/* check if this is fast-mode, in which case the next character can just be
				*	fetched from the source and upcasted (only if the next character has size 1) */
				if constexpr (fastMode) {
					auto [_len, result] = str::LengthNext<str::Relaxed>(fmt, true);
					valid = (result == str::DecResult::valid);
					if (valid && _len == 1)
						cp = static_cast<char32_t>(static_cast<uint32_t>(std::make_unsigned_t<FmtType>(fmt[0])));
					len = _len;
				}
				else {
					/* decode the next character (handle invalid decodings as valid characters, just not written to the sink) */
					auto [_len, _cp, result] = str::Decode<std::conditional_t<Strict, str::Strict, str::Relaxed>>(fmt, true);
					if (valid = (result == str::DecResult::valid))
						cp = _cp;
					len = _len;
				}

				/* check if an open-bracket has been encountered, which could either be part of an escape
				*	sequence or mark the start of an argument (discard any unescaped closing-brackets) */
				if (cp == U'{') {
					closeStarted = false;
					openStarted = !openStarted;
				}

				/* check if an opening bracket was the last token, in which case this must have been a valid argument start */
				else if (openStarted)
					return true;

				/* check if a closing-bracket is being escaped */
				else if (cp == U'}')
					closeStarted = !closeStarted;

				/* check if the token should be committed to the sink (ignore any characters not being writable to the destination character-set) */
				if (!openStarted && !closeStarted && valid) {
					if constexpr (std::is_same_v<FmtType, SinkType>)
						sink.append(fmt.substr(0, len));
					else
						str::EncodeInto<str::Relaxed>(sink, cp);
				}
				fmt = fmt.substr(len);
			}
			return false;
		}

		template <class Arg, class... Args>
		constexpr void Append(auto& sink, const Arg& arg, const Args&... args) {
			str::Formatter<Arg>{}(arg, sink, std::u32string_view{});
			if constexpr (sizeof...(args) > 0)
				detail::Append<Args...>(sink, args...);
		}
	}

	/* format the arguments into the sink, based on the formatting-string */
	template <str::IsMode Mode = str::Relaxed>
	constexpr auto& FormatInto(str::AnySink auto& sink, const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		using FmtType = str::StringChar<decltype(fmt)>;
		using SinkType = str::SinkChar<decltype(sink)>;
		enum class ArgState : uint8_t {
			preDigit,
			inDigit,
			inArgFmt,
			done
		};

		/* buffer the format-string until the entire format has been consumed */
		size_t nextArgument = 0, argFormatLen = 0;
		std::u32string argFormatBuffer;
		std::basic_string_view<FmtType> view{ fmt };

		/* print all characters from the format string until the next argument starts */
		while (detail::FormatPrintUntilArg<FmtType, SinkType, str::IsStrict<Mode>>(sink, view)) {
			size_t argIndex = 0, argFormatLen = 0;

			/* parse the next argument description */
			bool openStarted = false, closeStarted = false;
			ArgState state = ArgState::preDigit;
			while (!view.empty()) {
				/* decode the next character (handle invalid decodings as valid characters, just not written to the sink) */
				auto [len, cp, result] = str::Decode<Mode>(view, true);
				bool valid = (result == str::DecResult::valid);

				/* check if the current index needs to be updated */
				if (state != ArgState::inArgFmt) {
					/* check if an end has been found and either mark all previous characters as consumed or fetch the next index in order */
					if (cp < U'0' || cp > U'9') {
						if (state == ArgState::preDigit || cp != U':')
							argIndex = nextArgument++;
						else {
							argFormatLen = 0;
							valid = false;
						}
						state = ArgState::inArgFmt;
					}

					/* update the currently computed index (but still add the characters to the buffer as the index might not be formatted properly) */
					else {
						argIndex = argIndex * 10 + static_cast<size_t>(cp - U'0');
						state = ArgState::inDigit;
					}
				}

				/* check if the character is part of the argument string and update the bracket-escape counting */
				if (state == ArgState::inArgFmt) {
					/* check if a closing bracket has started (discard any single opening brackets) */
					if (cp == U'}') {
						openStarted = false;
						closeStarted = !closeStarted;
					}

					/* check if a closing bracket was the last token, in which case this is the first token after
					*	the closing bracket and does not need to be consumed or processed further for this argument */
					else if (closeStarted) {
						state = ArgState::done;
						break;
					}

					/* check if an opening-bracket is being escaped */
					else if (cp == U'{')
						openStarted = !openStarted;
				}

				/* remove the string from the view and check if it shoould be added to the argument string */
				view = view.substr(len);
				if (openStarted || closeStarted || !valid)
					continue;
				if (argFormatLen >= argFormatBuffer.size())
					argFormatBuffer.resize(argFormatLen + 1);
				argFormatBuffer[argFormatLen++] = cp;
			}

			/* check if the last token was a closing bracket, in which case this must have been a valid argument just at the end of the format string */
			if (closeStarted)
				state = ArgState::done;

			/* check if a valid argument has been found and the index is valid, in which case the argument and be printed */
			if (state == ArgState::done && argIndex < sizeof...(args))
				detail::Format<void>(sink, argIndex, std::u32string_view{ argFormatBuffer.data(), argFormatLen }, args...);
		}
		return sink;
	}

	/* format the arguments to a string of the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType, str::IsMode Mode = str::Relaxed>
	constexpr std::basic_string<ChType> Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		std::basic_string<ChType> out{};
		return str::FormatInto<Mode>(out, fmt, args...);
	}

	/* format the arguments to a string of the destination character-type (returning str::Small<Capacity>) */
	template <str::IsChar ChType, intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::Small<ChType, Capacity> Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		str::Small<ChType, Capacity> out{};
		return str::FormatInto<Mode>(out, fmt, args...);
	}

	/* build the arguments into the sink (as if formatting with format "{}{}{}...") */
	constexpr auto& BuildInto(str::AnySink auto& sink, const str::IsFormattable auto&... args) {
		if constexpr (sizeof...(args) > 0)
			detail::Append(sink, args...);
		return sink;
	}

	/* build the arguments to a string of the destination character-type (returning std::basic_string) */
	template <str::IsChar ChType>
	constexpr std::basic_string<ChType> Build(const str::IsFormattable auto&... args) {
		std::basic_string<ChType> out{};
		return str::BuildInto(out, args...);
	}

	/* build the arguments to a string of the destination character-type (returning str::Small<Capacity>) */
	template <str::IsChar ChType, intptr_t Capacity>
	constexpr str::Small<ChType, Capacity> Build(const str::IsFormattable auto&... args) {
		str::Small<ChType, Capacity> out{};
		return str::BuildInto(out, args...);
	}

	/* convenience for fast formatting to a std::basic_string */
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::string ChFormat(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char, Mode>(fmt, args...);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::wstring WdFormat(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<wchar_t, Mode>(fmt, args...);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u8string U8Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char8_t, Mode>(fmt, args...);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u16string U16Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char16_t, Mode>(fmt, args...);
	}
	template <str::IsMode Mode = str::Relaxed>
	constexpr std::u32string U32Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char32_t, Mode>(fmt, args...);
	}

	/* convenience for fast formatting to a str::Small<Capacity> */
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::ChSmall<Capacity> ChFormat(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char, Capacity, Mode>(fmt, args...);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::WdSmall<Capacity> WdFormat(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<wchar_t, Capacity, Mode>(fmt, args...);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U8Small<Capacity> U8Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char8_t, Capacity, Mode>(fmt, args...);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U16Small<Capacity> U16Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char16_t, Capacity, Mode>(fmt, args...);
	}
	template <intptr_t Capacity, str::IsMode Mode = str::Relaxed>
	constexpr str::U32Small<Capacity> U32Format(const str::AnyString auto& fmt, const str::IsFormattable auto&... args) {
		return str::Format<char32_t, Capacity, Mode>(fmt, args...);
	}

	/* convenience for fast building to a std::basic_string */
	constexpr std::string ChBuild(const str::IsFormattable auto&... args) {
		return str::Build<char>(args...);
	}
	constexpr std::wstring WdBuild(const str::IsFormattable auto&... args) {
		return str::Build<wchar_t>(args...);
	}
	constexpr std::u8string U8Build(const str::IsFormattable auto&... args) {
		return str::Build<char8_t>(args...);
	}
	constexpr std::u16string U16Build(const str::IsFormattable auto&... args) {
		return str::Build<char16_t>(args...);
	}
	constexpr std::u32string U32Build(const str::IsFormattable auto&... args) {
		return str::Build<char32_t>(args...);
	}

	/* convenience for fast formatting to a str::Small<Capacity> */
	template <intptr_t Capacity>
	constexpr str::ChSmall<Capacity> ChBuild(const str::IsFormattable auto&... args) {
		return str::Build<char, Capacity>(args...);
	}
	template <intptr_t Capacity>
	constexpr str::WdSmall<Capacity> WdBuild(const str::IsFormattable auto&... args) {
		return str::Build<wchar_t, Capacity>(args...);
	}
	template <intptr_t Capacity>
	constexpr str::U8Small<Capacity> U8Build(const str::IsFormattable auto&... args) {
		return str::Build<char8_t, Capacity>(args...);
	}
	template <intptr_t Capacity>
	constexpr str::U16Small<Capacity> U16Build(const str::IsFormattable auto&... args) {
		return str::Build<char16_t, Capacity>(args...);
	}
	template <intptr_t Capacity>
	constexpr str::U32Small<Capacity> U32Build(const str::IsFormattable auto&... args) {
		return str::Build<char32_t, Capacity>(args...);
	}

	/*
	*	parse standard formatting-string
	*
	*	padding: (.)?(<|^|>)?(\d+)(\.\d+)?(,\d+)
	*	[0]: fill-character (optional; default: ' ')
	*	[1]: fill-side (optional; leading,center,trailing; default: '<')
	*	[2]: min-size (if not found, no padding used; cannot be zero; default: 0)
	*	[3]: precision (optional; if type allows for it; default: 0)
	*	[4]: max-size (optional; will be increased if smaller than min-size; default: max-size)
	*	numbers might overwrite padding-side if padding-character is '0'
	*
	*	nested: (.*)(|(.*))?
	*	[0]: normal formatting considered (must exist)
	*	[1]: optional nested formatting (optional; if type allows for it; default: '')
	*
	*	options: additional options passed in, of which the first occurrence counts
	*		chars[n] = option[n][...] or 0, if not found
	*		chars[sizeof(options)] is padding-character
	*
	*	strict: (!|\?)? if contains any of them, should perform any char-formatting as strict
	*	[0]: If group == '?', use '?' as replacement character for invalid encodings, else no replacement
	*/
	enum class PaddingSide : uint8_t {
		leading,
		trailing,
		center
	};
	template <size_t OptCount>
	struct FormatParseOut {
		std::u32string_view nested;
		char32_t chars[OptCount + 1] = { 0 };
		size_t length = 0;
		size_t maximum = std::numeric_limits<size_t>::max();
		size_t precision = 0;
		str::PaddingSide side = str::PaddingSide::leading;
		bool strict = false;
		bool replace = false;
	};
	template <size_t OptCount>
	str::FormatParseOut<OptCount> ParseFormatting(const std::u32string_view& fmt, bool precision, bool nested, const std::u32string_view(&options)[OptCount]) {
		str::FormatParseOut<OptCount> out{};
		size_t size = 0;

		/* check if the formatting may contain a nested formatting-string and look it up */
		if (nested) {
			while (size < fmt.size() && fmt[size] != U'|')
				++size;
			out.nested = fmt.substr(size + 1);
		}
		else
			size = fmt.size();

		/* look for the start of the padding (first digit greater than 0) */
		size_t start = 0;
		for (; start < size; ++start) {
			if (fmt[start] >= U'1' && fmt[start] <= U'9')
				break;
		}
		size_t end = start;

		/* check if a number has been found and parse the numbers */
		if (start < size) {
			/* parse the length of the padding as well as the precision and maximum length */
			for (size_t i = 0; i < 3; ++i) {
				/* check if another number should be parsed */
				bool nextPrecision = false;
				if (i > 0) {
					if (end + 1 >= size || fmt[end + 1] < U'1' || fmt[end + 1] > U'9')
						break;
					if (fmt[end] == U'.' && i == 1 && precision)
						nextPrecision = true;
					else if (fmt[end] != U',')
						break;
					++end;
				}

				/* parse the next number */
				size_t number = 0;
				for (; end < size; ++end) {
					if (fmt[end] < U'0' || fmt[end] > U'9')
						break;
					number = number * 10 + static_cast<size_t>(fmt[end] - U'0');
				}

				/* assign the number to the right property */
				if (i == 0)
					out.length = number;
				else if (nextPrecision)
					out.precision = number;
				else {
					out.maximum = std::max<size_t>(out.length, number);
					break;
				}
			}

			/* check if the padding side has been overwritten */
			if (start > 0 && (fmt[start - 1] == U'<' || fmt[start - 1] == U'^' || fmt[start - 1] == U'>')) {
				--start;
				if (fmt[start] == U'<')
					out.side = str::PaddingSide::leading;
				else if (fmt[start] == U'^')
					out.side = str::PaddingSide::center;
				else
					out.side = str::PaddingSide::trailing;
			}

			/* check if the padding character has been overwritten */
			if (start > 0)
				out.chars[OptCount] = fmt[--start];
		}

		/* iterate over the remaining characters and check if they pick any of the options */
		for (size_t i = 0; i < size; ++i) {
			/* check if the current index lies within the padding-string */
			if (i >= start && i < end) {
				i = end - 1;
				continue;
			}

			/* check if the character is the strict/strict-with-replacement option */
			if (!out.strict && (fmt[i] == U'!' || fmt[i] == U'?')) {
				out.strict = true;
				out.replace = (fmt[i] == U'?');
				continue;
			}

			/* check if the character is one of the options */
			for (size_t j = 0; j < OptCount; ++j) {
				if (out.chars[j] != 0 || options[j].find(fmt[i]) == std::u32string_view::npos)
					continue;
				out.chars[j] = fmt[i];
				break;
			}
		}
		return out;
	}

	/* write the string and optionally enforce the limitations and perform the padding */
	template <bool Format, str::IsChar ChType, size_t OptCount>
	void WriteFormattedString(const str::FormatParseOut<OptCount>& parsed, str::AnySink auto& sink, const std::basic_string_view<ChType>& str) {
		/* limit the string to the boundary */
		std::basic_string_view<ChType> limited = str.substr(0, Format ? parsed.maximum : str.size());

		/* compute the padding characters to be written */
		size_t leading = 0, trailing = 0;
		if (Format && limited.size() < parsed.length) {
			size_t left = parsed.length - limited.size();

			if (parsed.side == str::PaddingSide::leading)
				leading = left;
			else if (parsed.side == str::PaddingSide::center)
				leading = (left / 2);
			trailing = left - leading;
		}

		/* write the leading-padding characters */
		if constexpr (Format) {
			for (size_t i = 0; i < leading; ++i)
				str::WriteFormattedString<false>(parsed, sink, std::u32string_view{ &parsed.chars[OptCount], 1 });
		}

		/* write the string itself */
		if (parsed.strict)
			str::ConvertInto<str::Strict>(sink, limited, parsed.replace ? '?' : 0);
		else
			str::ConvertInto<str::Copy>(sink, limited, 0);

		/* write the trailing-padding characters */
		if constexpr (Format) {
			for (size_t i = 0; i < trailing; ++i)
				str::WriteFormattedString<false>(parsed, sink, std::u32string_view{ &parsed.chars[OptCount], 1 });
		}
	}




	template <str::AnyString Type>
	struct Formatter<Type> {
		constexpr void operator()(const Type& t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			str::ConvertInto(sink, L"{fmt: \"");
			str::ConvertInto(sink, fmt);
			str::ConvertInto(sink, L"\"; payload: \"");
			str::ConvertInto(sink, t);
			str::ConvertInto(sink, L"\"}");
		}
	};

	/*
	*	padding-formatting rules for numbers
	*	[bB] [qQ] [oO] [dD] [xX] for base and casing
	*	[#] for prefix (of 0 and given type)
	*	[ +] for replacement character if number is not signed
	*/
	template <std::integral Type> struct Formatter<Type> {
		constexpr void operator()(Type t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			char buffer[64] = { 0 };

			/* parse the formatting string */
			str::FormatParseOut<3> parsed = str::ParseFormatting(fmt, false, false, { U"bBqQoOdDxX", U"#", U"+ " });

			/* extract the base to be used */
			int base = 10;
			switch (parsed.chars[0]) {
			case U'b':
			case U'B':
				base = 2;
				break;
			case U'q':
			case U'Q':
				base = 4;
				break;
			case U'o':
			case U'O':
				base = 8;
				break;
			case U'd':
			case U'D':
				base = 10;
				break;
			case U'x':
			case U'X':
				base = 16;
				break;
			};
			bool baseUppercase = (parsed.chars[0] == U'X');

			/* remove the sign of the number (as it might have to be inserted before the padding) */
			bool sign = (t < 0);
			if constexpr (std::is_signed_v<Type>) {
				if (sign)
					t = -t;
			}

			/* print the number to the string (buffer large enough for any size of number) */
			char* start = std::end(buffer);
			do {
				*(--start) = (baseUppercase ? "0123456789ABCDEF" : "0123456789abcdef")[t % base];
				t /= base;
			} while (t != 0);

			/* write the sign and prefix to the buffer (no matter if the '0'-padding needs to be inserted inbetween) */
			char* prefix = start;
			if (parsed.chars[1] == U'#') {
				*(--prefix) = "__b_q___o_d_____x"[base];
				*(--prefix) = '0';
			}
			if (sign)
				*(--prefix) = '-';
			else if (parsed.chars[2] != 0)
				*(--prefix) = (parsed.chars[2] == U'+' ? '+' : ' ');
			size_t total = (std::end(buffer) - prefix);

			/* check if this is the special case of the 0 padding, in which case the prefix has to be written first */
			if (parsed.chars[3] == U'0' && total < parsed.maximum) {
				str::WriteFormattedString<false>(parsed, sink, std::string_view{ prefix, start });
				for (size_t i = total; i < parsed.length; ++i)
					str::WriteFormattedString<false>(parsed, sink, std::u32string_view{ U"0" });
				str::WriteFormattedString<false>(parsed, sink, std::string_view{ start, std::end(buffer) });
			}

			/* write the entire formatted string out */
			else
				str::WriteFormattedString<true>(parsed, sink, std::string_view{ prefix, total });
		}
	};

	/*
	*	[!] to enfore strict transcoding rules
	*	[?] to use '?' as replacement character if character cannot be encoded
	*/
	template <> struct Formatter<char> {
		constexpr void operator()(char t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			char rep = (fmt.find(U'?') != std::u32string_view::npos ? '?' : 0);
			if (fmt.find(U'!') != std::u32string_view::npos)
				str::ConvertInto<str::Strict>(sink, std::basic_string_view{ &t, 1 }, rep);
			else
				str::ConvertInto<str::Copy>(sink, std::basic_string_view{ &t, 1 }, rep);
		}
	};
	template <> struct Formatter<wchar_t> {
		constexpr void operator()(wchar_t t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			char rep = (fmt.find(U'?') != std::u32string_view::npos ? '?' : 0);
			if (fmt.find(U'!') != std::u32string_view::npos)
				str::ConvertInto<str::Strict>(sink, std::basic_string_view{ &t, 1 }, rep);
			else
				str::ConvertInto<str::Copy>(sink, std::basic_string_view{ &t, 1 }, rep);
		}
	};
	template <> struct Formatter<char8_t> {
		constexpr void operator()(char8_t t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			char rep = (fmt.find(U'?') != std::u32string_view::npos ? '?' : 0);
			if (fmt.find(U'!') != std::u32string_view::npos)
				str::ConvertInto<str::Strict>(sink, std::basic_string_view{ &t, 1 }, rep);
			else
				str::ConvertInto<str::Copy>(sink, std::basic_string_view{ &t, 1 }, rep);
		}
	};
	template <> struct Formatter<char16_t> {
		constexpr void operator()(char16_t t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			char rep = (fmt.find(U'?') != std::u32string_view::npos ? '?' : 0);
			if (fmt.find(U'!') != std::u32string_view::npos)
				str::ConvertInto<str::Strict>(sink, std::basic_string_view{ &t, 1 }, rep);
			else
				str::ConvertInto<str::Copy>(sink, std::basic_string_view{ &t, 1 }, rep);
		}
	};
	template <> struct Formatter<char32_t> {
		constexpr void operator()(char32_t t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			char rep = (fmt.find(U'?') != std::u32string_view::npos ? '?' : 0);
			if (fmt.find(U'!') != std::u32string_view::npos)
				str::ConvertInto<str::Strict>(sink, std::basic_string_view{ &t, 1 }, rep);
			else
				str::ConvertInto<str::Copy>(sink, std::basic_string_view{ &t, 1 }, rep);
		}
	};

	/*
	*	padding-formatting rules (for numbers, if no [sS] option is used)
	*	[sS] to print [true] and [false] instead of [0] and [1] and casing
	*/
	template <> struct Formatter<bool> {
		constexpr void operator()(bool t, str::AnySink auto& sink, const std::u32string_view& fmt) const {
			/* extract the string to be used */
			size_t len = 1;
			const char* str = (t ? "1" : "0");
			for (size_t i = 0; i < fmt.size(); ++i) {
				if (fmt[i] != U's' && fmt[i] != U'S')
					continue;
				if (fmt[i] == U'S')
					str = (t ? "True" : "False");
				else
					str = (t ? "true" : "false");
				len = (t ? 4 : 5);
				break;
			}

			///* parse the padding */
			//str::PaddingStyle pad = str::ParsePadding(fmt, (len == 1));

			///* check if the string needs to be added before the padding */
			//if (pad.trailing)
			//	str::ConvertInto(sink, std::basic_string_view{ str, len });

			///* add the padding */
			//for (size_t i = len; i < pad.length; ++i)
			//	str::ConvertInto(sink, std::basic_string_view{ &pad.character, 1 });

			///* check if the string needs to be added after the padding */
			//if (!pad.trailing)
			//	str::ConvertInto(sink, std::basic_string_view{ str, len });
		}
	};
}
