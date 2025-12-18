/* SPDX-License-Identifier: BSD-3-Clause */
/* Copyright (c) 2025 Bjoern Boss Henrichsen */
#include "generated/tests/test-words.h"
#include "generated/tests/test-graphemes.h"
#include "generated/tests/test-sentences.h"
#include "generated/tests/test-normalization.h"
#include "generated/tests/test-lines.h"
#include "generated/tests/test-emoji.h"
#include "ustring.h"

#include <random>
#include <charconv>
#include <chrono>

namespace tester {
	namespace util {
		template <class BreakType>
		class TestRange {
		private:
			struct Lambda {
				util::TestRange<BreakType>& self;
				constexpr Lambda(util::TestRange<BreakType>& s) : self{ s } {}
				constexpr void operator()(const cp::Range& range) {
					if (self.pNext >= self.pReference.size())
						self.pValid = false;
					else if (self.pReference[self.pNext].first != range.first || self.pReference[self.pNext].second != range.last)
						self.pValid = false;
					else
						++self.pNext;
				}
			};

		private:
			const std::vector<std::pair<size_t, size_t>>& pReference;
			typename BreakType::template Type<Lambda> pBreaker;
			size_t pNext = 0;
			size_t pIndex = 0;
			bool pValid = true;

		public:
			TestRange(const BreakType& breaker, const std::vector<std::pair<size_t, size_t>>& ref) : pReference{ ref }, pBreaker{ breaker(Lambda{ *this }) } {}

		public:
			void next(char32_t cp) {
				pBreaker.next(cp, pIndex++);
			}
			bool done() {
				pBreaker.done();
				return (pValid && pNext == pReference.size());
			}
		};
		template <class BeforeCall, class... Args>
		constexpr size_t PreviousEdge(BeforeCall bc, str::CPIterator<char32_t> range, size_t index, const Args&... args) {
			while (bc(range.begin(), range.at(index), range.end(), args...) == cp::BreakKind::none)
				--index;
			return index;
		}
		template <class AfterCall, class... Args>
		constexpr size_t NextEdge(AfterCall ac, str::CPIterator<char32_t> range, size_t index, const Args&... args) {
			while (ac(range.begin(), range.at(index), range.end(), args...) == cp::BreakKind::none)
				++index;
			return index;
		}
		template <class BeforeCall, class AfterCall, class... Args>
		constexpr int8_t TestRangeIt(const str::u32::View& s, size_t index, const std::vector<std::pair<size_t, size_t>>& ranges, BeforeCall bc, AfterCall ac, const Args&... args) {
			/* find the range the current char lies in */
			size_t r = 0;
			while (index > ranges[r].second)
				++r;

			/* check the previous edge */
			if (util::PreviousEdge<BeforeCall, Args...>(bc, s.codepoints(), index, args...) != ranges[r].first)
				return -1;

			/* check the next edge */
			if (util::NextEdge<AfterCall, Args...>(ac, s.codepoints(), index, args...) != ranges[r].second)
				return 1;
			return 0;
		}
		static std::vector<std::pair<size_t, size_t>> LoadRange(std::pair<size_t, size_t> range, const size_t* blob) {
			std::vector<std::pair<size_t, size_t>> out;
			for (size_t i = 0; i < range.second; ++i)
				out.push_back({ blob[range.first + i * 2], blob[range.first + i * 2 + 1] });
			return out;
		}
	}

	static void Emojis() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::EmojiCount; ++i) {
			str::View str = cp::detail::gen::test::EmojiStrings[i];

			/* ensure that all codepoints are detected */
			if (!str.analyze(cp::TestEmoji{ true, true })) {
				str::FmtLn("Emoji error at [{}]: [{:e}]", i, str);
				++errors;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Decomposition() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::NormalizationCount; ++i) {
			str::View src = cp::detail::gen::test::NormalizationSource[i];
			str::View nfc = cp::detail::gen::test::NormalizationComposed[i];
			str::View nfd = cp::detail::gen::test::NormalizationDecomposed[i];

			/* transform all three source strings to the decomposed version */
			std::u32string _src = src.transform<std::u32string>(cp::Decompose{});
			std::u32string _nfc = nfc.transform<std::u32string>(cp::Decompose{});
			std::u32string _nfd = nfd.transform<std::u32string>(cp::Decompose{});

			/* ensure that all three source strings were decomposed properly */
			if (nfd != _src || nfd != _nfc || nfd != _nfd) {
				str::FmtLn("Decomposition error at [{}]: [{:e}]", i, nfd);
				++errors;
				continue;
			}

			/* ensure that the tester detects the decomposition accordingly */
			bool _testSrc = src.analyze(cp::TestDecompose{});
			bool _testNfc = nfc.analyze(cp::TestDecompose{});
			bool _testNfd = nfd.analyze(cp::TestDecompose{});
			if (!_testNfd || (nfd != src && _testSrc) || (nfd != nfc && _testNfc)) {
				str::FmtLn("Decomposition-test error at [{}]: [{:e}]", i, nfd);
				++errors;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Composition() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::NormalizationCount; ++i) {
			str::View src = cp::detail::gen::test::NormalizationSource[i];
			str::View nfc = cp::detail::gen::test::NormalizationComposed[i];
			str::View nfd = cp::detail::gen::test::NormalizationDecomposed[i];

			/* transform all three source strings to the composed version */
			std::u32string _src = src.transform<std::u32string>(cp::Compose{});
			std::u32string _nfc = nfc.transform<std::u32string>(cp::Compose{});
			std::u32string _nfd = nfd.transform<std::u32string>(cp::Compose{});

			/* ensure that all three source strings were composed properly */
			if (nfc != _src || nfc != _nfc || nfc != _nfd) {
				str::FmtLn("Composition error at [{}]: [{:e}]", i, nfd);
				++errors;
				continue;
			}

			/* ensure that the tester detects the composition accordingly */
			bool _testSrc = src.analyze(cp::TestCompose{});
			bool _testNfc = nfc.analyze(cp::TestCompose{});
			bool _testNfd = nfd.analyze(cp::TestCompose{});
			if (!_testNfc || (nfc != src && _testSrc) || (nfc != nfd && _testNfd)) {
				str::FmtLn("Composition-test error at [{}]: [{:e}]", i, nfd);
				++errors;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Graphemes() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::GraphemeCount; ++i) {
			const auto [ptr, len] = cp::detail::gen::test::GraphemeWords[i];
			str::u32::View s{ ptr, len };
			std::vector<std::pair<size_t, size_t>> r = util::LoadRange(cp::detail::gen::test::GraphemeRangesIndex[i], cp::detail::gen::test::GraphemeRangesBlob);

			/* test the ranges-object */
			if (!s.analyze(util::TestRange{ cp::GraphemeRanges{}, r })) {
				str::FmtLn("Grapheme ranges error at [{}]: [{:e}]", i, s);
				++errors;
				continue;
			}

			/* test the forward and backward iterators */
			for (size_t j = 0; j < s.size(); ++j) {
				int8_t res = util::TestRangeIt(s, j, r, cp::GraphemeBefore<str::CPIterator<char32_t>::iterator>, cp::GraphemeAfter<str::CPIterator<char32_t>::iterator>);
				if (res == 0)
					continue;
				if (res == -1)
					str::FmtLn("Grapheme prev-iterator error at [{}:{}]: [{:e}]", i, j, s);
				else
					str::FmtLn("Grapheme next-iterator error at [{}:{}]: [{:e}]", i, j, s);
				++errors;
				break;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Words() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::WordCount; ++i) {
			const auto [ptr, len] = cp::detail::gen::test::WordWords[i];
			str::u32::View s{ ptr, len };
			std::vector<std::pair<size_t, size_t>> r = util::LoadRange(cp::detail::gen::test::WordRangesIndex[i], cp::detail::gen::test::WordRangesBlob);

			/* test the ranges-object */
			if (!s.analyze(util::TestRange{ cp::WordRanges{}, r })) {
				str::FmtLn("Word ranges error at [{}]: [{:e}]", i, s);
				++errors;
				continue;
			}

			/* test the forward and backward iterators */
			for (size_t j = 0; j < s.size(); ++j) {
				int8_t res = util::TestRangeIt(s, j, r, cp::WordBefore<str::CPIterator<char32_t>::iterator>, cp::WordAfter<str::CPIterator<char32_t>::iterator>);
				if (res == 0)
					continue;
				if (res == -1)
					str::FmtLn("Word prev-iterator error at [{}:{}]: [{:e}]", i, j, s);
				else
					str::FmtLn("Word next-iterator error at [{}:{}]: [{:e}]", i, j, s);
				++errors;
				break;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Sentences() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::SentenceCount; ++i) {
			const auto [ptr, len] = cp::detail::gen::test::SentenceWords[i];
			str::u32::View s{ ptr, len };
			std::vector<std::pair<size_t, size_t>> r = util::LoadRange(cp::detail::gen::test::SentenceRangesIndex[i], cp::detail::gen::test::SentenceRangesBlob);

			/* test the ranges-object */
			if (!s.analyze(util::TestRange{ cp::SentenceRanges{}, r })) {
				str::FmtLn("Sentence ranges error at [{}]: [{:e}]", i, s);
				++errors;
				continue;
			}

			/* test the forward and backward iterators */
			for (size_t j = 0; j < s.size(); ++j) {
				int8_t res = util::TestRangeIt(s, j, r, cp::SentenceBefore<str::CPIterator<char32_t>::iterator>, cp::SentenceAfter<str::CPIterator<char32_t>::iterator>);
				if (res == 0)
					continue;
				if (res == -1)
					str::FmtLn("Sentence prev-iterator error at [{}:{}]: [{:e}]", i, j, s);
				else
					str::FmtLn("Sentence next-iterator error at [{}:{}]: [{:e}]", i, j, s);
				++errors;
				break;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Lines() {
		size_t errors = 0;
		for (size_t i = 0; i < cp::detail::gen::test::LineCount; ++i) {
			const auto [ptr, len] = cp::detail::gen::test::LineWords[i];
			str::u32::View s{ ptr, len };
			std::vector<std::pair<size_t, size_t>> r = util::LoadRange(cp::detail::gen::test::LineRangesIndex[i], cp::detail::gen::test::LineRangesBlob);

			/* test the ranges-object */
			if (!s.analyze(util::TestRange{ cp::LineRanges{ cp::LineMode::basic }, r })) {
				str::FmtLn("Line ranges error at [{}]: [{:e}]", i, s);
				++errors;
				continue;
			}

			/* test the forward and backward iterators */
			for (size_t j = 0; j < s.size(); ++j) {
				int8_t res = util::TestRangeIt(s, j, r, cp::LineBefore<str::CPIterator<char32_t>::iterator>, cp::LineAfter<str::CPIterator<char32_t>::iterator>, cp::LineMode::basic);
				if (res == 0)
					continue;
				if (res == -1)
					str::FmtLn("Line prev-iterator error at [{}:{}]: [{:e}]", i, j, s);
				else
					str::FmtLn("Line next-iterator error at [{}:{}]: [{:e}]", i, j, s);
				++errors;
				break;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
	static void Numbers(size_t count) {
		std::mt19937 randEngine{ std::random_device{}() };
		std::uniform_int_distribution<size_t> precision{ 2, 1024 };
		std::uniform_int_distribution<uint64_t> value;
		std::string hexString, numString, hexSelf, numSelf;
		size_t errors = 0;

		for (size_t i = 0; i < count; ++i) {
			/* construct the random but valid double */
			volatile double val = std::bit_cast<double>(value(randEngine));
			if (!std::isfinite(val) || val < std::numeric_limits<double>::min())
				continue;
			volatile size_t prec = precision(randEngine);

			/* construct the hex-string */
			hexString.resize(5120);
			size_t hexCount = std::to_chars(hexString.data(), hexString.data() + hexString.size(), val, std::chars_format::hex, int(prec) - 1).ptr - hexString.data();
			hexString.resize(hexCount);
			double hexCharsDouble = 0.0;
			std::from_chars(hexString.data(), hexString.data() + hexString.size(), hexCharsDouble, std::chars_format::hex);

			/* construct the num-string */
			numString.resize(5120);
			size_t numCount = std::to_chars(numString.data(), numString.data() + numString.size(), val, std::chars_format::scientific, int(prec) - 1).ptr - numString.data();
			numString.resize(numCount);
			double numCharsDouble = 0.0;
			std::from_chars(numString.data(), numString.data() + numString.size(), numCharsDouble);

			/* parse the hex-string */
			volatile auto hexResult = str::ParseNum<double>(hexString, { .radix = str::HexFloat });
			double hexDouble = hexResult.value;
			if (std::bit_cast<uint64_t>(hexDouble) != std::bit_cast<uint64_t>(hexCharsDouble)) {
				str::PrintLn("hex-parsing-difference:");
				str::PrintLn("    [", hexCharsDouble, " | 0x", str::As{ U"x", std::bit_cast<uint64_t>(hexCharsDouble) }, ']');
				str::PrintLn("    [", hexDouble, " | 0x", str::As{ U"x", std::bit_cast<uint64_t>(hexDouble) }, ']');
				str::PrintLn("");
				++errors;
			}

			/* parse the num-string */
			volatile auto numResult = str::ParseNum<double>(numString);
			double numDouble = numResult.value;
			if (std::bit_cast<uint64_t>(numDouble) != std::bit_cast<uint64_t>(numCharsDouble)) {
				str::PrintLn("num-parsing-difference:");
				str::PrintLn("    [", numCharsDouble, " | 0x", str::As{ U"x", std::bit_cast<uint64_t>(numCharsDouble) }, ']');
				str::PrintLn("    [", numDouble, " | 0x", str::As{ U"x", std::bit_cast<uint64_t>(numDouble) }, ']');
				str::PrintLn("");
				++errors;
			}

			/* construct the hex-string and num-string */
			hexSelf = str::Float<std::string>(val, { .precision = prec, .radix = str::HexFloat, .fltStyle = str::FloatStyle::scientific });
			numSelf = str::Float<std::string>(val, { .precision = prec, .fltStyle = str::FloatStyle::scientific });

			/* compare the hex-strings */
			if (hexString != hexSelf && (hexString[0] != '2' || hexSelf[0] != '1')) {
				str::PrintLn("hex-string:");
				str::PrintLn("    [", hexString, ']');
				str::PrintLn("    [", hexSelf, ']');
				str::PrintLn("");
				++errors;
			}

			/* compare the num-strings */
			if (numString != numSelf) {
				str::PrintLn("num-string:");
				str::PrintLn("    [", numString, ']');
				str::PrintLn("    [", numSelf, ']');
				str::PrintLn("");
				++errors;
			}
		}
		str::PrintLn("Errors: ", errors);
	}
}

namespace compare {
	namespace util {
		class Timer {
		private:
			std::chrono::time_point<std::chrono::high_resolution_clock> pStart;

		public:
			Timer() : pStart{ std::chrono::high_resolution_clock::now() } {}

		public:
			float stop() const {
				std::chrono::time_point stop = std::chrono::high_resolution_clock::now();
				return (std::chrono::duration_cast<std::chrono::microseconds>(stop - pStart).count() / 1000000.0f);
			}
		};
	}

	static void VsSNPrintf(size_t count, size_t precision, double value) {
		std::string buf(precision + 16, '\0');

		/* construct the format strings */
		std::string fmt0 = str::ch::Build("{:.", precision, "e}");
		std::string fmt1 = str::ch::Build("%.", precision - 1, "e");

		/* time the str-implementation */
		util::Timer t0;
		for (size_t i = 0; i < count; ++i)
			str::FormatTo(str::NullChars{ buf.data(), buf.size() }, fmt0.c_str(), value);
		float first = t0.stop();

		/* time the snprintf-implementation */
		util::Timer t1;
		for (size_t i = 0; i < count; ++i)
			std::snprintf(buf.data(), buf.size(), fmt1.c_str(), value);
		float second = t1.stop();

		/* log the results */
		str::PrintLn("str::FormatTo    : ", first, 's');
		str::PrintLn("std::snprintf    : ", second, 's');
	}
	static void VsToCharsAndConvert(size_t count, size_t precision, double value) {
		std::u16string buf(precision + 16, u'\0');
		std::string temp(precision + 16, '\0');

		/* time the str-implementation */
		util::Timer t0;
		for (size_t i = 0; i < count; ++i)
			str::FloatTo(str::NullChars{ buf.data(), buf.size() }, value, { .precision = precision, .fltStyle = str::FloatStyle::scientific });
		float first = t0.stop();

		/* time the to_chars+transcode-implementation */
		util::Timer t1;
		for (size_t i = 0; i < count; ++i) {
			size_t count = std::to_chars(temp.data(), temp.data() + temp.size(), value, std::chars_format::scientific, int(precision) - 1).ptr - temp.data();
			str::TranscodeAllTo(str::NullChars{ buf.data(), buf.size() }, std::string_view(temp.data(), count));
		}
		float second = t1.stop();

		/* log the results */
		str::PrintLn("str::FloatTo      : ", first, 's');
		str::PrintLn("to_chars+Transcode: ", second, 's');
	}
	static void VsToString(size_t count, double value) {
		std::string tmp;

		/* time the str-implementation */
		util::Timer t0;
		for (size_t i = 0; i < count; ++i)
			tmp = str::Float<std::string>(value, { .radix = 10, .fltStyle = str::FloatStyle::fixed });
		float first = t0.stop();

		/* time the to_string-implementation */
		util::Timer t1;
		for (size_t i = 0; i < count; ++i)
			tmp = std::to_string(value);
		float second = t1.stop();

		/* log the results */
		str::PrintLn("str::Float        : ", first, 's');
		str::PrintLn("std::to_string    : ", second, 's');
	}
	static void VsFromChars(size_t count, const std::string& num, bool hex) {
		double d = 0.0;

		/* time the str-implementation */
		util::Timer t0;
		for (size_t i = 0; i < count; ++i)
			d = str::ParseNum<double>(num, { .radix = (hex ? str::HexFloat : 10) }).value;
		float first = t0.stop();

		/* time the from_chars-implementation */
		util::Timer t1;
		for (size_t i = 0; i < count; ++i)
			std::from_chars(num.data(), num.data() + num.size(), d, (hex ? std::chars_format::hex : std::chars_format::general));
		float second = t1.stop();

		/* log the results */
		str::PrintLn("str::ParseNum     : ", first, 's');
		str::PrintLn("std::from_chars   : ", second, 's');
	}
}

int main(int argc, char** argv) {
	std::setlocale(LC_ALL, "utf-8");

	str::PrintLn("-------- Emoji Test --------");
	tester::Emojis();

	str::PrintLn("-------- Decomposition Test --------");
	tester::Decomposition();

	str::PrintLn("-------- Composition Test --------");
	tester::Composition();

	str::PrintLn("-------- Graphemes Test --------");
	tester::Graphemes();

	str::PrintLn("-------- Words Test --------");
	tester::Words();

	str::PrintLn("-------- Sentences Test --------");
	tester::Sentences();

	str::PrintLn("-------- Lines Test --------");
	tester::Lines();

	str::PrintLn("-------- Numbers Test (random) --------");
	tester::Numbers(0x200000);

	str::PrintLn("-------- Compare Format/snprintf (short) --------");
	compare::VsSNPrintf(0x400000, 50, 1.0e204);

	str::PrintLn("-------- Compare Format/snprintf (long) --------");
	compare::VsSNPrintf(0x400000, 650, 1.0e204);

	str::PrintLn("-------- Compare FloatTo/to_chars+Transcode to char16_t (short) --------");
	compare::VsToCharsAndConvert(0x400000, 50, 1.0e204);

	str::PrintLn("-------- Compare FloatTo/to_chars+Transcode to char16_t (long) --------");
	compare::VsToCharsAndConvert(0x400000, 650, 1.0e204);

	str::PrintLn("-------- Compare Float/to_string --------");
	compare::VsToString(0x400000, 1234.12312312301923171);

	str::PrintLn("-------- Compare ParseNum/from_chars (dec) --------");
	compare::VsFromChars(0x400000, "1234.123123123019231712839346E+100", false);

	str::PrintLn("-------- Compare ParseNum/from_chars (hex) --------");
	compare::VsFromChars(0x400000, "124.1923f01p-8", true);
	return 0;
}
