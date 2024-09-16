# Unicode String Functions for C++
![C++](https://img.shields.io/badge/language-c%2B%2B20-blue?style=flat-square)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-brightgreen?style=flat-square)](LICENSE.txt)

Header only library written in `C++20` to add support for various unicode operations and interoperability between all string types. Simply include [`ustring.h`](ustring.h) to add the entire functionality.

It includes functions to perform various unicode testing, segmentation, transformations, and analysis functions. Further, it adds the ability to parse and print numbers from any string type, format strings to any string type, serialize and deserialize string types to bytes, escape strings, and convert string types to each other as correctly as possible.

The library consideres any object to be convertible to a `string_view` of any type as a valid string.

## Using the library
This library is a header only library. Simply clone the repository, and include `<ustring/ustring.h>`

    $ git clone https://github.com/BjoernBoss/ustring.git

## Requirements for String Types
The library does have some requirements for the underlying types, which are also checked statically.

    char       Must have a size of 1 byte and either use utf-8 encoding or any multibyte encoding
    wchar_t    Must have a size of 2 or 4 bytes and either use utf-16 or utf-32 encoding
    char8_t    Must have a size of 1 byte and use utf-8 encoding
    char16_t   Must have a size of 2 bytes and use utf-16 encoding
    char32_t   Must have a size of 4 bytes and use utf-32 encoding

In case of `char` using utf-8 encoding, the decoding/encoding will be performed using the internal utf-8 coding functionalities. Otherwise `std::mbrtowc` and `str::wctomb` will be used. Similarly, for `wchar_t`, it will either use the internal utf-16 or utf-32 coding functions. For further optimizations, the library tries to determine at compile time, if the multibyte `char` maps all ascii-characters correctly. It exposes these properties in code:

    str::CharIsUtf8      Does the char encoding use utf-8, otherwise an unknown multibyte encoding
    str::CharHoldsAscii  Does the char encoding match ascii for the first 128 chars
    str::WideIsUtf16     Does the wchar_t encoding use utf-16
    str::WideIsUtf32     Does the wchar_t encoding use utf-32

Disclaimer: while this should work in theory, at least in `Visual Studio 2022` using the `execution-charset: IBM01149` still resulted in `a` being mapped to `0x61`, although it should have been mapped to `0x81`, which in turn resulted in the compile time ascii-detection to fail.

## Concepts and Interfaces
The library defines a set of concepts, which it frequently uses.

 - `str::IsChar` Check if the type is a supported character (`char`, `wchar_t`, ...)
 - `str::IsStr` Check if the type can be converted to a `string_view` (use `str::StringChar` to get the corresponding character-type)
 - `str::IsChStr<T>` Check if the type can be converted to a `basic_string_view<T>`
 - `str::IsData` Check if the type specialized the `str::ByteReader` (used for `str::FromWire` and such)
 - `str::IsSink` Check if the type specialized the `str::CharWriter` interface (used by all writing functions; use `str::SinkChar` to get the corresponding character-type)
 - `str::IsWire` Check if the type specialized the `str::ByteWriter` interface (used for `str::ToWire` and such)
 - `str::IsCharLoader` Check if the type specialized the `str::CharLoader` interface (used for character streaming such as `str::WireIn`)
 - `str::IsStream` Check if the type fulfills `str::IsStr` or `str::IsCharLoader` (used for `str::Stream` and such)
 - `str::IsByteLoader` Check if the type specialized the `str::ByteLoader` interface (used for byte streaming such as `str::WireIn`)
 - `str::IsSource` Check if the type fulfills `str::IsData` or `str::IsByteLoader` (used for `str::Source` and such)
 - `str::IsFormattable` Check if the type specialized the str::Formatter interface
 - `str::IsIterator` Check if the type is a codepoint iterator
 - `str::IsReceiver<T...>` Check if the type can receive the types through its call operator
 - `str::IsCollector` Check if the type has a `next(char32_t)` and `done()` method to act as recipient for transformed codepoints
 - `str::IsMapper` Check if the type is a transformation and can instantiate a new `str::IsCollector` when receiving a `str::IsCollector` to write its output to
 - `str::IsAnalysis` Check if the type performs a boolean analysis on a stream of codepoints
 - `str::IsTester` Check if the type performs a boolean analysis on a single codepoint

## Sinks, Sources, and Streams

By default the library uses the concepts of sinks or wires to write to, and strings or data to read from. A sink is anything that fulfills `str::IsSink`, for which many default specializations exist. For convenience, further implementations, such as `str::NullChars`, `str::Chars`, and `str::InheritSink` exist, to allow using buffers and virtualized sinks. A sink defines its supported character type, and all operations, which write to the sink, will ensure only the proper type of characters is written out. Further, no partially encoded codepoints will be written out. They will always be written out in a single batch.

A wire is defined in the same way as `str::IsWire`, and also offers `str::Bytes` and `str::InheritWire` as support.

A source for strings is anything that is convertible to an `std::basic_string_view`. For bytes, it is any type that fulfills `str::IsData`, for which many default specializations exist. Similarly to sinks, strings also define their source character type, which also defines their encoding used.

To further support streaming of data, `str::IsStream` exists for character streams, and `str::IsSource` for byte streams. In order to use them properly, `str::Stream` or `str::Source` exist, which wrap the type, and build up internal cached states to prevent repeated fetching of data, and allowing lookaheads.

## Standard Functionality
The standard string functionality lies in the namespace `str` for all string-related operations.

To allow local strings from being used, `str::Local<class CharType, ssize_t Capacity>` is provided, which allows to create a string on the stack of the given character type and the given capacity. For convenience, `str::LocalCh` / `str::LocalWd` / ... has been defined for each corresponding character type.

The idea of the library is to offer two functions of any kind, such as `str::Int` and `str::IntTo`, where the first function returns a new string-object of the result of the function, while the `To`-function writes the result to the character sink, which is passed in as the first argument.

### [str::String, str::View, str::UString, str::UView](str-ustring.h)
The primary functionality is combined into the `str::String` or `str::View` wrapper. They extend `std::basic_string` or `std::basic_string_view` accordingly, and extend it with the new functionality. They both provide the same interface, and will therefore only be referenced through `str::View` here. This functionality includes various testing functions, transformations, normalizations, and unicode normalized or case-folded comparisons. As an example:

```C++
    std::u16string s = str::View{ L"abc-def" }.norm().to<std::u16string>();
    bool t = str::View{ U"\U0001F9D1\u200D\U0001F680" }.isEmoji();
```

For convenience, `str::UString` is defined as `str::String<char16_t, str::err::DefChar>` and `str::UView` is defined as `str::View<char16_t, str::err::DefChar>` to be used as default string types.

### [str::Float, str::Int, str::ParseNum](str-number.h)
The number functions can parse any kind of number and produce float or integer-strings for any valid radix. The functions themselves can only be used with ascii-numbers. In order to use it for any other decimal-representation, such as arabic-indic digit `\u0664`, use the convenience function `str::View::asciiDecimals` function. Examples for interacting with numbers:

```C++
    std::wstring s = str::Float<std::wstring>(50.0f, str::FloatStyle::general);
    float f = str::ParseNum<float>(u8"-1523.23e+5").value;
```

### [str::Format, str::Build](str-format.h), [str::Fmt, str::Print](ustring.h)
For interacting with formatting, `str::Format` and `str::Build` exist, where `str::Build` is effectively defined as a format using the format-string `"{}{}{}..."`. Otherwise the default formatting rules mostly apply. Comments per type exist. Examples of using these:

```C++
    std::u16string s = str::Format<std::u16string>(u8"Test: {:.^#10x}\n", 65536);
    std::wstring t = str::Build<std::wstring>(1, true, "abc", u8'-', U"def");

    /* using an argument to modify the argument-string dynamically (will effectively use: #020x on 512) */
    std::string u = str::Format<std::string>(U"abc: {:#0{}{3}}\n", 512, 20, "unused", 'x');
```

For convenience, `str::Fmt`, `str::FmtLn`, `str::Print`, `str::PrintLn` exist to either format or build directly out to `std::cout`, or `std::wcout`, when using `str::FmtW`, ...

### [str::ToWire, str::FromWire](str-wire.h)
To encode or decode strings to raw bytes, the `str::ToWire` and `str::FromWire` exist. They both optionally add a `BOM` or try to determine the encoding type, based on an encountered `BOM`. Examples of using the functions:

```C++
    /* reading from a byte-source */
    uint8_t someBuffer[512] = /* ... */;
    auto x = str::FromWire{ str::WireCoding::utf8, str::BOMMode::detectAll };
    x.read<std::string>(someBuffer, 512);

    /* writing to a byte-source */
    std::ofstream someFile = /* ... */;
    auto y = str::ToWire{ str::WireCoding::utf16, true };
    y.write(someFile, u8"Some-String");
    y.write(someFile, U" some other string");
```

Note: For convenience, `str::WireOut` exists as valid sink to feed the string output directly to a corresponding wire object. Similarly, `str::WireIn` exists to create a character-stream from a byte-source and immediately transcode the characters.

```C++
    str::ofstream someFile = /* ... */;

    str::FormatTo(str::WireOut{ someFile, str::WireCoding::utf16le }, "abc: {:#0{}x}\n", 12345, 19);
```

### [str::Stream](str-chars.h), [str::Source](str-bytes.h)
The `str::Stream` object creates a wrapper around a type, which implements the character-source `str::IsStream`. `str::Source` does the same, but for any type, which implements the byte-source interface `str::IsSource`. Example of using the stream:

```C++
    str::Stream stream{ u8"Some-String" };
    while (!stream.done()) {
        str::PrintLn(stream.load(64));
        stream.consume();
    }
```

The `str::U32Stream` wrapper implements a stream, which allows to create a stream, which decodes any source stream to a stream of type char32_t. The `str::LimitStream` and `str::LimitSource` wrapper implement a source or stream, which limits the number of tokens to be consumed.

Similarly, `str::InheritSource` and `str::InheritStream` exist, alongside with `str::SourceImplementation` and `str::StreamImplementation`. These allow to create virtualized sources or streams, thus preventing to use templates everywhere.

### [str::Iterator](str-coding.h)
The `str::Iterator` provides a codepoint iterator, which allows iteration both forward and backward over the encoded codepoints. The iterator can immediately be instantiated through `str::View::it`. Example of using the iterators:

```C++
    auto it = str::View{ u"abc-def" }.it();
    while (it.next())
        foo(it.get());
```

## [Coding Error Handling](str-coding.h)
Any encoding or decoding errors will be handled according to the `CodeError` template parameter. Most corresponding functions in the `str` namespace will have the `CodeError` parameter, which is defaulted to `str::err::DefChar`. The following values are defined:

    str::err::Throw     Throw an error if an encoding or decoding error is encountered
    str::err::Nothing   Return str::Invalid as codepoint if an error is encountered
    str::err::Skip      Skip any invalid codepoints and ignore them
    %any%               Replace any invalid codepoints with the current value
                            => This also includes str::err::DefChar, which defaults to '?'

## Unicode Functionality
The unicode related operations lie in the namespace `cp`. All codepoints are represented using `char32_t` as type.

### [cp::prop, cp::ascii](unicode/cp-property.h)
The `cp::prop` and `cp::ascii` namespaces contain various test functions to query properties per single codepoint. Most functions are directly integrated into `str::View`. The `IsUpper` or `IsLower` functions should not be used to determine if the string is uppercased or lowercased, as they only perform checks based on the unicode properties, without respecting any surrouunding context. Examples of using the properties:

```C++
    cp::prop::GCType gc = cp::prop::GetCategory(U'a');
    bool s = cp::prop::IsAssigned(U'\U0010ff00');
    size_t t = cp::ascii::GetRadix(U'9');
```

### [cp::UpperCase, cp::LowerCase, cp::TitleCase, cp::FoldCase](unicode/cp-casing.h)
The objects for transforming the casing of the string all adhere to the `str::IsMapper` concept. They take an optional locale as constructor argument, and can then be used to instantiate a mapper object into any sink. For each of the transformations, a corresponding tester exists, such as `cp::TestUpperCase`, which checks if the given string would not be modified by the corresponding mapper anymore. All of these functions are directly introduced to `str::View`. Example usage:

```C++
    /* mapping a string to uppercase */
    auto it0 = str::View{ u"abc-def" }.it();
    auto it1 = it0;
    std::string out;
    auto map = cp::UpperCase{ L"en_us" }(str::Collect(out));
    while (it0.next())
        map.next(it0.get());
    map.done();

    /* mapping a string to lowercase */
    auto tester = cp::TestUpperCase{ 0 };
    while (it1.next())
        tester.next(it1.get());
    bool t = tester.done();
```

Note: For convenience, `str::Collect` and `str::ForEach` exist as valid collectors to feed the output of the mappers either directly to a sink-object or to a lambda function.

### [cp::Decompose, cp::Compose, cp::NormFold](unicode/cp-normalization.h)
Similar to the casing functionalities, the transformations to convert a stream of codepoints to the normalized composed `NFC` or decomposed versions `NFD` exist, as well as to convert the stream to a case-folded and decomposed stream.

### [Graphemes, Words, Sentences, Lines](unicode/cp-segmentation.h)
For each of the kinds of separations, there exist three implementations. For graphemes, for example: `cp::GraphemeBreak`, `cp::GraphemeRanges`, `cp::GraphemeIterator`.

The Break object allows to instantiate an iterator, which produces the corresponding break-type between any two codepoints. The Ranges object will produce a list of ranges of break-type none, as well as the last break-type before starting the given range. Both of these work on the correspoinding stream of codepoints from front to back, but are optimized for this. They take the codepoint and corresponding index, and will reference all results using the index. This allows strings to be used, which might require more than one source-character to encode a codepoint. Example usage:

```C++
    std::u32string str = U"abc def";
    std::vector<cp::Range> ranges;

    auto words = cp::WordRanges{}([&](const cp::Range& r) { ranges.push_back(r); });
    for (size_t i = 0; i < str.size(); ++i)
        words.next(str[i], i);
    words.done();
```

The Iterator object will allow to perform arbitrary iteration starting at any point and return the crossed break-type on every step, but it is less efficient than the other two alternatives. It operates based on a codepoint iterator passed to it. For convenience, the `str::View` expose functions to directly produce the iterator objects. Example usage:

```C++
    auto it = str::View{ "abc def" }.it(4);

    auto words = cp::WordIterator{ it };
    cp::BreakMode brk = words.prev();
```

## Unicode Data and Properties
The properties per codepoint are fetched from the `Unicode Character Base`. All required files will be downloaded to `generated/ucd` or `generated/tests`. To fetch the latest release, run:

    $ py generated/generate.py

This will download all relevant files, parse them, and generate all corresponding files to `generated/unicode-*`. Both the generation script and all functions using the generated functions will perform static checks of the current state, to ensure as best as possible that the used algorithms are still valid.
