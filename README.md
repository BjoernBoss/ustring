# Unicode String Functions for C++
![C++](https://img.shields.io/badge/language-c%2B%2B20-blue?style=flat-square)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-brightgreen?style=flat-square)](LICENSE.txt)

Header only library written in `C++20` to add support for various unicode operations and interoperability between all string types. Simply include [`str.h`](str.h) to add the entire functionality. 

It includes functions to perform various unicode testing, segmentation, transformations, and analysis functions. Further, it adds the ability to parse and print numbers from any string type, format strings to any string type, serialize and deserialize string types to bytes, escape strings, and convert string types to each other as correctly as possible.

## Using the library
This library is a header only library. Simply clone the repository, and include `<str-tools/str.h>`

    $ git clone https://github.com/BjoernBoss/str-tools.git

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

Disclaimer: while this should work in theory, at least in `Visual Studio 2019` using the `execution-charset: IBM01149` still resulted in `a` being mapped to `0x61`, although it should have been mapped to `0x81`, which in turn resulted in the compile time ascii-detection to fail.

## Functionalities
The functionality is split up into two namespaces: `str` for all string-related operations, and `cp` for all unicode related operations. All codepoints are represented using `char32_t` as type.

To allow local strings from being used, `str::Local<class CharType, ssize_t Capacity>` is provided, which allows to create a string on the stack of the given character type and the given capacity.

The idea of the library is to offer two functions of any kind, such as `str::Int` and `str::IntTo`, where the first function returns a new string-object of the result of the function, while the `To`-function writes the result to the character sink, which is passed in as the first argument.

The primary functionality is combined into the `str::UStr` or `str::UView` wrapper. They extend `std::basic_string` or `std::basic_string_view` accordingly, and extend it with the new functionality. As an example:

    std::u16string s = str::UView{ L"abc-def" }.norm().to<std::u16string>();

For convenience, `str::UString` is defined as `str::UStr<char16_t>`.

Examples for interacting with numbers:

    std::wstring s = str::Float<std::wstring>(50.0f, str::FloatStyle::general);
    float f = str::ParseNum<float>(u8"-1523.23e+5");

For interacting with formatting, `str::Format` and `str::Build` exist, where `str::Build` is effectively defined as a format using the format-string `"{}{}{}..."`. Otherwise the default formatting rules mostly apply. Comments per type exist. Examples of using these:

    std::u16string s = str::Format<std::u16string>(u8"Test: {:^#10x}\n", 65536);
    std::wstring t = str::Build<std::wstring>(1, true, "abc", u8'-', U"def");

For convenience, `str::Fmt`, `str::FmtLn`, `str::Out`, `str::OutLn` exist to either format or build directly out to `std::cout`, or `std::wcout`, when using `str::WFmt`, ...



## Unicode Data and Properties
The properties per codepoint are fetched from the `Unicode Character Base`. All required files will be downloaded to `generated/ucd` or `generated/tests`. To fetch the latest release, run:

    $ py generated/generate.py

This will download all relevant files, parse them, and generate all corresponding files to `generated/unicode-*`. Both the generation script and all functions using the generated functions will perform static checks of the current state, to ensure as best as possible that the used algorithms are still valid.
