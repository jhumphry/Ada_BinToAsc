# Ada_BinToAsc

## Introduction

This is an Ada 2012 project that implements various binary-to-ASCII
codecs such as Base64. The goal is to create a flexible, well-tested
library that fulfils most needs.

The library is licensed under the permissive ISC license without any
warranties - see the LICENSE file for details.

## Simple usage

The simplest way to use the library is via the `RFC4648` package. This
contains various instantiations of generic packages that are intended
to implement the requirements of
[RFC4648](http://tools.ietf.org/html/rfc4648) for Base16, Base64 etc.
The `To_String` functions in each of the instantiations convert a
`System.Storage_Elements.Storage_Array` to a string formatted according
to the relevant standard and the `To_Bin` function does the reverse.
The `To_Bin` function will raise an `Invalid_Data_Encoding` exception
if the input is not encoded correctly.

## Other uses

If the standard instantiations are not sufficient then the package can
be customised. First the `BinToAsc` package has to be instantiated with
suitable parameters for the binary type to be used. The package only
supports 8-bit modular types as the binary type. Then one of the
`BinToAsc` child packages can be instantiated with the relevant
alphabet, padding and/or case-sensitivity parameters to give the
required conversion functions.

If encoding or decoding has to be done in a streaming or incremental
fashion then objects of types derived from a root `Codec` type have to
be used to maintain the en/decoder state. Every `Codec'Class` object
has a `State` field that is an enumeration type `Ready`, `Failed` or
`Completed`. The `State` should be checked after every conversion to
identify if errors have arisen. A `Codec` should not be used again
after it has transitioned to the `Failed` or `Completed` states unless
it is `Reset`.

To do incremental encoding or decoding, call one of the `Process`
procedures repeatedly with the `Codec`, the input data, an output
buffer and a place to store the amount of the output buffer used. The
output buffer used by any call can vary, as the supported encodings all
convert a fixed-size group of binary data into a fixed-size group of
character data or vice-versa. The `Input_Group_Size` and
`Output_Group_Size` functions of the `Codec` object will let you know
how large the output buffer has to be. An extra `Output_Group_Size` of
output buffer space is generally required to account for any buffered
data accumulated in the `Codec` object from prior operations. To
complete the coding or decoding, call the `Complete` procedure which
will potentially output any remaining data received but not converted
by the `Codec`, together with any padding required.

## Variants supported

There have been many varieties of Base64 and similar codecs used in
products and protocols. The main focus of this project is the variants
suggested by RFC4648. Both `Base16` and `Base32` can be instantiated in
case-sensitive or case-insensitive variants.

In addition, `Base32` also has an option to allow homoglyphs. When
activated, the character '0' in input will be interpreted as 'o' or 'O',
if the supplied alphabet does not already assign a base-32 digit to
'0' but does to 'o' or 'O', and the character '1' will be interpreted
as 'l' or 'I' likewise. This can be useful where Base32 data may be
typed manually from a print-out, as in some typefaces these characters
are so visually similar that they are hard to distinguish.

Some `Base64` decoders will ignore junk characters or incorrect padding
etc. Currently this is not supported.

## Build process and tests

Three `gprbuild` project files are included. `ada_bintoasc.gpr`
compiles the library and takes a `mode` option that can be `debug`,
`optimize` or `coverage`. The `ada_bintoasc_external.gpr` file can be
used instead if you want to prevent recompilation of the library.

The `ada_bintoasc_tests.gpr` compiles `bintoasc_example` which
demonstrates simple usage of the library and `bintoasc_tests` which
runs a series of unit tests. The unit tests have complete coverage of
the library with the exception of the error path in `To_String` which
cannot easily be triggered without causing an out-of-memory situation
or other tricks not suitable for unit tests.
