# Decoders

A set of decoders for common compressed file formats, mainly useful for (self) educational purposes.
These decoders are not expected to be fast.

Right now the decoders are based on scodec.

## Zstd

[Zstd](https://github.com/facebook/zstd) is defined in [RFC 8878](https://tools.ietf.org/html/rfc8878).
It's a fairly standard (but cleverly optimized) LZ compression format. It uses Huffman coding for
literals and FSE coding (based on [ANS](https://en.wikipedia.org/wiki/Asymmetric_numeral_systems)) for
encoding of LZ sequences (tuples of literal length, and back reference match length and offsets).

[Implementation](https://github.com/jrudolph/decoders/blob/zstd/src/main/scala/net/virtualvoid/codecs/zstd/Zstd.scala)
