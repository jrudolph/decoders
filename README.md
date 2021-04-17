# Decoders

A set of decoders for common compressed file formats, mainly useful for (self) educational purposes.
These decoders are not expected to be fast.

Right now the decoders are based on scodec.

## Zstd

[Zstd](https://github.com/facebook/zstd) is defined in [RFC 8878](https://tools.ietf.org/html/rfc8878).
It's a fairly standard (but cleverly optimized) [LZ](https://en.wikipedia.org/wiki/LZ77_and_LZ78) compression format.
It uses Huffman coding for literals and [FSE](http://fastcompression.blogspot.com/2013/12/finite-state-entropy-new-breed-of.html)
coding (based on [ANS](https://en.wikipedia.org/wiki/Asymmetric_numeral_systems)) for encoding of LZ sequences (tuples of
literal length, and back reference match length and offsets).

[Implementation](https://github.com/jrudolph/decoders/blob/main/src/main/scala/net/virtualvoid/codecs/zstd/Zstd.scala)

The implementation is somewhat complete, some rarer cases are not yet handled:

 * some particular multibyte size formats
 * RLE literals mode
 * RLE sequence mode
 * skip frames
 * dictionaries

### Speed

On a single core (4800H) about 5-20MB/s on JDK8 vs. 2500-4000MB/s zstd 1.4.8, so ~ 100-1000x slower than an optimized version.

## Gzip

Gzip is defined in [RFC 1952](https://tools.ietf.org/html/rfc1952). It is a special wrapper around DEFLATE ([RFC 1951](https://tools.ietf.org/html/rfc1951))
encoded data. Deflate is *the* lossless text compression algorithm of the last decades. It's an LZ implementation that uses Huffman codes
for compression of literals and back references.

[Implementation](https://github.com/jrudolph/decoders/blob/main/src/main/scala/net/virtualvoid/codecs/gzip/Gzip.scala)

The implementation is incomplete but the most important bits are there.
