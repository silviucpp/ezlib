# ezlib

*An zlib NIF library for Erlang optimised for streaming. Around 50% - 60% faster than erlang zlib driver* 

### Getting starting:

```erlang
StringBin = <<"this is a string compressed with zlib nif library">>,
{ok, DeflateRef} = ezlib:new(?Z_DEFLATE),
{ok, InflateRef} = ezlib:new(?Z_INFLATE),
CompressedBin = ezlib:process(DeflateRef, StringBin),
DecompressedBin = ezlib:process(InflateRef, CompressedBin),
DecompressedBin = StringBin
```

### Settings

`ezlib:new` accepts a second parameter where you can specify the following options:

- `use_iolist` : Returns the output as string (iolist) not as binary (default).
- `compression_level` : Compression level 0 - 9 default 6. 0 no compression, 9 max compression
- `window_bits` : The windowBits parameter is the base two logarithm of the window size (the size of the history buffer). It should be in the range 8..15 
- `memory_level`: Specifies how much memory should be allocated for the internal compression state. Values between 1 to 9 default is 8
- `compression_strategy`: The compression strategy that should be used. One of the following values (`Z_DEFAULT_STRATEGY` default one):

```erlang
-define(Z_FILTERED,         1).
-define(Z_HUFFMAN_ONLY,     2).
-define(Z_RLE,              3).
-define(Z_FIXED,            4).
-define(Z_DEFAULT_STRATEGY, 0).
```

Example:

```erlang
Options = [
    {compression_level, 6},
    {window_bits, 15},
    {memory_level, 8}
],
{ok, DeflateRef} = ezlib:new(?Z_DEFLATE, Options)
```

### Memory footprint

zlib memory footprint which can be calculated as:

- deflate memory usage in bytes = (1 << (`window_bits`+2)) + (1 << (`memory_level`+9)) 
- inflate memory usage in bytes = (1 << `window_bits`) + 1440*2*sizeof(int) 

The default values for `window_bits` and `memory_level` are 15 and 8 so for this values the default required memory for deflate is 256 KB and for inflate is 44 KB.

In addition to this memory ezlib allocates a 1 KB buffer used to avoid reallocation of memory all the time. This buffer is auto resizeable and cannot grow over 16 KB.

### Benchmarks (using the benchmark.erl from testing folder)

`benchmark:run(erlang, 1000000, 1, 8, 2).`

Time to complete: 2451.745 ms  407872.76001378614 per second

`benchmark:run(ezlib, 1000000, 1, 8, 2).`
 
Time to complete: 1537.777 ms  650289.3462446115 per second 

So ezlib it's with around 59 % faster than erlang implementation on the lowest compression method. Even if you change the compression methods
the results are almost the same because the zlib overhead is the same for both libs.