# ezlib

[![Build Status](https://travis-ci.com/silviucpp/ezlib.svg?branch=master)](https://travis-ci.com/github/silviucpp/ezlib)
![GitHub](https://img.shields.io/github/license/silviucpp/ezlib)

*An zlib NIF library for Erlang optimised for streaming* 

### Intro

`ezlib` can be used with different zlib forks like:

- [baseline][1] - will statically link with the original zlib
- [cloudflare][2] - will statically link with the cloudflare zlib fork
- [intel][3] - will statically link with the intel zlib fork
- [zlibng][4] - will statically link with zlibng fork

By default the baseline is used. In order to change the zlib fork you can use the `ZLIB_FORK` env variable. For example
to use `zlibng` for you can use: 

```erlang
ZLIB_FORK=zlibng rebar compile
``` 

Based on [zlib FAQ][7] thread safety of the library can be achieved by meeting several conditions. The most important being
the fact that you should only operate on any given zlib stream from a single thread at a time. In erlang this means that
you need to operate on a ezlib session from the same process that created it. In case you access the same stream from multiple
processes you will get an error when calling `process/2`.

You can access the changelog from [here][8]

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
- inflate memory usage in bytes = (1 << `window_bits`) + 1440\*2\*sizeof(int) 

The default values for `window_bits` and `memory_level` are 15 and 8 so for this values the default required memory for deflate is 256 KB and for inflate is 44 KB.

In addition to this memory ezlib allocates a 1 KB buffer used to avoid reallocation of memory all the time. This buffer is auto resizeable and cannot grow over 8KB.

### Metrics

In order to see the statistics regarding compression ratio you can use `ezlib:metrics/1`. Output looks like:

```erlang
 {ok,[{bytes_in,42227900},
     {bytes_out,29096830},
     {compression_ratio,31.09572107540276}]}
```     

### Benchmarks (using the benchmark.erl from test folder)

You can specify a file path, concurrency level, how many times to deflate it line by line and compression level, window size and memory level.

`benchmark:run(ezlib,"file path here", 1, 1, 6, 10, 1).` or `benchmark:run(erlang,"file path here", 1, 1, 6, 10, 1).`

Benchmark results compressing a text file line by line 20 times on a MacBook Pro 

```erlang
benchmark:run(ezlib, FilePath, ConcurencyLevel, 20, 6, 10, 1).
```

Zlib settings:

- `Compression level`: 6
- `Window Size`: 10
- `Memory Level` : 1

Results: (for different concurrency levels)

| zlib library           | C1 (MB/s) | C5 (MB/s) | C10 (MB/s) | Compression Ratio (%) |
|:----------------------:|:---------:|:---------:|:----------:|:---------------------:|
| erlang 19.2 zlib       | 22.22     | 87.90     | 91.46      | N/A                   |
| ezlib baseline         | 25.33     | 105.37    | 113.68     | 73.10                 |
| ezlib cloudflare       | 28.98     | 122.36    | 133.12	  | 72.61                 |
| ezlib intel            | 29.43	 | 120.98	 | 131.13  	  | 75.89                 |
| ezlib zlibng           | 31.24     | 129.36    | 139.50     | 75.86                 |

Other useful resources regarding the optimisations did by Intel, Cloudflare and ZlibNg into their forks and other benchmarks as well can be found [here][5] and [here][6].

[1]:https://github.com/madler/zlib.git
[2]:https://github.com/cloudflare/zlib.git
[3]:https://github.com/jtkukunas/zlib.git
[4]:https://github.com/Dead2/zlib-ng.git
[5]:https://www.snellman.net/blog/archive/2014-08-04-comparison-of-intel-and-cloudflare-zlib-patches.html
[6]:http://www.snellman.net/blog/archive/2015-06-05-updated-zlib-benchmarks/
[7]:http://www.zlib.net/zlib_faq.html#faq21
[8]:https://github.com/silviucpp/ezlib/blob/master/CHANGELOG.MD
