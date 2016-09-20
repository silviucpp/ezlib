# ezlib

*An zlib NIF library for Erlang optimised for streaming. Much faster than the erlang zlib driver (with over 60%)* 

### Change log:

- v2.2
    - Add `rebar3` support

- v2.1
    - Add support for checking if the process that plays with the ezlib session is the same that created it. See Notes for more info
    - Updated the forks versions and changed to use baseline as default fork

- v2.0
    - Integrated support for external zlib forks (around 69 % faster than erlang driver):
    - [baseline][1] - will statically link with the original zlib
    - [cloudflare][2] - will statically link with the cloudflare zlib fork
    - [intel][3] - will statically link with the intel zlib fork
    - [zlibng][4] - will statically link with zlibng fork
- v1.0
    - Initial implementation. From load testing there is around 30 % improvements that `zlib erlang driver`.

### Getting starting:

```erlang
StringBin = <<"this is a string compressed with zlib nif library">>,
{ok, DeflateRef} = ezlib:new(?Z_DEFLATE),
{ok, InflateRef} = ezlib:new(?Z_INFLATE),
CompressedBin = ezlib:process(DeflateRef, StringBin),
DecompressedBin = ezlib:process(InflateRef, CompressedBin),
DecompressedBin = StringBin
```

### Notes

Based on [zlib FAQ][7] thread safety of the library can be achieved by meeting several conditions. The most important being
the fact that you should only operate on any given zlib stream from a single thread at a time. In erlang this means that
you need to operate on a ezlib session from the same process that created it. The build `2.1` returns an error in case this is not
achieved. You can disable this by removing from build options in `c_src/Makefile` the `-DCHECK_CALLER_PROCESS` directive.

### Settings

`ezlib:new` accepts a second parameter where you can specify the following options:

- `use_iolist` : Returns the output as string not as binary (default).
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

In addition to this memory ezlib allocates a 1 KB buffer used to avoid reallocation of memory all the time. This buffer is auto resizeable and cannot grow over 8KB.

### Metrics

In order to see the statistics regarding compression ratio you can use `ezlib:metrics/1`. Output looks like:

```erlang
{ok,{{total_in,192},
     {total_out,158},
     {ratio,17.708333333333336}}}
```     

### Benchmarks (using the benchmark.erl from testing folder)

You can specify a file path, how many times to deflate it line by line and compression level, window size and memory level.

`benchmark:run(ezlib,"file path here", 10000, 1, 8, 2).` or `benchmark:run(erlang,"file path here", 10000, 1, 8, 2).`

Benchmark results compressing a text file line by line 200 times on a Ubuntu 14.04 LTS ( `benchmark:run(ezlib,"file path here", 200, 6, 10, 1).` ):

Zlib settings:

- `Compression level`: 6
- `Window Size`: 10
- `Memory Level` : 1

Results:

| zlib library       | Run1 (ms) | Run2 (ms) | Run3 (ms) | Compression Ratio (%) |
|:------------------:|:---------:|:---------:|:---------:|:---------------------:|
| erlang zlib driver | 20128.801 | 20038.368 | 20000.725 | N/A                   |
| ezlib baseline     | 17588.222 | 17569.594 | 17610.2   | 67.92                 |
| ezlib cloudflare   | 15077.336 | 15074.345 | 15078.263 | 67.30                 |
| ezlib intel        | 14392.551 | 14339.686 | 14364.061 | 71.15                 |
| ezlib zlibng       | 13868.952 | 13855.77  | 13847.749 | 71.09                 |

Other usefull resources regarding the optimisations did by Intel, Cloudflare and ZlibNg into their forks and other benchmakrs as well can be found [here][5] and [here][6].

[1]:https://github.com/madler/zlib.git
[2]:https://github.com/cloudflare/zlib.git
[3]:https://github.com/jtkukunas/zlib.git
[4]:https://github.com/Dead2/zlib-ng.git
[5]:https://www.snellman.net/blog/archive/2014-08-04-comparison-of-intel-and-cloudflare-zlib-patches.html
[6]:http://www.snellman.net/blog/archive/2015-06-05-updated-zlib-benchmarks/
[7]:http://www.zlib.net/zlib_faq.html#faq21