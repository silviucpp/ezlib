# ezlib

*An zlib NIF library for Erlang optimised for streaming. Much faster than the erlang zlib driver (with over 60%)* 

### Change log:

- v2.6 

    - Use baseline zlib fork as default

- v2.5 

    - Update zlib forks versions (baseline = v1.2.11)
    - Improved the benchmark script
    - Fix the script for getting the zlib dependencies.

- v2.4

    - Remove the support for `use_iolist`. All output is returned as `binary()` now.
    - Implemented processing in chunks to make sure that the library is not blocking the VM schedulers. Also gives the runtime system a hint about how much CPU time the current NIF call has consumed using [enif_consume_timeslice][8]

- v2.3
    - Update zlib forks versions (baseline = v1.2.10, zlibng = last master)
    - Small code cleanup
    - Add proper validations for compression options
    - Do not throw exceptions in case of bad arguments, return `{error, Reason}` instead
    - Change the return value of `ezlib:metrics/1` to return a `proplist`
    - Add unit testing
    - Make `zlibng` default flavour

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

Benchmark results compressing a text file line by line 20 times on a Ubuntu 14.04 LTS ( `benchmark:run(ezlib,"file path here", 1, 20, 6, 10, 1).` ):

Zlib settings:

- `Compression level`: 6
- `Window Size`: 10
- `Memory Level` : 1

Results: (concurrency level 1)

| zlib library           | Run1 (ms) | Run2 (ms) | Run3 (ms) | Compression Ratio (%) |
|:----------------------:|:---------:|:---------:|:---------:|:---------------------:|
| erlang 19.2 zlib       | 5263.057  | 5310.38   | 5364.473  | N/A                   |
| ezlib baseline v1.2.10 | 4262.78   | 4272.341  | 4274.306  | 73.10                 |
| ezlib cloudflare       | 3679.534  | 3691.137  | 3695.24	 | 72.61                 |
| ezlib intel            | 3541.116	 | 3546.805	 | 3578.607	 | 75.89                 |
| ezlib zlibng           | 3466.844  | 3503.928  | 3478.131  | 75.86                 |

Other usefull resources regarding the optimisations did by Intel, Cloudflare and ZlibNg into their forks and other benchmakrs as well can be found [here][5] and [here][6].

[1]:https://github.com/madler/zlib.git
[2]:https://github.com/cloudflare/zlib.git
[3]:https://github.com/jtkukunas/zlib.git
[4]:https://github.com/Dead2/zlib-ng.git
[5]:https://www.snellman.net/blog/archive/2014-08-04-comparison-of-intel-and-cloudflare-zlib-patches.html
[6]:http://www.snellman.net/blog/archive/2015-06-05-updated-zlib-benchmarks/
[7]:http://www.zlib.net/zlib_faq.html#faq21
[8]:http://erlang.org/doc/man/erl_nif.html#enif_consume_timeslice