### Change log:

- v2.7

    - Improved internal buffer
    - use `ZLIB_FORK` env variable to change the default zlib fork

- v2.6 

    - Use baseline zlib fork as default

- v2.5 

    - Update zlib forks versions (baseline = v1.2.11)
    - Improved the benchmark script
    - Fix the script for getting the zlib dependencies.

- v2.4

    - Remove the support for `use_iolist`. All output is returned as `binary()` now.
    - Implemented processing in chunks to make sure that the library is not blocking the VM schedulers. Also gives the runtime system a hint about how much CPU time the current NIF call has consumed using [enif_consume_timeslice][5]

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

    - Integrated support for external zlib forks:
    - [baseline][1] - will statically link with the original zlib
    - [cloudflare][2] - will statically link with the cloudflare zlib fork
    - [intel][3] - will statically link with the intel zlib fork
    - [zlibng][4] - will statically link with zlibng fork

- v1.0

    - Initial implementation. 
    
[1]:https://github.com/madler/zlib.git
[2]:https://github.com/cloudflare/zlib.git
[3]:https://github.com/jtkukunas/zlib.git
[4]:https://github.com/Dead2/zlib-ng.git    
[5]:http://erlang.org/doc/man/erl_nif.html#enif_consume_timeslice