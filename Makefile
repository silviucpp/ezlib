#baseline - original zlib
#cloudflare - cloudflare fork
#intel - intel fork
#zlibng - zlibng fork

ifndef ZLIB_FORK
    ZLIB_FORK=baseline
endif

compile:
	@./build_deps.sh $(ZLIB_FORK)
	@make V=0 -C c_src -j 8

clean:
	@make -C c_src clean
