#!/usr/bin/env bash

DEPS_LOCATION=_build
DESTINATION=zlib2
ZLIB_FORK=$1

if [ -f "$DEPS_LOCATION/$DESTINATION/libz2.a" ]; then
    echo "Zlib fork already exist. delete $DEPS_LOCATION/$DESTINATION for a fresh checkout."
    exit 0
fi

BASELINE_REPO=https://github.com/madler/zlib.git
BASELINE_REV=4a090adef8c773087ec8916ad3c2236ef560df27

CLOUDFLARE_REPO=https://github.com/cloudflare/zlib.git
CLOUDFLARE_REV=a80420c63532c25220a54ea0980667c02303460a

INTEL_REPO=https://github.com/jtkukunas/zlib.git
INTEL_REV=4b9e3f0c56ce0a354bcb11f048f870f2d0fc544e

ZLIBNG_REPO=https://github.com/Dead2/zlib-ng.git
ZLIBNG_REV=343c4c549107d31f6eeabfb4b31bec4502a2ea0e

function fail_check
{
    "$@"
    local status=$?
    if [ $status -ne 0 ]; then
        echo "error with $1" >&2
        exit 1
    fi
}

function DownloadZlib()
{
	case $ZLIB_FORK in
		baseline)
			REPO=$BASELINE_REPO
			REV=$BASELINE_REV
			;;

		cloudflare)
			REPO=$CLOUDFLARE_REPO
			REV=$CLOUDFLARE_REV
			;;

		intel)
			REPO=$INTEL_REPO
			REV=$INTEL_REV
			;;

		zlibng)
			REPO=$ZLIBNG_REPO
			REV=$ZLIBNG_REV
			;;

		*)
		 	echo "Your fork is not supported: $ZLIB_FORK"
		 	exit 1	
		 	;;
	esac

	echo "repo=$REPO rev=$REV"


	mkdir -p $DEPS_LOCATION
	pushd $DEPS_LOCATION

	if [ ! -d "$DESTINATION" ]; then
	    fail_check git clone $REPO $DESTINATION
	fi

	pushd $DESTINATION
	fail_check git checkout $REV
	popd
	popd
}

function BuildZlib()
{
	OS=$(uname -s)
	KERNEL=$(echo $(lsb_release -ds 2>/dev/null || cat /etc/*release 2>/dev/null | head -n1 | awk '{print $1;}') | awk '{print $1;}')

	pushd $DEPS_LOCATION
	pushd $DESTINATION

    case $OS in
        Darwin)
            CPP_FLAGS="-msse4.2 -O3"
            ;;
        *)
            CPP_FLAGS="-fPIC -msse4.2 -mpclmul -O3"
            ;;
    esac

	echo "CFLAGS=$CPP_FLAGS"
    export CFLAGS="$CPP_FLAGS"

	fail_check ./configure --static
	fail_check make
	rm -f libz2.a
	fail_check cp libz.a libz2.a

	popd
	popd
}

DownloadZlib
BuildZlib
