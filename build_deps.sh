#!/usr/bin/env bash

DEPS_LOCATION=_build
DESTINATION=zlib2
ZLIB_FORK=$1

if [ -d "$DEPS_LOCATION/$DESTINATION" ]; then
    echo "Zlib fork already exist. delete $DEPS_LOCATION/$DESTINATION for a fresh checkout."
    exit 0
fi

BASELINE_REPO=https://github.com/madler/zlib.git
BASELINE_REV=50893291621658f355bc5b4d450a8d06a563053d

CLOUDFLARE_REPO=https://github.com/cloudflare/zlib.git
CLOUDFLARE_REV=a80420c63532c25220a54ea0980667c02303460a

INTEL_REPO=https://github.com/jtkukunas/zlib.git
INTEL_REV=4b9e3f0c56ce0a354bcb11f048f870f2d0fc544e

ZLIBNG_REPO=https://github.com/Dead2/zlib-ng.git
ZLIBNG_REV=515fcf5ff87957dc867c872d5af1b209384a4288

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
	git clone $REPO $DESTINATION
	pushd $DESTINATION
	git checkout $REV
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

	./configure --static
	make
	rm -f libz2.a
	cp libz.a libz2.a

	popd
	popd
}

DownloadZlib
BuildZlib
