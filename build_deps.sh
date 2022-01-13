#!/usr/bin/env bash

DEPS_LOCATION=_build
DESTINATION=zlib2
ZLIB_FORK=$1

if [ -f "$DEPS_LOCATION/$DESTINATION/libz2.a" ]; then
    echo "Zlib fork already exist. delete $DEPS_LOCATION/$DESTINATION for a fresh checkout."
    exit 0
fi

BASELINE_REPO=https://github.com/madler/zlib.git
BASELINE_REV=cacf7f1d4e3d44d871b605da3b647f07d718623f

CLOUDFLARE_REPO=https://github.com/cloudflare/zlib.git
CLOUDFLARE_REV=959b4ea305821e753385e873ec4edfaa9a5d49b7

INTEL_REPO=https://github.com/jtkukunas/zlib.git
INTEL_REV=bf29715726fe22390b59b2cbf9c935179ef49aed

ZLIBNG_REPO=https://github.com/Dead2/zlib-ng.git
ZLIBNG_REV=62b269b4a5f4502d6dc03d73bb5b552e80066885

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

	case $ZLIB_FORK in
        zlibng)
            fail_check cp libz-ng.a libz2.a
            ;;

        *)
            fail_check cp libz.a libz2.a
            ;;
    esac

	popd
	popd
}

DownloadZlib
BuildZlib
