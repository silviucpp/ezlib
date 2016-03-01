#!/usr/bin/env bash

ZLIB_FORK=$1

BASELINE_REPO=https://github.com/madler/zlib.git
BASELINE_REV=50893291621658f355bc5b4d450a8d06a563053d

CLOUDFLARE_REPO=https://github.com/cloudflare/zlib.git
CLOUDFLARE_REV=a80420c63532c25220a54ea0980667c02303460a

INTEL_REPO=https://github.com/jtkukunas/zlib.git
INTEL_REV=e176b3c23ace88d5ded5b8f8371bbab6d7b02ba8

ZLIBNG_REPO=https://github.com/Dead2/zlib-ng.git
ZLIBNG_REV=ba7f0eb6e294306ac6d771216ea4442f2ea2d830

DEPS_LOCATION=deps
DESTINATION=zlib

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
            CPP_FLAGS="-msse4.2 -mpclmul -O3"
            ;;
    esac

	echo "CFLAGS=$CPP_FLAGS"
    export CFLAGS="$CPP_FLAGS"

	./configure --static
	make
	
	popd	
	popd
}

DownloadZlib
BuildZlib
