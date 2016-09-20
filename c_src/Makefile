# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT)_nif.so

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)]).")

# System type and C compiler/flags.

UNAME_SYS := $(shell uname -s)

ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -Wall -Wmissing-prototypes
	CXXFLAGS ?= -g -O3 -arch x86_64 -Wall -Wextra -Werror -Wno-missing-field-initializers -fno-exceptions -fno-rtti -DUSE_CUSTOM_ALLOCATOR -DCHECK_CALLER_PROCESS -isystem $(BASEDIR)/_build
	LDFLAGS ?=  -Wl,-U,_enif_alloc \
				-Wl,-U,_enif_alloc_resource \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_get_int \
				-Wl,-U,_enif_get_list_cell \
				-Wl,-U,_enif_get_resource \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_inspect_binary \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_list \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_badarg \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_double \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_resource \
				-Wl,-U,_enif_make_string_len \
				-Wl,-U,_enif_make_tuple \
				-Wl,-U,_enif_make_ulong \
				-Wl,-U,_enif_open_resource_type \
				-Wl,-U,_enif_release_resource \
				-Wl,-U,_enif_priv_data \
				-Wl,-U,_enif_self \
				-arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -g -O3 -Wall -Wextra -Werror -Wno-missing-field-initializers -fno-exceptions -fno-rtti -D_USE_PRAGMA_WEAK -DUSE_CUSTOM_ALLOCATOR -DCHECK_CALLER_PROCESS -isystem $(BASEDIR)/_build
	LDFLAGS ?= -Wl,--exclude-libs=ALL
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR)
LDFLAGS += -L $(ERL_INTERFACE_LIB_DIR) \
		   -L$(BASEDIR)/_build/zlib2 \
		   -shared \
		   -lerl_interface \
		   -lei \
		   -lz2 \
		   -lstdc++

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
