GHC ?= ghc

GHCFLAGS += \
	--make \
	-O3

sources := $(wildcard *.hs)
objs    := $(sources:hs=o)
ifs     := $(sources:hs=hi)
targets := $(sources:.hs=)

.PHONY: all clean

%: %.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

all: $(targets)

clean:
	rm -f $(targets) $(objs) $(ifs)
