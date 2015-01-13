# Make RefTek libraries

all: reftek rtp util

reftek: FORCE
	cd $@; $(MAKE)

rtp: FORCE
	cd $@; $(MAKE)

util: FORCE
	cd $@; $(MAKE)

clean: FORCE
	cd reftek;  $(MAKE) $@
	cd rtp;     $(MAKE) $@
	cd util;    $(MAKE) $@

remove: FORCE
	cd reftek;  $(MAKE) $@
	cd rtp;     $(MAKE) $@
	cd util;    $(MAKE) $@

install: all

FORCE:
