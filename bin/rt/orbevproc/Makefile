# Copyright (c) 2007 Boulder Real Time Technologies, Inc.
# All rights reserved

# This software module is wholly owned by Boulder Real Time 
# Technologies, Inc. This software makefile may be copied
# and used in any way.

BIN =  orbevproc

MAN1 =

PF = orbevproc.pf

ldlibs= -lperlembed `perl -MExtUtils::Embed -e ldopts` -lbrttpkt -lbrttfilter -lbrttutil $(ORBLIBS) -lpthread

include $(ANTELOPEMAKE)
DIRS=

OBJS= orbevproc.o

$(BIN) : orbevproc_version.h $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

.time :
	-$(RM) $@
	-touch $@

orbevproc_version.h : .time orbevproc_version.h.id force
	-$(RM) $@
	produce $@
	touch -r .time orbevproc_version.h

force:
