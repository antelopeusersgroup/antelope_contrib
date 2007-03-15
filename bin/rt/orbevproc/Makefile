# Copyright (c) 2007 Boulder Real Time Technologies, Inc.
# All rights reserved

# This software module is wholly owned by Boulder Real Time 
# Technologies, Inc. This software makefile may be copied
# and used in any way.

BIN =  orbevproc dbevproc

MAN1 =

PF = orbevproc.pf dbevproc.pf

ldlibs= -lperlembed `perl -MExtUtils::Embed -e ldopts` -lassoc -lbrttpkt -lbrttfilter -lbrttutil $(ORBLIBS) -lpthread

include $(ANTELOPEMAKE)
DIRS=

OBJS= orbevproc.o

orbevproc : orbevproc_version.h $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

dbevproc : orbevproc
	$(RM) $@ ; cp orbevproc dbevproc

.time :
	-$(RM) $@
	-touch $@

orbevproc_version.h : .time orbevproc_version.h.id force
	-$(RM) $@
	produce $@
	touch -r .time orbevproc_version.h

force:
