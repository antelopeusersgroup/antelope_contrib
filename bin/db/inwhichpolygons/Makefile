BIN=inwhichpolygons

MAN1=inwhichpolygons.1

CLEAN=

cflags=-g -I$(ANTELOPE)/local/include
fflags= 	# 
include ldflags.$(OSTYPE)
#ifeq ($(OSTYPE),linux)	
#ldflags= -Wl,-rpath,$(ANTELOPE)/local/lib -L$(ANTELOPE)/local/lib	# flags to linker/loader
#else
#ldflags= -L$(ANTELOPE)/local/lib -R$(ANTELOPE)/local/lib	# flags to linker/loader
#endif
ldlibs=	$(DBLIBS)	-lpolygon # libraries to link in.

include $(ANTELOPEMAKE)
DIRS=

#SUBDIR=/local
OBJS=inwhichpolygons.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

