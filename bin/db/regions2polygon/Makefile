BIN=regions2polygon

MAN1=regions2polygon.1

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
OBJS=regions2polygon.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

