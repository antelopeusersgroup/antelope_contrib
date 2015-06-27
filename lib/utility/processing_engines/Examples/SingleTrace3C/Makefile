BIN=SingleTrace3C
DBG=-g
MATLAB=/opt/matlab6p1/extern
RPATH="-Wl,--rpath-link,$TMW_ROOT/extern/lib/$Arch,--rpath-link,$TMW_ROOT/bin/$Arch"

cflags=-I$(MATLAB)/include
ldlibs= -L$(MATLAB)/lib/sol2 -R$(MATLAB)/lib/sol2 -lmatlabprocessor -lgclgrid -lseispp -leng -lmx -lut -lmat \
	$(TRLIBS) -lperf

SUBDIR=/contrib
include $(ANTELOPEMAKE)
OBJS=	SingleTrace3C.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
