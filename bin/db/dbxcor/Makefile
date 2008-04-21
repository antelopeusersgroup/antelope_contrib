
BIN=dbxcor
DATADIR=schemas/css3.0.ext
DATA=evlink sclink wfprocess xcorarrival xcorbeam

PF=dbxcor.pf
MAN1=dbxcor.1
DBG=-g
cxxflags=-g 

ldlibs= -L./SciPlot $(XMOTIFLIB) -lseisw -lsciplot -lXm -lXt \
  -lseispp -lperf -lgclgrid -ltks $(TRLIBS) $(X11LIBS) -lseispp 

CLEAN = 

include $(ANTELOPEMAKE)  	
include $(ANTELOPEMAKE).local

CXXFLAGS += $(XMOTIFINCLUDE)

OBJS=	dbxcor.o \
  session_manager.o 
  
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
