
BIN=dbxcor
DATADIR=schemas/css3.0.ext
DATA=evlink sclink wfprocess xcorarrival xcorbeam

PF=dbxcor.pf
MAN1=dbxcor.1
DBG=-g
cxxflags=-g 

ldlibs= -L./SciPlot $(XMOTIFLIB) -lseisw -lsciplot -lXm -lXt \
  -lseispp -lperf -lgclgrid -ltks $(TRLIBS) $(X11LIBS) -lseispp 

CLEAN = cc_tks.h 

include $(ANTELOPEMAKE)  	
include $(ANTELOPEMAKE).local

CXXFLAGS += $(XMOTIFINCLUDE)

OBJS=	dbxcor.o \
  session_manager.o 
  
$(BIN) : $(OBJS) cc_tks.h
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

dbxcor.cc: cc_tks.h 

cc_tks.h: 
	@$(RM) $@
	@head -21 $(ANTELOPE)/include/tks.h > $@
	@echo \#ifdef  __cplusplus >> $@ 
	@echo extern \"C\" { >> $@ 
	@echo \#endif >> $@ 
	@grep extern $(ANTELOPE)/include/tks.h >> $@
	@echo \#ifdef  __cplusplus >> $@ 
	@echo } >> $@ 
	@echo \#endif >> $@ 
