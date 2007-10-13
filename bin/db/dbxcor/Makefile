
BIN=dbxcor
DATADIR=schemas/css3.0.ext
DATA=evlink sclink wfprocess xcorarrival xcorbeam

PF=dbxcor.pf
MAN1=dbxcor.1
DBG=-g
cxxflags=-g 
#
#  Set this macro to another location if the motif libraries are 
#  not in a standard place
# MacOS location when installed with Fink Commander
# XMOTIF=/sw
# MacOS location when compiling source code from open motif 
# Also seems to be standard for most linux distributions
XMOTIF=/usr/X11R6
# Solaris can use this -- redundant because normally in system
# include path
# XMOTIF=/usr/openwin

ldlibs= -L./SciPlot -L$(XMOTIF)/lib -lseisw -lsciplot -lXm -lXt \
  -lseispp -lperf -lgclgrid -ltks $(TRLIBS) $(X11LIBS) -lseispp 

CLEAN = cc_tks.h 

include $(ANTELOPEMAKE)  	

CXXFLAGS += -I$(XMOTIF)/include

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
