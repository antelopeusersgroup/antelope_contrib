
BIN=dbxcor
DATADIR=schemas
DATA=xcor1.0

PF=dbxcor.pf
#MAN1=dbxcor.1

ldlibs= -L./SciPlot -lseisw -lsciplot -lXm -lXt \
  -lseispp -lperf -lgclgrid $(TRLIBS) $(X11LIBS) -lseispp

include $(ANTELOPEMAKE)  	

OBJS=	dbxcor.o \
  session_manager.o \
  AnalysisSetting.o \
  XcorProcessingEngine.o 
  
$(BIN) : $(OBJS)
	$(RM) $@
	$(CXX) $(CCFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

