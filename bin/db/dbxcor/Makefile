
BIN=dbxcor
DATADIR=schemas
DATA=xcor1.2
INCLUDE=AnalysisSetting.h XcorProcessingEngine.h

PF=dbxcor.pf
MAN1=dbxcor.1
DBG=-g
cxxflags=-g

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

