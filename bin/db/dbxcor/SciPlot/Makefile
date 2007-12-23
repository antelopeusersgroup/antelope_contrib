LIB=libsciplot.a

cflags = $(XMOTIFINCLUDE)

include $(ANTELOPEMAKE)  	
include $(ANTELOPEMAKE).local

#cflags=-Dsun -Dsparc -DSVR4 -DSYSV
#cflags=-Xc -xF -xarch=v8

OBJS=SciPlot.o SciPlotUtil.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@
