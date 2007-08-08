LIB=libsciplot.a

cflags = -I/sw/include 

include $(ANTELOPEMAKE)  	
#cflags=-Dsun -Dsparc -DSVR4 -DSYSV
#cflags=-Xc -xF -xarch=v8

OBJS=SciPlot.o SciPlotUtil.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@
