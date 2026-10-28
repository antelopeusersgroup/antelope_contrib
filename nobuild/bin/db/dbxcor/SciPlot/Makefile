LIB=libsciplot.a

cflags = -I$(XMOTIFINCLUDE)

ANTELOPEMAKELOCAL = $(ANTELOPE)/contrib/include/antelopemake.local

SUBDIR=/contrib
include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

#cflags=-Dsun -Dsparc -DSVR4 -DSYSV
#cflags=-Xc -xF -xarch=v8

OBJS=SciPlot.o SciPlotUtil.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@
