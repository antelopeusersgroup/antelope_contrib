LIB=libteles.a
MAN3=libteles.3
DATADIR=tables/teles
DATA=iaspei.time2 elpcor


fflags=-g

SUBDIR=/contrib
include $(ANTELOPEMAKE)
OBJS=getiasp.o bjdaz2.o elpcr.o redtab.o find.o glat.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@
