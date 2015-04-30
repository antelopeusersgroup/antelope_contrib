LIB=		libglp.a
LICENSES= license_libglp.txt

CLEAN= 		

ldflags=
ldlibs=

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
DIRS=

OBJS= acal.o afree.o calnder.o clock.o comppro.o cputlt.o dttime.o firstn.o \
ftest.o gclc.o gisol.o iotrap.o iquell.o istaptr.o izptr.o layer.o \
lenstr.o linear.o linsrc.o makeggt.o makertr.o mfnwa.o mkcov.o mkres.o \
mtrans.o mtrx0.o mtrxadd.o mtrxib.o mtrxib2.o mtrxie.o mtrxmul.o mtrxoe.o \
mtrxpr.o nalpha.o nextw.o numbig.o opnerr.o opnhis.o parsefd.o project.o \
quad.o rmeq.o scale10.o scalpdc.o seqht.o shape.o slant.o smtmax.o split.o \
sqrf.o strcat.o strclr.o strcmp.o sum.o sumsq.o sxpy.o symx.o test0.o \
ttlvz.o txhalf.o txint.o txref.o utmul.o vmodin.o vscal.o weight.o \
wtlqk.o wtshot.o xnorm.o xnorm_sun.o zerolt.o 

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@


$(LICENSES) : LICENSE
	cp $< $@

# $Id$ 
