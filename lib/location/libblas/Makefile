
LIB=		libblas.a

MAN1=		
MAN3=		
MAN5=		

CLEAN= 		

cflags=		

include $(ANTELOPEMAKE)  	

SRCS=   caxpy.f \
	ccopy.f \
	cdcdot.f \
	cdotc.f \
	cdotu.f \
	crotg.f \
	cscal.f \
	cswap.f \
	dasum.f \
	daxpy.f \
	dcopy.f \
	ddot.f \
	dnrm2.f \
	drot.f \
	drotg.f \
	drotm.f \
	drotmg.f \
	dscal.f \
	dsdot.f \
	dswap.f \
	icamax.f \
	idamax.f \
	isamax.f \
	sasum.f \
	saxpy.f \
	scasum.f \
	scnrm2.f \
	scopy.f \
	sdot.f \
	sdsdot.f \
	snrm2.f \
	srot.f \
	srotg.f \
	srotm.f \
	srotmg.f \
	sscal.f \
	sswap.f

OBJS=	$(SRCS:.f=.o)
$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

# $Id$ 
