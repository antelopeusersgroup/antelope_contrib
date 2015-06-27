LIB	= libmw.a
DLIB=$(LIB:.a=$(DSUFFIX))

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
DIRS=			

CSOURCES = css2io.c \
           getfdcss.c \
           ioredercss.c \
           longiscss.c \
	    adaptspectra.c \
	    compute_fstat.c \
	    get_mwtaper.c \
	    tr2mw.c \
	    line_remove.c \
	    eigenspectra.c 

FSOURCES = adaptwt.f \
           adaptwt1.f \
           adaptwtl.f \
           adaptwtl1.f \
           arfilt.f \
           ceigenft.f \
           cfft.f \
           coher2.f \
           coher21.f \
           compos.f \
           compos1.f \
           cross.f \
           d1mach.f \
           dcabs1.f \
           dgqif.f \
           dot.f \
           dotp.f \
           drotg.f \
           dznrm2.f \
           efnnr.f \
           eigenft.f \
           fspec.f \
           ftest.f \
           fzero.f \
           linefind.f \
           mwsvd.f \
           mwsvd1.f \
           mwtr.f \
           mwtr1.f \
	   prob.f \
           pssev.f \
           pssevf.f \
           qr.f \
           r1mach.f \
           reshape.f \
           rotampspc.f \
           rtpss.f \
           rtpss1.f \
           sft.f \
           ssort.f \
           tdmev.f \
           tinvit.f \
           tridib.f \
           upefrr.f \
           ywar.f \
           zaxpy.f \
           zdotc.f \
           zdrot.f \
           zscal.f \
           zsvdc.f \
           zswap.f 

OBJS=	$(FSOURCES:.f=.o) $(CSOURCES:.c=.o)
$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(CONTRIBDLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)
        

# $Id$ 
