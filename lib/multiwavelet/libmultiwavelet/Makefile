LIB=libmultiwavelet.a		
DLIB=$(LIB:.a=$(DSUFFIX)) 

DATADIR=schemas
DATA=MWavelet1.3	

INCLUDE=multiwavelet.h 	
PF=mwap.pf		

MAN3=mwtransform.3		


# cflags= -DSUNPERF6

include $(ANTELOPEMAKE)  	
DIRS=			
			

OBJS=	apsubs.o convolution.o dbsubs.o decimate.o lapack_routines.o \
   matrix_subs.o mwavelet.o \
   mwaveletsubs.o print_band_info.o polarization.o stations.o statistics_subs.o \
   utilities.o trace_subs.o ttsubs.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
