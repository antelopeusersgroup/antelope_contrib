LIB=libmultiwavelet.a		
DLIB=$(LIB:.a=$(DSUFFIX)) 
DATADIR=schemas
DATA= MWavelet1.4
INCLUDE=multiwavelet.h 	
PF=mwap.pf		
MAN3=mwtransform.3		

CLEAN= 		
ldlibs= -lgenloc $(PERFLIBS) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	
DIRS=			


OBJS=	apsubs.o convolution.o dbsubs.o decimate.o lapack_routines.o \
   get_time_weight_function.o \
   matrix_subs.o mwavelet.o mwaveletsubs.o \
    print_band_info.o polarization.o stations.o statistics_subs.o \
   utilities.o trace_subs.o ttsubs.o MWsave_gather.o MWstack.o \
   MWcoherence.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
