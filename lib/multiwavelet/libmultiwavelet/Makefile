LIB=libmultiwavelet.a		
DLIB=$(LIB:.a=$(DSUFFIX)) # uncomment when creating a dynamic library
DATADIR=schemas2 # directory under $(ANTELOPE)/data in which to put $(DATA) files
DATA=MWavelet1.4	# data files to be placed in data/$(DATADIR)
INCLUDE=multiwavelet.h 	# include files to be placed in include
PF=mwap.pf		# parameter files to be placed in data/pf

MAN3=mwtransform.3		

CLEAN= 		# Extra files which should be removed during a "make clean"

#cflags= -g -DSUNPERF6
cflags=-g
fflags= 	# 
ldflags= 	# flags to linker/loader
ldlibs= -lgenloc $(PERFLIBS) 


include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=			# Omit this line to run make in subdirectories, or
			# fill in to run make only in certain subdirectories


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
