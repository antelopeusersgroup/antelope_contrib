LIB=libmultiwavelet.a		
DLIB=$(LIB:.a=$(DSUFFIX)) # uncomment when creating a dynamic library
DATADIR=	# directory under $(ANTELOPE)/data in which to put $(DATA) files
DATA=		# data files to be placed in data/$(DATADIR)
INCLUDE=multiwavelet.h 	# include files to be placed in include
PF=		# parameter files to be placed in data/pf

MAN3=mwtransform.3		

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags=		# -g or -O is the most common option here and in fflags
fflags= 	# 
ldflags= 	# flags to linker/loader
ldlibs=		# libraries to link in.

include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=			# Omit this line to run make in subdirectories, or
			# fill in to run make only in certain subdirectories


OBJS=	convolution.o dbsubs.o decimate.o lapack_routines.o mwavelet.o \
   mwaveletsubs.o polarization.o stations.o statistics_subs.o utilities.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
