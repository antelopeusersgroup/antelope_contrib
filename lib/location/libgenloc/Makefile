LIB=		libgenloc.a
INCLUDE = location.h
DLIB=$(LIB:.a=$(DSUFFIX))
MAN3 = genloc.3 \
       genloc_intro.3 \
       ggnloc.3 \
       eqerror.3 \
	genloc_ttinterface.3

CLEAN= 		

#
#  Comment out the following line if you are using Sun's Performance
#  library.  Most universities receive this bundled with compilers.
#
cflags=	-DSUNPERF
ldflags= 	
ldlibs=$DBLIBS -ltt -lm

include $(ANTELOPEMAKE)  	
		
DIRS=	
OBJS=	  bisquare.o \
    calc_statistics.o \
    dbinput_routines.o \
    distance_weight.o \
    erfinv.o \
    eqerror.o \
    generic_tu_calculate.o \
    huber.o \
    initial_locate.o \
    input_routines.o \
    locate.o \
    matrix.o \
    save_predarr.o \
    svdcmp.o \
    table_interpolate_functions.o \
    thomson.o \
    ttcalc_interface.o\
    ttlvz.o \
    ttlvz_interface.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -L$(ANTELOPE)/lib $(DBLIBS) -ldl -lcoords -lstock -lposix4 -lm -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

# $Id$ 
