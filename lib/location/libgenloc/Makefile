LIB=		libgenloc.a
INCLUDE = location.h

CLEAN= 		

cflags=	
ldflags= 	
ldlibs=	

include $(DSAPMAKE)  	
		
DIRS=	

OBJS=	  bisquare.o \
    calc_statistics.o \
    dbinput_routines.o \
    distance_weight.o \
    erfinv.o \
    generic_tu_calculate.o \
    huber.o \
    initial_locate.o \
    input_routines.o \
    locate.o \
    matrix.o \
    svdcmp.o \
    table_interpolate_functions.o \
    thomson.o \
    ttcalc_interface.o\
    ttlvz.o \
    ttlvz_interface.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@



# $Id$ 
