PF=dblocgrad.pf
BIN=dblocgrad		
DATADIR=schemas2
DATA=css_vel


cflags=		
fflags= 	-e  -C
ldflags= 	
ldlibs=$(DBLIBS)

include $(DSAPMAKE)  	
DIRS=			

OBJS=dblocgrad.o locsubs_flv.o matsubs.o tracesubs_flv.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)



# $Id$ 
