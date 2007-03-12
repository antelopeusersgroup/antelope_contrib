

BIN 	= dbarrparams
MAN1=  dbarrparams.1

ldlibs= -lscv2 -ldbl2 $(TRLIBS) $(F77LIBS)

include	$(ANTELOPEMAKE)
DIRS=			

OBJS=	dbarrparams.o eigen.o polarization.o filter.o butwth.o
$(BIN) : $(OBJS)
	$(FC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


# $Id$ 
