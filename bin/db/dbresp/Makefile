

BIN 	= dbresp
ldlibs = -ldbl2 $(TRLIBS) -lgrx -lXext

include	$(ANTELOPEMAKE)
DIRS=

OBJS=	 dbresp.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
