
BIN 	= dbresp
ldlibs = -ldbl2 $(TRLIBS) -lgrx $(X11LIBS) -lXext 

include	$(ANTELOPEMAKE)
DIRS=

OBJS=	 dbresp.o
$(BIN) : $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
