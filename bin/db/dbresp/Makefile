

BIN 	= dbresp
ldlibs=         -ldbl2 -lgrx $(X11LIBS) $(TRLIBS) $(F77LIBS)

include	$(ANTELOPEMAKE)
DIRS=


OBJS=	 dbresp.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


# $Id$ 
