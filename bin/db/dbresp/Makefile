

BIN 	= dbresp
ldlibs=         -ldbl2 -lgrx $(X11LIBS) $(DBLIBS) $(F77LIBS)

include	$(DSAPMAKE)
DIRS=


OBJS=	 dbresp.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)


# $Id$ 
