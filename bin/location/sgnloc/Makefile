BIN=sgnloc
PF=sgnloc.pf

cflags = 
ldflags = 
ldlibs= -lgenloc -lblas -lcoords -lposix4 -lstock $(F77LIBS) -ltt -ldl
include $(DSAPMAKE)
DIRS=

OBJS= sgnloc.o
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
