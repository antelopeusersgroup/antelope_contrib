BIN=orbgenloc
MAN1=orbgenloc.1

cflags= #-xpg
ldlibs= -lgenloc -ltt -lblas -lcoords -lposix4 -lstock \
    $(DBLIBS) $(F77LIBS) -lpkt -lorb -ldl -lnsl -lsocket -lcoords  \
   -lpkt -lstock $(TRLIBS) $(SVR4LIBS)
#ldlibs=	-lgenloc -ltt -lblas -lcoords -lposix4 -lstock $(DBLIBS) $(F77LIBS) -lpkt -lorb -ldl -lnsl -lsocket -lcoords -lpkt -lstock 

include $(DSAPMAKE)
DIRS=

SRCS= orbgenloc.c

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)



# $Id$ 
