BIN=orbgenloc
MAN1=orbgenloc.1
PF=orbgenloc.pf

cflags= #-xpg
ldlibs= -lgenloc -ltt -lblas -lcoords -lposix4 -lstock \
    $(DBLIBS) $(F77LIBS) -lpkt -lorb -ldl -lnsl -lsocket -lcoords  \
   -lpkt -lstock $(TRLIBS) $(SVR4LIBS)
#ldlibs=	-lgenloc -ltt -lblas -lcoords -lposix4 -lstock $(DBLIBS) $(F77LIBS) -lpkt -lorb -ldl -lnsl -lsocket -lcoords -lpkt -lstock 

include $(DSAPMAKE)
DIRS=

SRCS= orbgenloc.c maketmpdb.c  orb_arrivals_in.c 

OBJS=$(SRCS:.c=.o)
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)



lint: $(SRCS)
	lint $(SRCS) -I/opt/dsap/solaris/include -I/usr/openwin/include  
# $Id$ 
