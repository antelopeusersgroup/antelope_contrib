
BIN = dbampmag

MAN1 = dbampmag.1

PF = dbampmag.pf

cflags =  -g
ldflags = 
ldlibs = -lbrttfilter -lbrttutil $(TRLIBS) $(SVR4LIBS) 

SUBDIR=/contrib
include $(ANTELOPEMAKE)
#SUBDIR=/local
DIRS=

OBJS= \
	dbampmag.o dsseis.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
