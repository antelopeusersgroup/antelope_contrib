
BIN = orbampmag

MAN1 = orbampmag.1
CLEAN= core .make.state

PF = orbampmag.pf mb.pf ml.pf ms.pf

ldflags = 
ldlibs = -lbrttpkt -lwffil -lbrttutil $(ORBLIBS) $(THREAD)

include $(ANTELOPEMAKE)
DIRS=
#SUBDIR=/local

OBJS= \
	orbampmag.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
