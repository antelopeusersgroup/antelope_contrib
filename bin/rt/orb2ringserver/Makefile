BIN=	orb2ringserver

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags= -Ilibmseed -Ilibdali
ldflags= -Llibmseed -Llibdali
ldlibs= $(ORBLIBS) -lmseed -ldali

include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=	libmseed libdali

OBJS=	orb2ringserver.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
