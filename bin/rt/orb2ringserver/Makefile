BIN     = orb2ringserver
MAN1    = $(BIN).1

cflags  = -Ilibmseed -Ilibdali
ldflags = -Llibmseed -Llibdali
ldlibs  = $(ORBLIBS) -lmseed -ldali

SUBDIR = /contrib
include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..

DIRS   = libmseed libdali

OBJS   = orb2ringserver.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
