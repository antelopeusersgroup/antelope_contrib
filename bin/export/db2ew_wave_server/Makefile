BIN=db2ew_wave_server

PF= $(BIN).pf

MAN1= $(BIN).1

CLEAN=

cflags= -mt
fflags=
ldflags=
ldlibs= -lthread $(TRLIBS) $(SVR4LIBS)

include $(ANTELOPEMAKE)

DIRS=

OBJS= $(BIN).o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
