#
#
# Wmake File - For Watcom's wmake
# Use 'wmake -f Makefile.wat'

.BEFORE
	@set INCLUDE=.;$(%watcom)\H;$(%watcom)\H\NT
	@set LIB=.;$(%watcom)\LIB386

cc     = wcc386
cflags = -zq
lflags = OPT quiet OPT map LIBRARY ..\libdali.lib LIBRARY ws2_32.lib
cvars  = $+$(cvars)$- -DWIN32

BIN = daliclient.exe

INCS = -I..

OBJS=	daliclient.obj

all: $(BIN)

$(BIN):	$(OBJS)
	wlink $(lflags) name daliclient file {$(OBJS)}

# Source dependencies:
daliclient.obj:	daliclient.c

# How to compile sources:
.c.obj:
	$(cc) $(cflags) $(cvars) $(INCS) $[@ -fo=$@

# Clean-up directives:
clean:	.SYMBOLIC
	del *.obj *.map
	del $(BIN)
