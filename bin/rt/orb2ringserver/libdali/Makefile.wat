#
#
# Wmake File - For Watcom's wmake
# Use 'wmake -f Makefile.wat'

.BEFORE
	@set INCLUDE=.;$(%watcom)\H;$(%watcom)\H\NT
	@set LIB=.;$(%watcom)\LIB386

cc     = wcc386
cflags = -zq
lflags = OPT quiet OPT map
cvars  = $+$(cvars)$- -DWIN32

# To build a DLL uncomment the following two lines
#cflags = -zq -bd
#lflags = OPT quiet OPT map SYS nt_dll

LIB = libdali.lib
DLL = libdali.dll

INCS = -I.

OBJS=	timeutils.obj	&
	genutils.obj	&
	strutils.obj	&
	logging.obj	&
	network.obj	&
	statefile.obj	&
	config.obj	&
	portable.obj	&
	connection.obj     

all: lib

lib:	$(OBJS) .SYMBOLIC
	wlib -b -n -c -q $(LIB) +$(OBJS)

dll:	$(OBJS) .SYMBOLIC
	wlink $(lflags) name libdali file {$(OBJS)}

# Source dependencies:
timeutils.obj:	timeutils.c libdali.h
connection.obj:	connection.c libdali.h
strutils.obj:	strutils.c libdali.h
logging.obj:	logging.c libdali.h
network.obj:	network.c libdali.h
statefile.obj:	statefile.c libdali.h
config.obj:	config.c libdali.h
portable.obj:	portable.c portable.h libdali.h
main.obj:	main.c libdali.h

# How to compile sources:
.c.obj:
	$(cc) $(cflags) $(cvars) $(INCS) $[@ -fo=$@

# Clean-up directives:
clean:	.SYMBOLIC
	del *.obj *.map
	del $(LIB) $(DLL)
