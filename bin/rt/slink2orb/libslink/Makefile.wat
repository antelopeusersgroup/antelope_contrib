#
#
# Wmake File For libslink - For Watcom's wmake
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

LIB = libslink.lib
DLL = libslink.dll

INCS = -I.

OBJS=	tswap.obj	&
	unpack.obj	&
	msrecord.obj	&
	slutils.obj	&
	strutils.obj	&
	logging.obj	&
	network.obj	&
	statefile.obj	&
	config.obj	&
	slplatform.obj	&
	main.obj

all: lib

lib:	$(OBJS) .SYMBOLIC
	wlib -b -n -c -q $(LIB) +$(OBJS)

dll:	$(OBJS) .SYMBOLIC
	wlink $(lflags) name libslink file {$(OBJS)}

# Source dependencies:
tswap.obj:	tswap.c tswap.h slplatform.h
unpack.obj:	unpack.c unpack.h tswap.h 
msrecord.obj:	msrecord.c libslink.h
slutils.obj:	slutils.c libslink.h
strutils.obj:	strutils.c libslink.h
logging.obj:	logging.c libslink.h
network.obj:	network.c libslink.h
statefile.obj:	statefile.c libslink.h
config.obj:	config.c libslink.h
slplatform.obj:	slplatform.c slplatform.h libslink.h
main.obj:	main.c libslink.h

# How to compile sources:
.c.obj:
	$(cc) $(cflags) $(cvars) $(INCS) $[@ -fo=$@

# Clean-up directives:
clean:	.SYMBOLIC
	del *.obj *.map
	del $(LIB) $(DLL)
