BIN=	orbptrigger
PF=		# parameter files to be placed in data/pf
MAN1=	orbptrigger.1	# man pages for commands

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags=
ldlibs= $(ORBLIBS)

include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=

OBJS=	strutils.o orbptrigger.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
