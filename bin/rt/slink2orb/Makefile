# Use this Makefile with Sun's compiler

BIN=	slink2orb
PF=	slink2orb.pf	# parameter files to be placed in data/pf
MAN1=	slink2orb.1	# man pages for commands

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags= -Ilibslink -Llibslink
ldlibs= $(ORBLIBS) -lslink

include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=   libslink

OBJS=	mseed2orbpkt.o slink2orb.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

