BIN=	mseed2seedpkt
MAN1=   mseed2seedpkt.1

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags=
ldlibs= $(ORBLIBS)

include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=

OBJS=	seedutil.o mseed2orbpkt.o mseed2seedpkt.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
