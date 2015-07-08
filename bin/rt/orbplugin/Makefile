BIN=	orb_plugin

CLEAN= 		# Extra files which should be removed during a "make clean"

ldlibs= $(ORBLIBS)

include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=

OBJS=	orb_plugin.o plugin.o qcdatutil.o seedutil.o

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
