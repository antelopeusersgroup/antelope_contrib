BIN= dbnew2orb
PF=dbnew2orb.pf
MAN1=dbnew2orb.1
ldlibs=	-lbrttutil $(ORBLIBS)

include $(ANTELOPEMAKE)
DIRS=

#SUBDIR=/local
OBJS= dbnew2orb.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
