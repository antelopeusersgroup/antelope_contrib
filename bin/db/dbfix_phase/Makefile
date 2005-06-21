BIN= dbfix_phase
PF=dbfix_phase.pf
MAN1=dbfix_phase.1
CLEAN= 
cflags=
ldflags=
ldlibs=	$(DBLIBS)
include $(ANTELOPEMAKE)
SUBDIR=/local
DIRS=	
OBJS= dbfix_phase.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

