
BIN=dbms
MAN1=dbms.1

ldlibs=	$(TRLIBS) $(F77LIBS)

include $(ANTELOPEMAKE)  	
DIRS=			

OBJS=	dbms.o dbutil.o domag.o proc_sta.o save_ms.o util.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
