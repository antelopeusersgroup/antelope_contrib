BIN=reftek_dbtiming
MAN1=reftek_dbtiming.1
cflags=-g
ldflags= 
ldlibs=$(DBLIBS)

include $(ANTELOPEMAKE)  	

OBJS=reftek_dbtiming.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

