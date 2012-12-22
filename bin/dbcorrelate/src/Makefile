ldflags =
include $(ANTELOPEMAKE)
BIN = ../bin
RM = rm -i
CTARGS = dbcorrelate create_ccfile event_pairs hypodd2db
OBJS1 = get_tau.o stats.o

allprog:$(CTARGS)

dbcorrelate: dbcorrelate.o get_tau.o stats.o nint.o   
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ dbcorrelate.c get_tau.o stats.o nint.o $(DBLIBS) $(TRLIBS)

create_ccfile: create_ccfile.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ create_ccfile.c $(DBLIBS)

event_pairs:  event_pairs.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ event_pairs.c $(DBLIBS)

hypodd2db:  hypodd2db.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ hypodd2db.c $(DBLIBS)

cleanobj:
	$(RM) *.o

clobberbin:
	$(RM) -r $(BIN); mkdir $(BIN)
