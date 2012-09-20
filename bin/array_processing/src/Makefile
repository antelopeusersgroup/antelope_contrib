ldflags =
include $(ANTELOPEMAKE)
BIN = ../bin
RM = rm -i
CTARGS = arf beam delay distaz funcgen kfgrid kfresp sigcor slow2d tcgrid
OBJS1 = get_tau.o stats.o

allprog:$(CTARGS)

arf:    
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ arf.c $(DBLIBS)

beam:   beam.o fft_sp.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ beam.c fft_sp.o $(DBLIBS) $(TRLIBS)

delay:  delay.o  
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ delay.c $(DBLIBS)

distaz: distaz.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ distaz.c $(DBLIBS)

funcgen:funcgen.o stats.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ funcgen.c stats.o $(DBLIBS)

kfgrid: kfgrid.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ kfgrid.c $(DBLIBS) $(TRLIBS)

kfresp: kfresp.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ kfresp.c $(DBLIBS) $(TRLIBS)

sigcor:	sigcor.o nint.o get_tau.o stats.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ sigcor.c nint.o get_tau.o stats.o $(DBLIBS) $(TRLIBS)

slow2d: slow2d.o fft_sp.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ slow2d.c fft_sp.o $(DBLIBS) $(TRLIBS)

tcgrid: tcgrid.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BIN)/$@ tcgrid.c $(DBLIBS) $(TRLIBS)

cleanobj:
	$(RM) *.o

clobberbin:
	$(RM) -r $(BIN); mkdir $(BIN)
