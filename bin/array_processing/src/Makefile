ldflags =
SUBDIR=/contrib
include $(ANTELOPEMAKE)

BINDIR = ../bin
RM = rm -i
CTARGS = arf beam delay distaz funcgen kfgrid kfresp sigcor slow2d tcgrid
OBJS1 = get_tau.o stats.o

allprog:$(CTARGS)

arf: arf.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ arf.c $(DBLIBS)

beam: beam.c fft_sp.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ beam.c fft_sp.o $(DBLIBS) $(TRLIBS)

delay:  delay.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ delay.c $(DBLIBS)

distaz: distaz.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ distaz.c $(DBLIBS)

funcgen: funcgen.c stats.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ funcgen.c stats.o $(DBLIBS)

kfgrid: kfgrid.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ kfgrid.c $(DBLIBS) $(TRLIBS)

kfresp: kfresp.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ kfresp.c $(DBLIBS) $(TRLIBS)

sigcor: sigcor.c nint.o get_tau.o stats.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ sigcor.c nint.o get_tau.o stats.o $(DBLIBS) $(TRLIBS)

slow2d: slow2d.c fft_sp.o
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ slow2d.c fft_sp.o $(DBLIBS) $(TRLIBS)

tcgrid: tcgrid.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $(BINDIR)/$@ tcgrid.c $(DBLIBS) $(TRLIBS)

cleanobj:
	$(RM) *.o

clobberbin:
	$(RM) -r $(BINDIR); mkdir $(BINDIR)
