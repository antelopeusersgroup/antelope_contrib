BIN =dbdepest
MAN1=$(BIN).1
PF  =$(BIN).pf

ldflags = -L../teles
ldlibs= -lteles $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

OBJ=$(BIN).o fftall.o rayle.o mkbutter3.o filt.o readpf.o
$(BIN): $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) $(LDFLAGS) $(LDLIBS) $(GALIBS) -o $(BIN)
