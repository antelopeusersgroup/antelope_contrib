BIN =dbdepest
MAN1=dbdepest.1
PF  =dbdepest.pf

ldflags = -L../teles
ldlibs= -lteles $(TRLIBS) 

include $(ANTELOPEMAKE)

OBJ =dbdepest.o fftall.o rayle.o mkbutter3.o filt.o readpf.o
dbdepest: $(OBJ)
	$(FC) $(FFLAGS) $(OBJ) $(LDFLAGS) $(LDLIBS) $(GALIBS) -o dbdepest
