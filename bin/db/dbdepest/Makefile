BIN=	dbdepest
MAN1=dbdepest.1
PF=dbdepest.pf

fflags = -fast -w
#LDLIBS= $(ANTELOPE)/lib
GALDIR= ../teles
GALIBS= -lteles
#LIBS= -lteles -ltr -ldb -lstock -lcoords -ltttaup -lm

ldlibs= $(TRLIBS) $(GALIBS)

include $(ANTELOPEMAKE)
#
#

OBJ =dbdepest.o fftall.o rayle.o mkbutter3.o filt.o readpf.o
#
dbdepest:	$(OBJ)
	f77 $(FFLAGS) $(OBJ) $(LDFLAGS) -L$(GALDIR) $(LDLIBS) $(GALIBS) -o dbdepest

.f.o:
	f77 -c $(FFLAGS) $*.f
