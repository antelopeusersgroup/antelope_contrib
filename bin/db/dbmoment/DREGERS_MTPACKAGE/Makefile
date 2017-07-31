#FF = gfortran
#FC = g77
#
LIBS = $(PLTLIB) -lm

BIN = fromHelm \
	mkHelm \
	window \
	fortran_fkrprog \
	wvint9 \
	tdmt_inv

SUBDIR=/contrib

include $(ANTELOPEMAKE)

CFLAGS+=-Wno-return-type -Wno-implicit-int -Wno-implicit
CFLAGS+=-Wno-format -Wno-unused-variable -Wno-int-conversion
CFLAGS+=-Wno-implicit-function-declaration

DIRS =

fromHelm: getpar.c
	$(CC) $(CFLAGS) -o $@ fromHelm.c fixbase.c getpar.c

mkHelm: getpar.c
	$(CC) $(CFLAGS) -o $@ mkHelm.c getpar.c

window: getpar.c
	$(CC) $(CFLAGS) -o $@ window.c getpar.c

fortran_fkrprog:
	$(FC) $(FFLAGS) -o $@ fortran_fkrprog.f

wvint9:
	$(FC) $(FFLAGS) -o $@ wvint9.f cwrite.c


CSRC = tdmt_invc_iso.c minvdbl.c correl2b.c readhelm.c fitcheck2_iso.c mt_plot_dat.c
OBJS2 = $(CSRC:%.c=%.o)

FSRCSUB = mtmanip_module.f fmap_subs_new.f
FOBJSUB = $(FSRCSUB:%.f=%.o)


tdmt_inv: $(FOBJSUB) $(OBJS2)
	$(FC) $(FFLAGS) -o $@ $(OBJS2) $(FOBJSUB) $(LIBS)

.c.o:
	$(CC) $< -c $(CFLAGS)

.f.o:
	$(FC) $< -c $(FFLAGS)
