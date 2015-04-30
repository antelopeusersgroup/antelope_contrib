BIN = example2wfdisc \
	fromHelm \
	mkHelm \
	window \
	sac2bin \
	bin2sac \
	tdmt_invc \
	fortran_fkrprog \
	wvint9 \
	putmt

cflags = -Wno-return-type -Wno-implicit-int
#cflags = -arch x86_64 -Wno-return-type -Wno-format -Wno-uninitialized -Wno-unused-variable -Wno-implicit-int -Wno-int-conversion -Wno-implicit-function-declaration
#fflags = -m64

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS= plotsubs GETPAR C_CODE

%.o: %.c
	$(CC) $(cflags) -o $@ $<

libget.a:
	(cd GETPAR; make;)

libstd.a: libget.a
	(cd plotsubs; make;)

nrcv2.a:
	(cd C_CODE; make;)

fromHelm: libget.a fromHelm.c fixbase.c
	$(CC) -o fromHelm fromHelm.c fixbase.c libget.a

mkHelm: libget.a mkHelm.c
	$(CC) -o mkHelm mkHelm.c libget.a

window: libget.a window.c
	$(CC) -o window window.c libget.a

sac2bin: libget.a sac2bin.c
	$(CC) -o sac2bin sac2bin.c libget.a

bin2sac: libget.a bin2sac.c
	$(CC) -o bin2sac bin2sac.c libget.a

putmt: libget.a
	$(CC) -g -o putmt putmt.c readhelm.c libget.a -lm

fortran_fkrprog:
	$(FC) $(fflags) -o $@ fortran_fkrprog.f

wvint9:
	$(FC) $(fflags) -o $@ wvint9.f cwrite.c

tdmt_invc: nrcv2.a libstd.a tdmt_invc_linux.o minvdbl_linux.o correl2b_linux.o readhelm.o  fitcheck2.o mt_plot6iso2_linux2.o fmap_subs_linux.o
	$(FC) $(fflags) -o tdmt_invc tdmt_invc_linux.o minvdbl_linux.o correl2b_linux.o readhelm.o mt_plot6iso2_linux2.o fitcheck2.o fmap_subs_linux.o nrcv2.a libstd.a -lm 
