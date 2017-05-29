BIN = fromHelm \
	mkHelm \
	window \
	sac2bin \
	fortran_fkrprog \
	wvint9 \
	putmt \
	tdmt_invc

SUBDIR=/contrib

include $(ANTELOPEMAKE)

CFLAGS+=-Wno-return-type -Wno-implicit-int -Wno-implicit
CFLAGS+=-Wno-format -Wno-unused-variable -Wno-int-conversion
CFLAGS+=-Wno-implicit-function-declaration

DIRS= GETPAR C_CODE

%.o: %.c
	$(CC) $(cflags) -o $@ $< libget.a c_libs.a

libget.a:
	(cd GETPAR; make;)

c_libs.a:
	(cd C_CODE; make;)


fromHelm: libget.a
	$(CC) $(CFLAGS) -o fromHelm fromHelm.c fixbase.c libget.a

mkHelm: libget.a
	$(CC) $(CFLAGS) -o mkHelm mkHelm.c libget.a

window: libget.a
	$(CC) $(CFLAGS) -o window window.c libget.a

sac2bin: libget.a
	$(CC) $(CFLAGS) -o sac2bin sac2bin.c libget.a

putmt: libget.a
	$(CC) $(CFLAGS) -g -o putmt putmt.c readhelm.c libget.a -lm

fortran_fkrprog:
	$(FC) $(FFLAGS) -o $@ fortran_fkrprog.f

wvint9:
	$(FC) $(FFLAGS) -o $@ wvint9.f cwrite.c

tdmt_invc: c_libs.a  tdmt_invc_linux.o minvdbl_linux.o correl2b_linux.o readhelm.o  fitcheck2.o fmap_subs_linux.o
	$(FC) $(FFLAGS) -o tdmt_invc tdmt_invc_linux.o minvdbl_linux.o correl2b_linux.o readhelm.o fitcheck2.o fmap_subs_linux.o c_libs.a
