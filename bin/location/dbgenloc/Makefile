MAN1=dbgenloc.1
BIN=dbgenloc
PF=dbgenloc.pf dbgenloc_default.pf
#DATADIR=tables/genloc
#DATA=iasp91.pf iasp91.P.pf iasp91.S.pf test_model.pf kyrghyz.pf kyrghyz.P.pf kyrghyz.S.pf 

cflags = -dalign
ldflags = 

ldlibs=-lgenloc $(DBLIBS) -lsunperf $(F77LIBS)

#
# Turn off the above, and use something like the following if the 
# Sun Performance Library (sunperf) is not installed on your system.
#  This example uses the C version of LAPACK that should appear in
# the contrib area of new releases.
#
#ldlibs= -lgenloc   \
#-L/opt/dsap/ext/CLAPACK/F2CLIBS  -llapack_alpha  -lblas_alpha -lI77f2c -lF77f2c \
#$(DBLIBS) $(F77LIBS) -ltt -ldl
include $(ANTELOPEMAKE)
DIRS=ttlvz taup uniform

OBJS= dbgenloc.o run_location.o load_observations.o write_log.o save_results.o 
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
