BIN=dbgenloc
PF=dbgenloc.pf 
#DATADIR=tables/genloc
#DATA=iasp91.pf iasp91.P.pf iasp91.S.pf test_model.pf kyrghyz.pf kyrghyz.P.pf kyrghyz.S.pf 

cflags = 
ldflags = 
ldlibs= -lgenloc -lblas $(DBLIBS) $(F77LIBS) -ltt -ldl
include $(DSAPMAKE)
DIRS=ttlvz

OBJS= dbgenloc.o run_location.o load_observations.o write_log.o save_results.o 
CC=cc
$(BIN):$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

# $Id$ 
