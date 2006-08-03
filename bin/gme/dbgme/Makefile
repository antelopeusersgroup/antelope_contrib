
BIN= dbgme 
PF= dbgme.pf 
INCLUDE= dbgme.h

MAN1= dbgme.1 
MAN3= dbgme_delegates.3 trinetsm_es99.3 sp_bssa87.3

cflags= 
fflags=
ldflags= 
ldlibs= $(TRLIBS) -lcgeom  

include $(ANTELOPEMAKE)
DBG= -g

DIRS= 

OBJS= dbgme.o \
      sp_bssa87.o \
      trinetsm_es99.o \
      delegate_to_matlab.o \
      delegate_to_perl.o  

dbgme : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
