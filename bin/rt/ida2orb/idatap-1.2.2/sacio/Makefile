# Makefile for sacio contribution to idatap library
cflags  = -I../include
include $(ANTELOPEMAKE)
DIRS=
SRCS    = \
         rah.o    \
         rbh.o    \
         sttodt.o \
         wad.o    \
         wah.o    \
         wbh.o
OBJS=$(SRCS:.c=.o)
../libidatap.a : $(OBJS)
	$(AR) -qv $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)

