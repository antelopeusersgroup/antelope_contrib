# Makefile for seed contribution to idatap library
cflags  = -I../include
include $(ANTELOPEMAKE)
DIRS=
SRCS	=\
          b1000.o \
          fsdh.o \
          mini.o \
          sint.o \
          time.o
OBJS=$(SRCS:.c=.o)
../libidatap.a : $(OBJS)
	$(AR) -qv $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)

