# Makefile for cssio contribution to idatap library

LIB  = ../libidatap.a
cflags  = -I../include 
include $(ANTELOPEMAKE)
DIRS=
SRCS    = \
          css2sac.o  \
          datatype.o \
          jdate.o    \
          rwfdisc.o  \
          rwfdrec.o  \
          wdcut.o    \
          wf28to30.o \
          wf30to28.o \
          wwfdisc.o  \
          wrdsize.o
OBJS=$(SRCS:.c=.o)
$(LIB) : $(OBJS)
	$(AR) -qv $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)   
	$(DLD) $(DLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)

