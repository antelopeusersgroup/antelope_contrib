LIB       = libdefunctpkt2.a

INCLUDE=_pkt2.h pkt2.h
#PF= pkt2.pf

OPTIMIZE=decompress.c

SUBDIR=/local
SUBDIR=/contrib
include $(ANTELOPEMAKE)
DIRS=

SRCS=   b3s2.c \
	dbug.c \
	decompress.c \
	initpf.c \
	netstachan.c \
	orbacc.c \
	orbsort.c \
	pktheader.c \
	pkts_init.c \
	pkttype.c \
	quanterra_pkt.c \
	querypkt.c \
	read_DP.c \
	read_IP.c \
	read_psclDP.c \
	read_psclIP.c \
	stuff_iw_tracebuf.c \
	stuff_liss.c \
	unstuffpkt.c \
	unstuff_hdr.c \
	unstuff_iw_tracebuf.c \
	util.c
	

OBJS=$(SRCS:.c=.o)
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)   
	$(RM) $@
	$(DLD) $(CONTRIBDLDFLAGS) -o $@ $(LORDER) $(OBJS) $(TSORT)

lint ::
	lint $(SRCS)

