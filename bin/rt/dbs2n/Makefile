#
#	Makefile for dbs2n
#
BIN=		dbs2n

# MAN1=		dbs2n.1

CLEAN= 		# Extra files which should be removed during a "make clean"

cflags=		-g
fflags= 	-g
ldflags= 	# flags to linker/loader
ldlibs=		-lscv2 -ldbl2 $(TRLIBS)

include $(ANTELOPEMAKE)	# This line must appear following the definitions above, 
			# and before the DIRS definition below..
DIRS=			# Omit this line to run make in subdirectories, or
			# fill in to run make only in certain subdirectories

#OBJS=	 dbs2n.o sc_subs.o trace_subs.o filter_subs.o butwth.o lddate.o
OBJS=	 dbs2n.o sc_subs.o trace_subs.o filter_subs.o lddate.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
