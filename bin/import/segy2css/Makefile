#$Name $Revision$ $Date$ 
# Makefile for sgy2css

BIN 	= segy2css
MAN1 = segy2css.1
ldlibs = $(DBLIBS)

include $(DSAPMAKE)
DIRS=

OBJS = decode.o \
	init.o \
	main.o \
	rd_segfil.o \
	tmutils.o \
	util.o \
	wrt_segdata.o \
	wrt_segwfd.o 

$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 

# $Id$ 
