# @(#)Makefile	1.1 3/14/95 
# Makefile for extrd

BIN = extrd
cflags  = -g
ldlibs  = -lscv2 -ldbl2 $(TRLIBS)
 
include $(DSAPMAKE)


OBJS	= extrd.o\
	  create_dir.o\
	  dtsplit.o\
	  find_path.o\
	  get_data.o\
	  mkfname.o\
	  parse_name.o\
	  steim_util.o\
	  sig_hdlr.o\
	  util.o\
	  wrt_data.o
$(BIN):	$(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS)
