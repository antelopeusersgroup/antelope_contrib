
# $Name $Revision$ $Date$ 
# Makefile for extrd

BIN = extrd
MAN1 = extrd.1
ldlibs= $(DBLIBS)

include $(DSAPMAKE) 

OBJS	= main.o \
	  find_path.o \
	  create_dir.o \
	  mkfname.o \
	  extract_data.o \
	  get_data.o \
	  parse_name.o \
	  print_wd.o \
	  rd_wfdisc.o \
	  time_util.o \
	  wfdcmp.o \
	  write_wfd.o

$(BIN):	$(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS) 


# $Id$ 
