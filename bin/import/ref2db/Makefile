# %W% %G% 
# Makefile for ref2db

BIN = ref2db
MAN1=ref2db.1   

ldlibs	=-Bstatic -lconfirm -lpkt -lorb -ltr -lds -ltttaup -lresponse -lcoords -lstock -Bdynamic -lm $(SVR4LIBS)

include $(ANTELOPEMAKE)
OBJS 	= ref2db.o\
	  openIP.o\
	  parfile.o\
	  read_input.o\
	  read_psclIP.o\
	  record.o\
	  save_seed.o\
	  util.o\
	  valid_pkt.o\
	  wrt2db.o
$(BIN):	$(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS)
