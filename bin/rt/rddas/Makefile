BIN = rddas
MAN1=rddas.1
#
#ldlibs	= -lsocket -lnsl -B static -lorb -lpkt -ltr -lgpl2 -lol -lxcom -lgrx $(X11LIBS) -lcoords -lstock -lds -ltttaup -lresponse -lcoords -lstock -lposix4 -lm 

ldlibs	= -lsocket -lnsl -B static -lorb -lpkt -ltr -lds -lresponse -lcoords -lstock -B dynamic -lposix4 -lm 

include $(ANTELOPEMAKE)
OBJSM	= rddas.o\
	  openIP.o\
	  read_in_port.o
$(BIN):	$(OBJSM)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJSM) $(LDFLAGS) $(LDLIBS)
