BIN=setdne

MAN1=		# man pages for commands


cflags=-g
ldlibs=$(DBLIBS)		# libraries to link in.

include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 
OBJS=	setdne.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
