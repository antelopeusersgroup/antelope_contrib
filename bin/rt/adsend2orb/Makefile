BIN= adsend2orb
PF=adsend2orb.pf
MAN1=adsend2orb.1

ldlibs=	-lwormsol -lwormutil -lwormds -ldefunctpkt -lorb $(TRLIBS) $(SVR4LIBS) $(THREAD)

include $(ANTELOPEMAKE)  	

OBJS= $(BIN).o swap.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)

