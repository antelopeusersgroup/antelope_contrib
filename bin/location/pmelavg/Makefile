BIN=pmelavg
PF=pmelavg.pf

MAN1=	pmelavg.1
ldlibs= -lgenloc -lglputil $(DBLIBS)  \
-ltrvltm -ldl -lm -lperf
include $(ANTELOPEMAKE)  	# This line must appear following the definitions above, 

OBJS=pmelavg.o
$(BIN) : $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
