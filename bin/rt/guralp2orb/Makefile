BIN= guralp2orb
MAN1=guralp2orb.1
PF=guralp2orb.pf

cflags = 
ldflags =
ldlibs=  $(THREAD) -lbrttutil $(ORBLIBS) 

include $(ANTELOPEMAKE)  	
DBG=-g

DIRS=

OBJS = $(BIN).o 

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $(BIN) $(OBJS) $(LDFLAGS) $(LDLIBS) 

ll:guralp2orb.c
	cc -i -v -D_REENTRANT -g    -I/opt/antelope/dev/include -I/usr/openwin/include -KPIC  -c  guralp2orb.c -o guralp2orb.ll -Zll

