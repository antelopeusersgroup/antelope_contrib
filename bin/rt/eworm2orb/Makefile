BIN= eworm2orb
DATADIR=misc
DATA=eworm2orb.d eworm2orb.desc
INCLUDE=
 
MAN1=eworm2orb.1
  
CLEAN= $(BIN)

ICEWORMLIBS= -lwormsol -lwormutil -lwormds $(TRLIBS) $(SVR4LIBS)
   
cflags= -mt 
fflags=
ldflags= -xildoff 
ldlibs= -lpkt -lorb $(ICEWORMLIBS)

include $(ANTELOPEMAKE)

GLOBALFLAGS= -D_SPARC -D_SOLARIS
CFLAGS += $(GLOBALFLAGS)
 
OBJS = eworm2orb.o 

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
