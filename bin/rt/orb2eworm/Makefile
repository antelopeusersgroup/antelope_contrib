BIN= orb2eworm
DATADIR=misc
DATA=orb2eworm.d orb2eworm.desc
INCLUDE=
 
MAN1=orb2eworm.1
  
CLEAN= $(BIN)

ICEWORMLIBS= -lwormsol -lwormutil -lwormds $(TRLIBS) $(SVR4LIBS)
   
cflags= -mt 
fflags=
ldflags= -xildoff 
ldlibs= -lpkt -lorb $(ICEWORMLIBS)

include $(ANTELOPEMAKE)

GLOBALFLAGS= -D_SPARC -D_SOLARIS
CFLAGS += $(GLOBALFLAGS)
 
OBJS = orb2eworm.o 

$(BIN): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
