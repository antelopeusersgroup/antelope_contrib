BIN= orb2eworm
DATADIR=misc
DATA=orb2eworm.d orb2eworm.desc
MAN1=orb2eworm.1
   
cflags= -D_SPARC -D_SOLARIS
ldflags= -xildoff 
ldlibs= -lpkt -lorb -lwormsol -lwormutil -lwormds $(TRLIBS) $(SVR4LIBS) -mt

include $(ANTELOPEMAKE)
