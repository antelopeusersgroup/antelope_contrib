BIN= orb2eworm
PF=orb2eworm.d orb2eworm.desc
MAN1=orb2eworm.1
   
cflags= -D_SPARC -D_SOLARIS -mt
ldflags= -xildoff 
ldlibs= -lPkt -lorb -lwormsol -lwormutil -lwormds $(TRLIBS) $(SVR4LIBS) -mt

PACKAGE=$(ANTELOPE)/contrib
include $(ANTELOPEMAKE)
