
DSDLIB= _polygon.so 
DLIB=python__polygon$(DSUFFIX)

DATADIR= python/antelope

DATA= $(DSDLIB) polygon.pyc 

MAN3Y= pythonpolygon.3y

CLEAN= *.pyc *.so

cflags= -I$(PYTHONINCLUDE)
fflags=
ldflags= 
ldlibs= -lpyutil -lpolygon $(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DSOBJS= _polygon.o

$(DSDLIB) : $(DSOBJS)
	$(RM) $@
	$(DLD) $(CONTRIBDLDFLAGS) -o $(DSDLIB) $(DSOBJS) $(DLDLIBS) 

$(DLIB): $(DSDLIB)
	$(RM) $@
	@cp $(DSDLIB) $(DLIB)

