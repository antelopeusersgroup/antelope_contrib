DSDLIB= _polygon.so 
DLIB=python__polygon$(DSUFFIX)

DATADIR= python/zamg

DATA= $(DSDLIB) polygon.py __init__.py 

MAN3Y= pythonpolygon.3y

CLEAN= *.pyc *.so

cflags= -I$(PYTHONINCLUDE) -I$(ANTELOPE)/contrib/include
fflags=
ldflags=-L$(ANTELOPE)/contrib/lib -L$(ANTELOPE)/contrib/static
ldlibs= $(PYTHONLIBS) -lpyutil -lpolygon

include $(ANTELOPEMAKE)
SUBDIR= /contrib

DSOBJS= _polygon.o

$(DSDLIB) : $(DSOBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DSDLIB) $(DSOBJS) $(DLDLIBS) 

$(DLIB): $(DSDLIB)
	$(RM) $@
	@cp $(DSDLIB) $(DLIB)
