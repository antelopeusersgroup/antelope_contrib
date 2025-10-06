
DSDLIB= _missing.so 
DLIB=python__missing$(DSUFFIX)

DATADIR= python/zamg

DATA= $(DSDLIB) missing.py __init__.py 

NEWMAN = TIME2SAMP.3y SAMP2TIME.3y SAMPRATE.3y NSAMP.3y ENDTIME.3y wfchanid.3y arrchanid.3y
NEWMAN = dbget_calib.3y TIME2SAMP.3y SAMP2TIME.3y SAMPRATE.3y NSAMP.3y ENDTIME.3y dbget_remark.3y dbadd_remark.3y
MAN3Y= python_missing.3y $(NEWMAN)

CLEAN= mkso *.pyc *.so __init__.py $(NEWMAN) python__*

cflags= -I$(PYTHONINCLUDE)
fflags=
ldlibs= $(PYTHONLIBS) $(TRLIBS) -lpyutil -lxtra

include $(ANTELOPEMAKE)
SUBDIR= /contrib

DSOBJS= _missing.o
mkso:
	produce $@
dbget_calib.3y:      mkso
	./mkso missing.py python_missing.3y
TIME2SAMP.3y:		mkso
	./mkso missing.py python_missing.3y

__init__.py:
	@touch $@

$(DSDLIB) : $(DSOBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(DSDLIB) $(DSOBJS) $(DLDLIBS) 

$(DLIB): $(DSDLIB)
	$(RM) $@
	@cp $(DSDLIB) $(DLIB)
