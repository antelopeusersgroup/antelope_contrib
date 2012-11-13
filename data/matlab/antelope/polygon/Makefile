DATADIR=matlab/$(MATLAB_RELEASE)/antelope/antelope

DATA= $(MEXSRCS:.c=.$(MEXEXT)) \
      $(MEXSRCS:.c=.m)

HTMLDIR= matlab/html
HTML= $(MEXSRCS:.c=.html)

MEXSRCS= \
	readpolygon.c \
	inwhichpolygons.c \
	writepolygondata.c 

CLEAN= 


include $(ANTELOPE)/local/include/mex_antelope_makerules
DIRS=
#ldlibs=-lpolygon -lantelope_mex -lmex_orb
ldlibs= -lpolygon -lantelope_mex.$(MATLAB_RELEASE) -lmex_orb.$(MATLAB_RELEASE) $(DBLIBS)
