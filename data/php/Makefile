
MYDLIB= Datascope.so

DATADIR= php

DATA= $(MYDLIB) \
      webdbe.inc \
      webdbe_style.css \
      webdbe_title.gif \
      webdbe_functions.js

MAN3= Datascope_PHP.3

CLEAN= $(MYDLIB)

cflags= -I$(PHP_INC) -I$(PHP_INC)/main -I$(PHP_INC)/Zend -I$(PHP_INC)/TSRM \
	-DHAVE_CONFIG_H -DPHP_ATOM_INC
ldlibs= $(DBLIBS)

include $(ANTELOPEMAKE)
DBG=
DIRS=

OBJS= Datascope.o
$(MYDLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(MYDLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
