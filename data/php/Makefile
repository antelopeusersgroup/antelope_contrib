
MYDLIB= Datascope.so

DATADIR= php

DATA= $(MYDLIB) \
      webdbe.inc \
      webdbe_style.css

MAN1=
MAN3= phpdb.3
MAN5=

CLEAN= $(MYDLIB)

PHP= /usr/local/include/php

cflags= -I$(PHP) -I$(PHP)/main -I$(PHP)/Zend -I$(PHP)/TSRM \
	-DHAVE_CONFIG_H -DPHP_ATOM_INC
fflags=
ldflags=
ldlibs= $(DBLIBS)

include $(ANTELOPEMAKE)
DBG=
DIRS=

OBJS= Datascope.o
$(MYDLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $(MYDLIB) $(LORDER) $(OBJS) $(TSORT) $(DLDLIBS) 
