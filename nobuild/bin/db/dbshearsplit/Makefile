FFLAGS  = -g
BIN = dbshearsplit_compute dbshearsplit_display
MAN1= dbshearsplit_compute.1 dbshearsplit_display.1
DATADIR = matlab/antelope/antelope
DATA = dbshearsplit_display.m
SCHEMADIR = schemas
SCHEMA = ssplit1.0

F77LIBS=-L/opt/SUNWspro/lib -L/opt/SUNWspro/SC5.0/lib -R/opt/SUNWspro/lib -R/opt/SUNWspro/SC5.0/lib -lM77 -lF77 -lsunmath 

COPT=-xO4 -dalign -xlibmil -fsimple=1 
CCOMMON=-i -D_REENTRANT 
DCFLAGS=-KPIC
DSUFFIX=.so.3
VERSION=3

LD=ld 
DLD=$(LD) 

LDRUN=-R$(ANTELOPE)/lib 
DLDFLAGS= $(ldflags) $(LDEXTRA) $(LDPATH) $(LDRUN) -G -ztext 

CXX=$(CCC)	# c++ on linux
CXXAR=$(CCC)
CXXDLD=$(CCC)

CXXARFLAGS=-xar -o
CXXDLDFLAGS=$(DCFLAGS) $(ldflags) $(LDEXTRA) $(LDPATH) $(LDRUN) -G 

LORDER=`lorder
TSORT=| tsort`

X11=/usr/openwin
X11LIBS=-L$(XLIB) -R$(XLIB) -lX11

TCLLDFLAGS = -L$(TCLLIB) -R$(TCLLIB) 

THREAD=-lthread
NSLLIBS = -lsocket -lnsl

CRYPT=
STOCKNEEDS=-lkstat -ldl -lposix4 -lnsl -lsocket -lm 

FFLAGS  = -D_REENTRANT $(DBG)  $(fflags) $(FEXTRA) -I$(ANTELOPE)/include -I$(ANTELOPE)/local/include -I$(XINCLUDE) $(DCFLAGS) -R/opt/SUNWspro/lib
CFLAGS  = $(CCOMMON) $(DBG) $(cflags) $(CEXTRA) -I$(ANTELOPE)/include -I$(ANTELOPE)/local/include -I$(XINCLUDE) $(DCFLAGS)
COFLAGS = $(CCOMMON) $(COPT) $(cflags) $(CEXTRA) -I$(ANTELOPE)/include -I$(ANTELOPE)/local/include -I$(XINCLUDE) $(DCFLAGS)
CCFLAGS = $(CCOMMON) $(DBG)  $(cflags) $(CEXTRA) -I$(ANTELOPE)/include -I$(ANTELOPE)/local/include -I$(XINCLUDE) $(DCFLAGS)

ASFLAGS = -P -D_ASM # -K PIC  # Remove the -K PIC for x86

RANLIB=:

.KEEP_STATE :
.PRECIOUS:$(LIB) $(DLIB)

MAKE=make -e

DIRS=`find * -type d -prune \( ! -name CVS ! -name .\* ! -name \*- ! -name +\* ! -name SunWS_cache \) | sort -f`

# The following lines would normally be in the
# default makefile rules, but Sun bungles the debuggers/compilation
# when a preprocessor is used on Fortran code.  However, 
# it's very useful to use the .F suffix for Fortran in order
# to include include files from a central location, rather than 
# embedding absolute paths into the include line, as the
# fortran include statement (as implemented by Sun)
# forces you to do.  The following rules always generate
# .f files from the .F files, and remove the # line# lines
# which cpp inserts (even with the -P flag, which it isn't supposed to).
# This will all work ok, if you're using .KEEP_STATE, however, 
# it can get confusing.

FPP=/usr/ccs/lib/cpp
COMPILE.F=$(FC) $(FFLAGS) $(FPPFLAGS) $(TARGET_ARCH) -c
LINK.F=$(FC) $(FFLAGS) $(FPPFLAGS) $(LDFLAGS) $(TARGET_ARCH)

# Cppflags is required in order to screen out compiler flags
# other than -I from FFLAGS, so that cpp will shut up about them.
# This is a hack, of course, and requires that options be
# mentioned separately, not all together, but I hope it will
# work most of the time.
Cppflags=`echo $(FFLAGS) $(FPPFLAGS) | sed 's/-[^I][^ ]*//g'`

.F.f :
	$(FPP) $(Cppflags) $*.F | sed '/^#/d' > $*.f"

.F:
	$(FPP) $(Cppflags) $*.F | sed '/^#/d' > $*.f"
	$(COMPILE.f) $(OUTPUT_OPTION) $*.f
	$(LINK.f) -o $@ $*.o $(LDLIBS)

.F.o:
	$(FPP) $(Cppflags) $*.F | sed '/^#/d' > $*.f"
	$(COMPILE.f) $(OUTPUT_OPTION) $*.f

.F.a:
	$(FPP) $(Cppflags) $*.F | sed '/^#/d' > $*.f"
	$(COMPILE.f) -o $% $*.f
	$(AR) $(ARFLAGS) $@ $%
	$(RM) $%

.f:
	$(COMPILE.f) $(OUTPUT_OPTION) $<
	$(LINK.f) -o $@ $@.o $(LDLIBS)
ANTELOPE_BASE=/opt/antelope
PERL=$(ANTELOPE)/bin/perl
DEST=$(ANTELOPE)$(SUBDIR)

DBG=-g

LEX= flex -v

CC=compile

LDPATH=-L$(ANTELOPE)/lib -L$(ANTELOPE)/static  -L$(ANTELOPE)/local/lib
LDFLAGS = $(ldflags) $(LDEXTRA) $(LDPATH) $(LDRUN) 

LDLIBS= $(ldlibs) 
DLDLIBS=$(ldlibs) 

XLIB=$(X11)/lib
XINCLUDE=$(X11)/include

PERFLIBS=-lperf 

TCLLIB=$(ANTELOPE_BASE)/tcltk8.4/lib 
TCLINCLUDE=-I$(ANTELOPE_BASE)/tcltk8.4/include
TKLIBS =  $(TCLLDFLAGS) -ltk -ltcl $(X11LIBS)
TCLLIBS = $(TCLLDFLAGS) -ltcl

STOCKLIBS=-lcoords -lbanner -lstock -ldeviants $(STOCKNEEDS) 
DBLIBS=-lds -ltrvltm -lresponse $(STOCKLIBS)
GPLLIBS=-lgpl -lol -ltks -lgrx $(X11LIBS)
TRLIBS=-ltr $(DBLIBS) 
ORBLIBS=-lxtra -lPkt -lforb -lorb $(TRLIBS) 

YFLAGS = -v -d -l 
CXXFLAGS = -I$(ANTELOPE)/include -I$(ANTELOPE)/local/include

INSTALL=deposit

all clean Include install installMAN pf relink tags uninstall :: FORCED
	@-DIRS="$(DIRS)" ;\
	for i in $$DIRS ; do \
	    printf "+    %-40s  %s\n" $$i "`date`" ; \
	    if [ -f $$i/Makefile -o -f $$i/makefile ] ; then ( cd $$i && $(MAKE) $@ ; ) ; else if [ -d $$i ] ; then echo Error: no Makefile in `pwd`/$$i ; fi ; fi ; \
        done

static ::
	$(MAKE) "LDFLAGS = $(ldflags) $(LDEXTRA) -L$(ANTELOPE)/static -L$(ANTELOPE)/lib $(LDRUN) -ldl"

# Occasionally, when a filesystem fills, .make.state is corrupted
# and then make refuses to work because of the corrupt .make.state files.
# "make purge" fixes the problem, for directories below the current directory.
purge :: FORCED
	@-find . -type f \( \
	    -name .make.state \
	    -o -name .#\* \
	    -o -name .depends \
	    -o -name .optimize.dir \
	    -o -name .optimize.pag \
	    -o -name .man\* \
	    -o -name \*- \
	    -o -name .make\* \
	    \) -exec rm \{\} \;
	@-find . -type d -name SunWS_cache -exec rm -rf \{\} \;

# purge removes a few other extra files also
#  .#* are cvs, 
#  .man* are fixman
# Be careful not to remove .no_optimizing

all:: ALL
	
clean ::
	@-$(RM) *.o *.a *.BAK
	@-$(RM) $(BIN) $(LIB) $(DLIB) $(CLEAN) .make.state tags 

Include::INCLUDE

install :: INCLUDE LIB BIN PF DATA MAN DEMO DOC EXAMPLE SCHEMA

pf :: PF

relink ::
	@$(RM) $(BIN) $(LIB) $(DLIB) ; $(MAKE) install

tags :: FORCED
	-ctags *.c *.f *.h *.l *.y

uninstall :: 
	@echo uninstalling $(BIN) $(PF) $(DATA) $(MAN1) $(INCLUDE) $(LIB) $(DLIB) $(MAN3) $(MAN3F) $(MAN3T) $(MAN3P) $(MAN5) $(MAN6) $(MAN8) $(UNINSTALL) $(DEMO) $(DOC) $(EXAMPLE) $(SCHEMA)
	@-$(RM) $(UNINSTALL)
	@-if [ -d $(DEST)/include ] ; then \
	    (cd $(DEST)/include ; $(RM) $(INCLUDE)) \
	fi
	@-if [ -d $(DEST)/man/man1 ] ; then \
	    installman -u $(DEST)/man/man1 $(MAN1) ; \
	fi
	@-if [ -d $(DEST)/man/man3 ] ; then \
	    installman -u $(DEST)/man/man3 $(MAN3) ; \
	fi
	@-if [ -d $(DEST)/man/man3f ] ; then \
	    installman -u $(DEST)/man/man3f $(MAN3F) ; \
	fi
	@-if [ -d $(DEST)/man/man3p ] ; then \
	    installman -u $(DEST)/man/man3p $(MAN3P) ; \
	fi
	@-if [ -d $(DEST)/man/man3t ] ; then \
	    installman -u $(DEST)/man/man3t $(MAN3T) ; \
	fi
	@-if [ -d $(DEST)/man/man5 ] ; then \
	    installman -u $(DEST)/man/man5 $(MAN5) ; \
	fi
	@-if [ -d $(DEST)/man/man6 ] ; then \
	    installman -u $(DEST)/man/man6 $(MAN6) ; \
	fi
	@-if [ -d $(DEST)/man/man8 ] ; then \
	    installman -u $(DEST)/man/man8 $(MAN8) ; \
	fi
	@-if [ -d $(DEST)/demo/$(DEMODIR) ] ; then \
	    (cd $(DEST)/demo/$(DEMODIR) ; $(RM) -r $(DEMO)) \
	fi
	@-if [ -d $(DEST)/doc/$(DOCDIR) ] ; then \
	    (cd $(DEST)/doc/$(DOCDIR) ; $(RM) -r $(DOC)) \
	fi
	@-if [ -d $(DEST)/example/$(EXAMPLEDIR) ] ; then \
	    (cd $(DEST)/example/$(EXAMPLEDIR) ; $(RM) -r $(EXAMPLE)) \
	fi
	@-if [ -d $(DEST)/data/$(DATADIR) ] ; then \
	    (cd $(DEST)/data/$(DATADIR) ; $(RM) -r $(DATA)) \
	fi
	@-if [ -d $(DEST)/data/$(SCHEMADIR) ] ; then \
	    (cd $(DEST)/data/$(SCHEMADIR) ; $(RM) -r $(SCHEMA)) \
	fi
	@-if [ -d $(DEST)/data/pf ] ; then \
	    (cd $(DEST)/data/pf ; $(RM) $(PF)) \
	fi
	@-if [ -d $(DEST)/bin ] ; then \
	    (cd $(DEST)/bin ; $(RM) $(BIN)) ; \
	fi 
	@-if [ -d $(DEST)/lib ] ; then \
	    (cd $(DEST)/lib ; $(RM) $(LIB) $(DLIB) $(DLIB:$(DSUFFIX)=.so)) \
	fi
	@-if [ -d $(DEST)/static ] ; then \
	    (cd $(DEST)/static ; $(RM) $(LIB)) \
	fi

ALL :  $(INCLUDE) $(LIB) $(DLIB) $(BIN) $(PF) $(DATA) $(DEMO) $(DOC) \
	$(EXAMPLE) \
	$(SCHEMA) \
	$(MAN1) $(MAN2) $(MAN3) \
	$(MAN4) $(MAN5) $(MAN6) \
	$(MAN7) $(MAN8) $(MAN9) \
	$(MANL)

BIN     :: $(BIN)      $(BIN:%=$(DEST)/bin/%) 
LIB     :: $(LIB) $(DLIB)   DLIB SLIB
SLIB    :: $(LIB)      $(LIB:%=$(DEST)/static/%) 
DLIB    :: $(DLIB)     $(DLIB:%=$(DEST)/lib/%) 
PF      :: $(PF)       $(PF:%=$(DEST)/data/pf/%) 
DATA    :: $(DATA)     $(DATA:%=$(DEST)/data/$(DATADIR)/%) 
SCHEMA  :: $(SCHEMA)   $(SCHEMA:%=$(DEST)/data/$(SCHEMADIR)/%) 
DEMO    :: $(DEMO)     $(DEMO:%=$(DEST)/demo/$(DEMODIR)/%) 
DOC     :: $(DOC)      $(DOC:%=$(DEST)/doc/$(DOCDIR)/%) 
EXAMPLE :: $(EXAMPLE)  $(EXAMPLE:%=$(DEST)/example/$(EXAMPLEDIR)/%) 
INCLUDE :: $(INCLUDE)  $(INCLUDE:%=$(DEST)/include/%) 


installMAN :: MAN

MAN :: $(MAN1:%=$(DEST)/man/man1/%) \
	$(MAN3:%=$(DEST)/man/man3/%) \
	$(MAN3F:%.3f=$(DEST)/man/man3f/%.3f) \
	$(MAN3P:%.3p=$(DEST)/man/man3p/%.3p) \
	$(MAN3T:%.3t=$(DEST)/man/man3t/%.3t) \
	$(MAN5:%=$(DEST)/man/man5/%) \
	$(MAN6:%=$(DEST)/man/man6/%) \
	$(MAN7:%=$(DEST)/man/man7/%) \
	$(MAN8:%=$(DEST)/man/man8/%) 
 
$(DEST)/lib/%$(DSUFFIX) : %$(DSUFFIX)
	$(INSTALL) $< $(DEST)/lib

$(DEST)/static/%.a : %.a
	$(INSTALL) $< $(DEST)/static
	$(RANLIB) $@

$(DEST)/bin/% : %
	$(INSTALL) $< $(DEST)/bin

$(DEST)/data/pf/% : %
	$(INSTALL) $< $(DEST)/data/pf

$(DEST)/include/% : %
	$(INSTALL) $< $(DEST)/include

$(DEST)/data/$(DATADIR)/% : % 
	$(INSTALL) $< $(DEST)/data/$(DATADIR)

$(DEST)/data/$(SCHEMADIR)/% : % 
	$(INSTALL) $< $(DEST)/data/$(SCHEMADIR)

$(DEST)/demo/$(DEMODIR)/% : % 
	$(INSTALL) $< $(DEST)/demo/$(DEMODIR)

$(DEST)/doc/$(DOCDIR)/% : % 
	$(INSTALL) $< $(DEST)/doc/$(DOCDIR)

$(DEST)/example/$(EXAMPLEDIR)/% : % 
	$(INSTALL) $< $(DEST)/example/$(EXAMPLEDIR)

$(DEST)/man/man1/% : % 
	$(INSTALL) $< $(DEST)/man/man1

$(DEST)/man/man3/% : % 
	$(INSTALL) $< $(DEST)/man/man3

$(DEST)/man/man3f/%.3f : %.3f 
	$(INSTALL) $< $(DEST)/man/man3f

$(DEST)/man/man3p/%.3p : %.3p 
	$(INSTALL) $< $(DEST)/man/man3p

$(DEST)/man/man3t/%.3t : %.3t
	$(INSTALL) $< $(DEST)/man/man3t

$(DEST)/man/man5/% : % 
	$(INSTALL) $< $(DEST)/man/man5

$(DEST)/man/man6/% : % 
	$(INSTALL) $< $(DEST)/man/man6

$(DEST)/man/man7/% : %
	$(INSTALL) $< $(DEST)/man/man7

$(DEST)/man/man8/% : % 
	$(INSTALL) $< $(DEST)/man/man8

.SUFFIXES: .xpl .xppl .xtcl .xwish .xvwish .xwish8 .csh 

% : %.xpl $(ANTELOPE)/data/templates/xpl
	produce $@

% : %.xppl $(ANTELOPE)/data/templates/xppl
	produce $@

.xtcl : $(ANTELOPE)/data/templates/xtcl
	produce $@

.xwish : $(ANTELOPE)/data/templates/xwish 
	produce $@

.xvwish : $(ANTELOPE)/data/templates/xvwish 
	produce $@

.xwish8 : $(ANTELOPE)/data/templates/xwish8
	produce $@

.csh : $(ANTELOPE)/data/templates/csh
	produce $@

FORCED :

dirs:
	@-echo directories: $(DIRS) | tr ' ' '\n'


depends:
	@echo " -> running makedepend in all directories below `pwd` ..."
	@-find . -type d ! \( \
	    -name CVS \
	    -o -name \*- \
	    \) -print | mkdepends -I$(ANTELOPE)/include -I$(ANTELOPE)/local/include $(TCLINCLUDE) -I$(XINCLUDE) 

#	Java rule

%.class: %.java
	javac $<

purify :
	$(MAKE) "CC=purify cc -g" "FC=purify f77 -g" all

purify1 :
	$(MAKE) "CC=purify -windows=no cc -g" "FC=purify f77 -g" all

DIRS=

F77LIBS=-L/opt/SUNWspro/lib -L/opt/SUNWspro/SC5.0/lib -R/opt/SUNWspro/lib -R/opt/SUNWspro/SC5.0/lib -lsunmath 

MFLAGS=-mt -M/opt/matlab/extern/lib/sol2/fexport.map -O -G -L/opt/matlab/bin/sol2 -I/opt/matlab/extern/include

MATLABLIBS=-lmx -lmex -lmat -lm -lcx -lc -L/opt/matlab/extern/lib

dbshearsplit_compute: dbshearsplit_compute.o ftnbridge.o shearlib.o tslib.o
	$(F77) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(DBLIBS) $(TRLIBS) $(F77LIBS)

dbshearsplit_display: dbshearsplit_display.o ftnbridge.o shearlib.o tslib.o
	$(F77) $(FFLAGS) $(LDFLAGS) -o $@ $^ $(DBLIBS) $(TRLIBS) $(F77LIBS)

dbshearsplit_compute.o:	dbshearsplit_compute.f
	$(F77) $(FFLAGS) $(LDFLAGS) -c $^ 

dbshearsplit_display.o:	dbshearsplit_display.f
	$(F77) $(FFLAGS) $(LDFLAGS) -c $^ 


