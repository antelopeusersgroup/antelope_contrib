AUTHOR_FIELD=Nikolaus Horn <nikolaus.horn\@zamg.ac.at>
MAN3=perlpolygon.3
CLEAN=  .make.state  runmake Makefile.old Makefile.PL Makefile polygon.bs polygon.c pm_to_blib 

PD=$(ANTELOPE)/local/data/perl
PM=$(ANTELOPE)/local/man

ldlibs=-lpolygon -lm
ldflags=-L$(ANTELOPE)/local/lib -R$(ANTELOPE)/local/lib
cflags=-I$(ANTELOPE)/local/include
include $(ANTELOPEMAKE)
DIRS=
#SUBDIR=/local

SRCS= errlog2string.c polygon.pm polygon.xs 
install:: runmake $(SRCS) Makefile 
	./runmake install 

all:: $(SRCS) Makefile runmake
	./runmake all

clean::
	rm -rf blib
	
Makefile : Makefile.PL
	$(PERL) Makefile.PL INSTALLPRIVLIB=$(PD) INSTALLSCRIPT=$(PD) INSTALLSITELIB=$(PD) INSTALLSITELIB=$(PD) INSTALLMAN3DIR=$(PM)/man3

Makefile.PL : Makefile.PL.proto 
	sed -e 's@LinkArguments@$(LDFLAGS) $(LDLIBS)@' \
	    -e 's@AuthorField@$(AUTHOR_FIELD)@' \
	    -e 's@CompileArguments@$(CFLAGS)@' < Makefile.PL.proto > $@
	
#perlpolygon.3 : perlpolygon.pm
#	pod2man --section 3p perlpolygon.pm $@
