AUTHOR_FIELD=Nikolaus Horn <nikolaus.horn\@zamg.ac.at>
MAN3P=perlpolygon.3p
CLEAN=  .make.state  runmake Makefile.old Makefile.PL Makefile polygon.bs polygon.c pm_to_blib 

PD=$(ANTELOPE)/data/perl
PM=$(ANTELOPE)/man

ldlibs=-lpolygon -lm
include $(ANTELOPEMAKE)
DIRS=

SRCS= errlog2string.c polygon.pm polygon.xs 
install:: runmake $(SRCS) Makefile 
	touch polygon.pm # so time is new in $antelope/data/perl
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
