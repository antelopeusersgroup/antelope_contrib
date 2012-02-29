AUTHOR_FIELD=Nikolaus Horn <nikolaus.horn\@zamg.ac.at>
PERLMAKEFILE = perl_makefile
MAN3P=perlpolygon.3p
CLEAN=  Makefile.PL runmake $(PERLMAKEFILE).old $(PERLMAKEFILE).PL $(PERLMAKEFILE) polygon.bs polygon.c pm_to_blib 

PD=$(ANTELOPE)/data/perl
PM=$(ANTELOPE)/man

ldlibs=-lpolygon $(STOCKLIBS)
include $(ANTELOPEMAKE)
DIRS=

SRCS= polygon.pm polygon.xs 
install:: runmake $(SRCS) $(PERLMAKEFILE) 
	touch polygon.pm # so time is new in $antelope/data/perl
	./runmake install 
	@-chmod -R ug+rw blib
	@-chmod -R ug+rw $(ANTELOPE)/data/perl

all:: $(SRCS) $(PERLMAKEFILE) runmake
	./runmake all

clean::
	rm -rf blib
	
$(PERLMAKEFILE) : Makefile.PL
	$(PERL) Makefile.PL INSTALLPRIVLIB=$(PD) INSTALLSCRIPT=$(PD) INSTALLSITELIB=$(PD) INSTALLSITELIB=$(PD) INSTALLMAN3DIR=$(PM)/man3

Makefile.PL : Makefile.PL.proto 
	sed -e 's@LinkArguments@$(LDFLAGS) $(LDLIBS)@' \
	    -e 's@AuthorField@$(AUTHOR_FIELD)@' \
	    -e 's@CompileArguments@$(CFLAGS)@' < Makefile.PL.proto > $@
	
#perlpolygon.3 : perlpolygon.pm
#	pod2man --section 3p perlpolygon.pm $@
