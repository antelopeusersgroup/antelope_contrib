PERLMAKEFILE = perl_makefile
AUTHOR_FIELD=Nikolaus Horn <nikolaus.horn\@zamg.ac.at>
MAN3P=perlwf.3p
CLEAN=  .make.state  $(PERLMAKEFILE).old Makefile.PL $(PERLMAKEFILE) runmake Makefile.PL wfutil.bs wfutil.c pm_to_blib 

PD=$(ANTELOPE)$(SUBDIR)/data/perl
PM=$(ANTELOPE)$(SUBDIR)/man

ldlibs=$(TRLIBS) 
include $(ANTELOPEMAKE)
DIRS=
#SUBDIR=/local
SUBDIR=

SRCS= errlog2string.c wfutil.pm wfutil.xs 
install:: runmake $(SRCS) $(PERLMAKEFILE) 
	./runmake install 

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
