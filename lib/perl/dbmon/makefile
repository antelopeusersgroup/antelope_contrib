AUTHOR_FIELD=Dr. Kent Lindquist <kent\@lindquistconsulting.com>
PERLMAKEFILE = perl_makefile
MAN3P=perldbmon.3p
CLEAN=  Makefile.PL runmake $(PERLMAKEFILE).old $(PERLMAKEFILE).PL $(PERLMAKEFILE) dbmon.bs dbmon.c pm_to_blib 

PD=$(ANTELOPE)/data/perl
PM=$(ANTELOPE)/man

ldlibs=-ldbmon $(DBLIBS)
include $(ANTELOPEMAKE)
DIRS= hints

SRCS= dbmon.pm dbmon.xs

install:: runmake $(SRCS) $(PERLMAKEFILE) 
	./runmake install 
	@-chmod -R ug+rw blib
	@-chmod -R ug+rw $(ANTELOPE)/data/perl
	$(RM) $(ANTELOPE)/data/perl/perllocal.pod
	touch $(ANTELOPE)/data/perl/Datascope/dbmon.pm

all:: $(SRCS) $(PERLMAKEFILE) runmake
	./runmake all

clean::
	@if [ -x ./runmake ] ; then ./runmake realclean ; fi
	rm -rf blib
	
$(PERLMAKEFILE) : Makefile.PL
	$(PERL) Makefile.PL INSTALLPRIVLIB=$(PD) INSTALLSCRIPT=$(PD) INSTALLSITEARCH=$(PD) INSTALLSITELIB=$(PD) INSTALLSITELIB=$(PD) INSTALLMAN3DIR=$(PM)/man3

Makefile.PL : Makefile.PL.proto 
	sed -e 's@LinkArguments@$(LDFLAGS) $(LDLIBS)@' \
	    -e 's@AuthorField@$(AUTHOR_FIELD)@' \
	    -e 's@CompileArguments@$(CFLAGS)@' < Makefile.PL.proto > $@
