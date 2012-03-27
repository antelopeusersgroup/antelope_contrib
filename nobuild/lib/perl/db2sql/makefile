AUTHOR_FIELD=Dr. Kent Lindquist <kent\@lindquistconsulting.com>
PERLMAKEFILE = perl_makefile
MAN3P=perldb2sql.3p
CLEAN=  Makefile.PL runmake $(PERLMAKEFILE).old $(PERLMAKEFILE).PL $(PERLMAKEFILE) db2sql.bs db2sql.c pm_to_blib 

PD=$(ANTELOPE)/data/perl
PM=$(ANTELOPE)/man

ldlibs=-ldb2sql $(DBLIBS)
include $(ANTELOPEMAKE)
DIRS= hints

SRCS= db2sql.pm db2sql.xs

install:: runmake $(SRCS) $(PERLMAKEFILE) 
	./runmake install 
	@-chmod -R ug+rw blib
	@-chmod -R ug+rw $(ANTELOPE)/data/perl
	$(RM) $(ANTELOPE)/data/perl/perllocal.pod
	touch $(ANTELOPE)/data/perl/Datascope/db2sql.pm

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
