AUTHOR_FIELD=Dr. Kent Lindquist <kent\@lindquistconsulting.com>
PERLMAKEFILE = perl_makefile
MAN3P=perlpf2xml.3p
CLEAN=  Makefile.PL runmake $(PERLMAKEFILE).old $(PERLMAKEFILE).PL $(PERLMAKEFILE) pf2xml.bs pf2xml.c pm_to_blib 

PD=$(ANTELOPE)/data/perl
PM=$(ANTELOPE)/man

ldlibs=-lpfxml $(DBLIBS)
include $(ANTELOPEMAKE)
DIRS=

SRCS= pf2xml.pm pf2xml.xs
install:: runmake $(SRCS) $(PERLMAKEFILE) 
	touch pf2xml.pm # so time is new in \$antelope/data/perl
	./runmake install 

all:: $(SRCS) $(PERLMAKEFILE) runmake
	./runmake all

clean::
	rm -rf blib
	
$(PERLMAKEFILE) : Makefile.PL
	$(PERL) Makefile.PL INSTALLPRIVLIB=$(PD) INSTALLSCRIPT=$(PD) INSTALLSITEARCH=$(PD) INSTALLSITELIB=$(PD) INSTALLSITELIB=$(PD) INSTALLMAN3DIR=$(PM)/man3

Makefile.PL : Makefile.PL.proto 
	sed -e 's@LinkArguments@$(LDFLAGS) $(LDLIBS)@' \
	    -e 's@AuthorField@$(AUTHOR_FIELD)@' \
	    -e 's@CompileArguments@$(CFLAGS)@' \
	    -e 's@-framework IOKit@@' \
	    -e 's@-framework CoreFoundation@@' < Makefile.PL.proto > $@
