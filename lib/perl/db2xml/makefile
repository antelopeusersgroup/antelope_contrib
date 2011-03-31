AUTHOR_FIELD=Dr. Kent Lindquist <kent\@lindquistconsulting.com>
PERLMAKEFILE = perl_makefile
MAN3P=perldb2xml.3p
CLEAN=  Makefile.PL runmake $(PERLMAKEFILE).old $(PERLMAKEFILE).PL $(PERLMAKEFILE) db2xml.bs db2xml.c pm_to_blib 

PD=$(ANTELOPE)/data/perl
PM=$(ANTELOPE)/man

ldlibs=-ldbxml $(DBLIBS)
include $(ANTELOPEMAKE)
DIRS=

SRCS= db2xml.pm db2xml.xs
install:: runmake $(SRCS) $(PERLMAKEFILE) 
	touch db2xml.pm # so time is new in \$antelope/data/perl
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
