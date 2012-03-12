MAN3P = atws_mwp.3p

PERLMAKEFILE = perl_makefile

AUTHOR_FIELD = ATWS

PD = $(ANTELOPE)/data/perl
PM = $(ANTELOPE)/man

CLEAN = Makefile.PL runmake $(PERLMAKEFILE).old $(PERLMAKEFILE).PL $(PERLMAKEFILE) atws_mwp.bs atws_mwp.c pm_to_blib 

ldlibs = $(DBLIBS)

include $(ANTELOPEMAKE)
DIRS=

SRCS = atws_mwp.pm atws_mwp.xs

install:: runmake $(SRCS) $(PERLMAKEFILE) 
	touch atws_mwp.pm 
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
