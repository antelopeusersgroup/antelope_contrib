CLEAN= darwin.pl

SUBDIR=/contrib
include $(ANTELOPEMAKE)

all :: darwin.pl

install :: darwin.pl

darwin.pl: darwin.pl.id
	getid < darwin.pl.id > darwin.pl
