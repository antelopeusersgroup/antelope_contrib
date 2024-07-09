#library
DATADIR= python/zamg
DATA=db2obspy.py obspy2db.py __init__.py 

#binaries
BIN=testread test_zamg_obspy obspy-ppsd
include $(ANTELOPEMAKE)
DIRS=scripts
SUBDIR=/contrib

__init__.py:
	@touch $@

% : %.id
	getid < $@.id > $@
	chmod a+x $@
