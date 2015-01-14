ldlibs = -ldbmon $(DBLIBS)

CLEAN = try_dbmon

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

clean :: clean_dsym

clean_dsym:
	$(RM) -r *.dSYM
