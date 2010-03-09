ldlibs = -ldb2sql $(DBLIBS)

CLEAN = try_db2sql 

include $(ANTELOPEMAKE)

DIRS=

clean :: clean_dsym

clean_dsym:
	$(RM) -r *.dSYM
