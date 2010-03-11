ldlibs = -ldb2sql $(DBLIBS)

CLEAN = try_sqlcreate try_sqlinsert

include $(ANTELOPEMAKE)

DIRS=

clean :: clean_dsym

clean_dsym:
	$(RM) -r *.dSYM
