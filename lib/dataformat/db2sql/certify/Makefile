ldlibs = -ldb2sql $(DBLIBS)

CLEAN = try_sqlcreate try_sqlinsert try_sqldelete

include $(ANTELOPEMAKE)

DIRS=

clean :: clean_dsym

clean_dsym:
	$(RM) -r *.dSYM
