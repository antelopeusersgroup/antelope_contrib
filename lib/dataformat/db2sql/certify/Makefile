ldlibs = -ldb2sql $(DBLIBS)

CLEAN = try_sqlcreate try_sqlinsert try_sqldelete try_syncfield

include $(ANTELOPEMAKE)

DIRS=

clean :: clean_dsym

clean_dsym:
	$(RM) -r *.dSYM

try_sqlinsert : try_sqlinsert.o private_compute_row_sync.o
	$(RM) $@
	$(CC) $(CFLAGS) -o try_sqlinsert try_sqlinsert.o private_compute_row_sync.o $(LDFLAGS) $(LDLIBS)

try_sqldelete : try_sqldelete.o private_compute_row_sync.o
	$(RM) $@
	$(CC) $(CFLAGS) -o try_sqldelete try_sqldelete.o private_compute_row_sync.o $(LDFLAGS) $(LDLIBS)

try_syncfield : try_syncfield.o private_compute_row_sync.o
	$(RM) $@
	$(CC) $(CFLAGS) -o try_syncfield try_syncfield.o private_compute_row_sync.o $(LDFLAGS) $(LDLIBS)

