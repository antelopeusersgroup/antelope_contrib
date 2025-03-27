BIN  = db2sac db2ah
MAN1 = db2sac.1
LICENSES = license_db2sac.txt license_db2ah.txt

cflags = -I/usr/include/tirpc
ldlibs	= -lscv2 -ldbl2 $(TRLIBS)

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

db2ah : db2ah.o write_ah.o ioroutin.o tmpl.o
	$(CC) $(CFLAGS) -o db2ah db2ah.o write_ah.o ioroutin.o tmpl.o \
              $(LDFLAGS) $(LDLIBS)

db2ah.o : db2.c
	$(CC) $(CFLAGS) -DAH -c db2.c -o db2ah.o

db2sac : db2sac.o write_sac.o tmpl.o
	$(CC) $(CFLAGS) -o db2sac db2sac.o write_sac.o tmpl.o \
              $(LDFLAGS) $(LDLIBS)

db2sac.o : db2.c
	$(CC) $(CFLAGS) -DSAC -c db2.c -o db2sac.o

$(LICENSES) : LICENSE
	cp $< $@
