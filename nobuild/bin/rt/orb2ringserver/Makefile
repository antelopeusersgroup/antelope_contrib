BIN     = orb2ringserver
MAN1    = $(BIN).1

cflags  = -Ilibmseed -Ilibdali
ldflags = -Llibmseed -Llibdali
ldlibs  = $(ORBLIBS) -lmseed -ldali

# Build embedded, dependent libraries statically
all install ::
	@echo Building libmseed
	@$(MAKE) -C libmseed static
	@echo Building libdali
	@$(MAKE) -C libdali static

 # Clean embedded, dependent libraries
clean ::
	@echo Cleaning libmseed
	@$(MAKE) -C libmseed $@
	@echo Cleaning libdali
	@$(MAKE) -C libdali $@

SUBDIR = /contrib
include $(ANTELOPEMAKE) # This line must appear following the definitions above, 
			# and before the DIRS definition below..

DIRS   = # Avoid automatic recursion into subdirectories

OBJS   = orb2ringserver.o

