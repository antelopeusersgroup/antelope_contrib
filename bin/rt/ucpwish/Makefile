BIN = ucpwish

cflags =  $(TCLINCLUDE)
ldflags = -L$(TCLLIB)
ldlibs = -lrtucp $(TKLIBS) -ltkx $(TCLLIBS) -ltclx -lpkt -lorb $(TRLIBS) 

include $(ANTELOPEMAKE)
DIRS=

ucpwish : ucpwish.o tkXmain.o  
	$(CC) $(CFLAGS) -o $@ ucpwish.o tkXmain.o $(LDFLAGS) $(LDLIBS)

