BIN = ucpwish

cflags =  $(TCLINCLUDE)
#ldflags = -L../rtucp -L$(TCLLIB)
ldflags = -L$(TCLLIB)
ldlibs = -lrtucp -ltcldb $(TKLIBS) $(TCLLIBS) -lpkt -lorb $(TRLIBS) $(SVR4LIBS)

include $(ANTELOPEMAKE)
DIRS=

ucpwish : ucpwish.o tkXmain.o  
	$(CC) $(CFLAGS) -o $@ ucpwish.o tkXmain.o $(LDFLAGS) $(LDLIBS)

