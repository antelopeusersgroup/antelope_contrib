INCLUDE=cmplx.h
LIB=libahio.a libcmplx.a libts.a libfft.a

include $(ANTELOPEMAKE)

libahio.a : ioroutin.o
	$(RM) $@
	$(AR) $(ARFLAGS) $@ ioroutin.o
	$(RANLIB) $@

libahio$(DSUFFIX) : ioroutin.o
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $@ $(DLDLIBS) $(LORDER) $(DOBJS) $(TSORT) 

libcmplx.a : cmplx.o
	$(RM) $@
	$(AR) $(ARFLAGS) $@ cmplx.o
	$(RANLIB) $@

libcmplx$(DSUFFIX) : cmplx.o
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $@ $(DLDLIBS) $(LORDER) $(DOBJS) $(TSORT) 

libts.a : tssubs.o
	$(RM) $@
	$(AR) $(ARFLAGS) $@ tssubs.o 
	$(RANLIB) $@

libts$(DSUFFIX) : tssubs.o
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $@ $(DLDLIBS) $(LORDER) $(DOBJS) $(TSORT) 

libfft.a : fftsubs.o
	$(RM) $@
	$(AR) $(ARFLAGS) $@ fftsubs.o
	$(RANLIB) $@

libfft$(DSUFFIX) : fftsubs.o
	$(RM) $@
	$(DLD) $(DLDFLAGS) -o $@ $(DLDLIBS) $(LORDER) $(DOBJS) $(TSORT) 

