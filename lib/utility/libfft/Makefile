LIB=fftlib.a

include $(ANTELOPEMAKE)  	
OBJS=fftsubs.o
$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@
