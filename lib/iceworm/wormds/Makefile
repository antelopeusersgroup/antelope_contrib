LIB= libwormds.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

INCLUDE= iceworm_extensions.h

# ldlibs= -lwormsol -lwormutil -lwormds $(TRLIBS) $(SVR4LIBS) 
cflags = -D_SPARC -D_SOLARIS 

include $(ANTELOPEMAKE)
DIRS=

OBJS= site_iw_ext.o \
      chdir_ewparams.o \
      datasize.o \
      getavail_anydisk.o \
      get_null_ak_ahhead.o \
      write_ak_ahheader.o \
      write_sac_header.o \
      save_waveform_header.o \
      save_waveform_data.o

$(LIB) : $(OBJS)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(RM) $@
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
