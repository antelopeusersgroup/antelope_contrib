LIB= libwormds.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

MAN3=wave_client.3

INCLUDE=iceworm_extensions.h \
	iw_wfheader.h \
	wave_client.h \
	trace_buf.h \
	ak_ahheader.h 

cflags=	-mt -D_SPARC -D_SOLARIS

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
      save_waveform_data.o \
      wave_client.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
