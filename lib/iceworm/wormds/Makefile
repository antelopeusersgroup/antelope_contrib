LIB= libwormds.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

CLEAN= $(LIB) $(DLIB) $(DLIB).[0-9]

INCLUDE= site_iw_ext.h

cflags=	-mt
fflags=
ldflags= 
ldlibs=

include $(ANTELOPEMAKE)

GLOBALFLAGS= -D_SPARC -D_SOLARIS
CFLAGS += $(GLOBALFLAGS)
		
DIRS=

OBJS= site_iw_ext.o \
      datasize.o \
      get_null_ak_ahhead.o \
      write_ak_ahheader.o \
      write_sac_header.o \
      save_waveform_header.o \
      save_waveform_data.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
