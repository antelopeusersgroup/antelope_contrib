LIB= libwormutil.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

CLEAN= $(LIB) $(DLIB) $(DLIB).[0-9]

INCLUDE= earthworm.h \
	kom.h \
	platform.h \
	trace_buf.h 

cflags=	
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)

GLOBALFLAGS= -D_SPARC -D_SOLARIS
CFLAGS += $(GLOBALFLAGS)
		
OBJS= 	chron3.o \
	getutil.o \
	kom.o \
	logit.o \
	site.o \
	tlay.o \
	mnbrak.o \
	brent.o \
	swap.o \
	socket_ew_common.o \
	mem_circ_queue.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
