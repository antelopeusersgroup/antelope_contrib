LIB= libwormsol.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

CLEAN= $(LIB) $(DLIB) $(DLIB).[0-9]

INCLUDE= socket_ew.h \
	transport.h \
	util_ext.h \
	wave_client.h

cflags=	-mt
fflags=
ldflags= 
ldlibs=

include $(ANTELOPEMAKE)

GLOBALFLAGS= -D_SPARC -D_SOLARIS
CFLAGS += $(GLOBALFLAGS)
		
OBJS= 	copyfile.o \
	dirops_ew.o \
	getavail.o \
	getsysname_ew.o \
	wave_client.o \
	pipe.o \
	sema_ew.o \
	sendmail.o \
	sendpage.o \
	sleep_ew.o \
	socket_ew.o \
	threads_ew.o \
	time_ew.o \
	transport.o \
	errexit.o \
	wait_timer.o

$(LIB) : $(OBJS)
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
