LIB= libwormsol.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

INCLUDE=transport.h \
	time_ew.h \
	socket_ew.h \
	wait_timer.h


cflags=	-mt -D_SPARC -D_SOLARIS

PACKAGE=$(ANTELOPE)/contrib
include $(ANTELOPEMAKE)
		
OBJS= 	copyfile.o \
	dirops_ew.o \
	getsysname_ew.o \
	getavail.o \
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
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
