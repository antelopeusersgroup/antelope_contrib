LIB= libwormsol.a
DLIB=$(LIB:.a=$(DSUFFIX)) 

INCLUDE= socket_ew.h \
	time_ew.h \
	transport.h \
	wave_client.h

cflags=	-D_SPARC -D_SOLARIS 
ldlibs=

include $(ANTELOPEMAKE)

OBJS=   copyfile.o \
        dirops_ew.o \
        getsysname_ew.o \
        getavail.o \
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
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
