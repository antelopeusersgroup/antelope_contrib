LIB=libglputil.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
DATADIR=
DATA=
INCLUDE=glputil.h
MAN3=
MAN5=		# man pages for file formats

cflags=-g
ldlibs= $(DBLIBS)

include $(ANTELOPEMAKE)  	


OBJS=dbform_working_view.o  ftest.o matrix_subs.o pfutils.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
