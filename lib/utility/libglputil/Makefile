LIB=libglputil.a
DLIB=$(LIB:.a=$(DSUFFIX)) 
DATADIR=
DATA=
INCLUDE=glputil.h
MAN3=check_required_pf.3 \
  ftest.3  \
  dbform_working_view.3 \
  glp_matrix_utils.3

MAN5=		# man pages for file formats

cflags=-g
ldlibs= $(DBLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)  	


OBJS=dbform_working_view.o  ftest.o matrix_subs.o pfutils.o
$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@

$(DLIB) : $(OBJS)
	$(DLD) $(DLDFLAGS) $(LDLIBS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)
