INCLUDE= antelope_mex.h

CLEAN=

cflags=
fflags=
ldflags=
ldlibs=

include ../../antelope_mex_make
DBG += -argcheck

OBJS= 	  variables2mxArray.o \
	  dbptr_struct.o \
	  get_malloced_string.o \
	  antelope_mexUsageMsgTxt.o \
	  stringtbl2cellstr.o \
	  cellstr2stringtbl.o \
	  pfarr2struct.o \
	  pftbl2cellarr.o \
	  mxArray2Response.o \
	  mxArray2Pf.o \
	  mxPfprompt.o \
	  mxPfprompt_string.o \
	  mxTranslate_Boolean.o \
	  pfput_mxArray.o \
	  pfpeek.o \
	  get_variables.o \
	  antelope_mex_clear_register.o \
	  dbfield2mxArray.o \
	  dbcolumn2mxArray.o \
	  mxArray2dbvalue.o \
	  orbpkt2mxArray.o \
	  SCAFFOLD_fix_tr_endtime.o

$(LIB) : $(OBJS)
	rm -f $@
	$(AR) $(ARFLAGS) $@ $(LORDER) $(OBJS) $(TSORT)
	$(RANLIB) $@
