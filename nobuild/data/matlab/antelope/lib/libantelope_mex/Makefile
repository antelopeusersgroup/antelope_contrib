DLIB= libantelope_mex.$(MATLAB_RELEASE)$(DSUFFIX)
INCLUDE= antelope_mex.h

CLEAN=

cflags= 
fflags=
ldflags=
ldlibs=

include $(ANTELOPE)/local/include/mex_antelope_makerules

OBJS= 	  variables2mxArray.o \
	  dbptr_struct.o \
	  get_malloced_string.o \
	  antelope_mexUsageMsgTxt.o \
	  stringtbl2cellstr.o \
	  cellstr2stringtbl.o \
	  pfarr2struct.o \
	  pftbl2cellarr.o \
	  mxArray2CGGrid.o \
	  mxArray2Response.o \
	  mxArray2Pf.o \
	  matlabPfprompt.o \
	  mxPfprompt.o \
	  mxPfprompt_string.o \
	  mxTranslate_Boolean.o \
	  pfput_mxArray.o \
	  pfstring2mxArray.o \
	  pfpeek.o \
	  get_variables.o \
	  antelope_mex_clear_register.o \
	  antelope_mex_elog_callback.o \
	  dbfield2mxArray.o \
	  dbcolumn2mxArray.o \
	  mxArray2dbvalue.o \
	  orbpkt2mxArray.o \
	  dbprocess_error.o 

$(DLIB) : $(OBJS) dsymutil_dir
	$(DLD) $(DLDFLAGS) -o $(DLIB) $(LORDER) $(OBJS) $(TSORT)

dsymutil_dir:
	mkdir -p $(ANTELOPE)/dsymutil
