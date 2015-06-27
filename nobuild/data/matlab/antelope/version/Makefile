CLEAN= matlab_release

ANTELOPEMAKELOCAL= $(ANTELOPE)/local/include/antelopemake.local

SUBDIR=/contrib
include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

SUBDIR= /local

matlab_release: $(ANTELOPEMAKELOCAL)
	$(MATLAB)/bin/matlab -nodisplay -nojvm -r "fid = fopen( 'matlab_release', 'w' ); fwrite(fid,['MATLAB_RELEASE=' version('-release')]); fclose( fid ); quit" 
	echo >> matlab_release
	echo MEX=$(MATLAB)/bin/mex >> matlab_release
	echo MEXEXT= `$(MATLAB)/bin/mexext` >> matlab_release

all :: matlab_release
