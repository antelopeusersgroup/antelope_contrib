CLEAN= matlab_release

ANTELOPEMAKELOCAL= $(ANTELOPEMAKE).local

include $(ANTELOPEMAKE)
include $(ANTELOPEMAKELOCAL)

matlab_release: $(ANTELOPEMAKELOCAL)
	$(MATLAB)/bin/matlab -nodisplay -nojvm -r "fid = fopen( 'matlab_release', 'w' ); fwrite(fid,['MATLAB_RELEASE=' version('-release')]); fclose( fid ); quit" 
	echo >> matlab_release
	echo MEX=$(MATLAB)/bin/mex >> matlab_release
	echo MEXEXT= `$(MATLAB)/bin/mexext` >> matlab_release

all :: matlab_release

