DATADIR= matlab/antelope/html
DATA= Antelope_Toolbox_for_Matlab.pdf

CLEAN= *.backup.fm *.pdf fmbatch_script

include $(ANTELOPE)/include/mex_antelope_makerules

.SUFFIXES: .SUFFIXES .pdf .fm

.fm.pdf: pdf_settings.fm
	$(RM) fmbatch_script
	echo Open $< | cat > fmbatch_script
	echo Open pdf_settings.fm | cat >> fmbatch_script
	echo Print $< pdf_settings.fm | cat >> fmbatch_script
	echo Quit | cat >> fmbatch_script
	fmbatch fmbatch_script
	$(RM) fmbatch_script
