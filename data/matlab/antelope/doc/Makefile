DOCDIR= matlab
DOC= \
    Antelope_Toolbox_for_Matlab-1.gif \
    Antelope_Toolbox_for_Matlab-2.gif \
    Antelope_Toolbox_for_Matlab-3.gif \
    Antelope_Toolbox_for_Matlab-4.gif \
    Antelope_Toolbox_for_Matlab-5.gif \
    Antelope_Toolbox_for_Matlab-6.gif \
    Antelope_Toolbox_for_Matlab.css \
    Antelope_Toolbox_for_Matlab.fm \
    Antelope_Toolbox_for_Matlab.html \
    Antelope_Toolbox_for_Matlab.pdf 

CLEAN= *.backup.fm *.pdf fmbatch_script fmbatch_wrapper

include $(ANTELOPE)/include/mex_antelope_makerules

.SUFFIXES: .SUFFIXES .pdf .fm

.fm.pdf: pdf_settings.fm fmbatch_wrapper
	make fmbatch_wrapper
	$(RM) fmbatch_script
	echo Open $< | cat > fmbatch_script
	echo Open pdf_settings.fm | cat >> fmbatch_script
	echo Print $< pdf_settings.fm | cat >> fmbatch_script
	echo Quit | cat >> fmbatch_script
	./fmbatch_wrapper $< $@
	$(RM) fmbatch_script
