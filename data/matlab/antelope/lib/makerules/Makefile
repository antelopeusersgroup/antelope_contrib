RULES=mex_antelope_makerules.linux \
      mex_antelope_makerules.solaris

include $(ANTELOPEMAKE)

Include:: mex_antelope_makerules

install:: mex_antelope_makerules

mex_antelope_makerules :: $(RULES)
	@case `uname -s` in \
	    SunOS) cp mex_antelope_makerules.solaris $(ANTELOPE)/include/mex_antelope_makerules;; \
	    Linux) cp mex_antelope_makerules.linux $(ANTELOPE)/include/mex_antelope_makerules;; \
	   *) echo "Error: System type `uname -s` not recognized." ;; \
	esac 
