RULES=mex_antelope_makerules \
      mex_antelope_makerules.linux \
      mex_antelope_makerules.sol

include $(ANTELOPEMAKE)

# Don't use the standard INCLUDE macro for the makerules
# or the uninstall will uninstall them, hamstringing 
# the rest of the uninstall for the Matlab tools. The 
# cost is acquiescing to the makerules never getting 
# uninstalled. 

Include :: $(RULES:%=$(ANTELOPE)/include/%)	
