BIN = localmake_config
PF = localmake_config_proto.pf

MAN1 = localmake_config.1
MAN5 = antelopemakelocal.5

include $(ANTELOPEMAKE)

# Always install localmake_config to work around compile-dependency tangles
all clean Include :: install

install :: 
	(cd .. ; localmake_config -i)
