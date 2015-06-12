BIN = localmake_config

MAN1 = localmake_config.1
MAN5 = antelopemakelocal.5

LICENSES = license_localmake_config.txt

CLEAN = $(LICENSES)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

# Always install localmake_config to work around compile-dependency tangles
all clean Include :: install

install ::
	(cd .. ; localmake_config -if)

$(LICENSES) : LICENSE
	cp $< $@
