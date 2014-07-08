# Copyright (c) 2001 Boulder Real Time Technologies, Inc.
# All rights reserved
# vim: ft=make

# This software module is wholly owned by Boulder Real Time
# Technologies, Inc. Any use of this software module without
# express written permission from Boulder Real Time Technologies,
# Inc. is prohibited.

BIN = orbstatj

DATADIR = java
DATA = antelope.jar
MAN1 = orbstatj.1

include $(ANTELOPEMAKE)
DIRS=

JAVAS = \
	com/brtt/antelope/Epoch.java \
	com/brtt/antelope/JFrameOrbStat.java \
	com/brtt/antelope/Orb.java \
	com/brtt/antelope/OrbClient.java \
	com/brtt/antelope/OrbErrorException.java \
	com/brtt/antelope/OrbPacket.java \
	com/brtt/antelope/OrbPacketChannel.java \
	com/brtt/antelope/OrbSource.java \
	com/brtt/antelope/OrbStat.java \
	com/brtt/antelope/SourcesModel.java \
	com/brtt/antelope/Stock.java

CLASSES = \
	com/brtt/antelope/Epoch.class \
	com/brtt/antelope/JFrameOrbStat.class \
	com/brtt/antelope/Orb.class \
	com/brtt/antelope/OrbClient.class \
	com/brtt/antelope/OrbErrorException.class \
	com/brtt/antelope/OrbPacket.class \
	com/brtt/antelope/OrbPacketChannel.class \
	com/brtt/antelope/OrbSource.class \
	com/brtt/antelope/OrbStat.class \
	com/brtt/antelope/SourcesModel.class \
	com/brtt/antelope/Stock.class

antelope.jar	: $(CLASSES)
	jar cvf antelope.jar com/brtt/antelope/*.class

$(ANTELOPE)/man/javadoc : $(JAVAS)
	if [ -d $(ANTELOPE)/man/javadoc ] ; then $(RM) -r $(ANTELOPE)/man/javadoc ; fi
	mkdir $(ANTELOPE)/man/javadoc
	javadoc -d $(ANTELOPE)/man/javadoc $(JAVAS)

CLEAN=antelope.jar
clean:: remove_classes

remove_classes:
	@-(cd com/brtt/antelope ; $(RM) `ls | grep '.class$$' | grep -v CVS` )

install::   $(ANTELOPE)/man/javadoc

uninstall::
	@echo uninstalling $(ANTELOPE)/man/javadoc ; if [ -d $(ANTELOPE)/man/javadoc ] ; then $(RM) -r $(ANTELOPE)/man/javadoc ; fi

