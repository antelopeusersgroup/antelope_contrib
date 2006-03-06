# Copyright (c) 2001 Boulder Real Time Technologies, Inc.
# All rights reserved 

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
	$(ANTELOPE)/src/java/com/brtt/antelope/Epoch.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/JFrameOrbStat.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/Orb.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/OrbClient.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/OrbErrorException.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/OrbPacket.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/OrbPacketChannel.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/OrbSource.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/OrbStat.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/SourcesModel.java \
	$(ANTELOPE)/src/java/com/brtt/antelope/Stock.java

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

antelope.jar	: remove_classes $(CLASSES)
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
	
