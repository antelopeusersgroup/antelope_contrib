


BIN = orbstatj

DATADIR = java
DATA = antelope.jar
MAN1 = orbstatj.1

CLASSPATH=.
include $(ANTELOPEMAKE)
DIRS=

JAVAS = \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/Epoch.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/JFrameOrbStat.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/Orb.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbClient.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbErrorException.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbPacket.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbRawPacket.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbWaveformPacket.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbStringPacket.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbPacketChannel.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbSource.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbStat.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/SourceName.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/SourcesModel.java \
	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/Stock.java

#	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbParameterFilePacket.java \
#	$(ANTELOPE)/src/contrib/java/com/brtt/antelope/OrbDatabasePacket.java \

CLASSES = \
	com/brtt/antelope/Epoch.class \
	com/brtt/antelope/Stock.class \
	com/brtt/antelope/OrbClient.class \
	com/brtt/antelope/OrbErrorException.class \
	com/brtt/antelope/OrbPacket.class \
	com/brtt/antelope/OrbRawPacket.class \
	com/brtt/antelope/OrbWaveformPacket.class \
	com/brtt/antelope/OrbStringPacket.class \
	com/brtt/antelope/OrbPacketChannel.class \
	com/brtt/antelope/OrbSource.class \
	com/brtt/antelope/OrbStat.class \
	com/brtt/antelope/Orb.class \
	com/brtt/antelope/SourceName.class \
	com/brtt/antelope/SourcesModel.class \
	com/brtt/antelope/JFrameOrbStat.class 

#	com/brtt/antelope/OrbDatabasePacket.class \
#	com/brtt/antelope/OrbParameterFilePacket.class \

%.class : %.java
	javac $<

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
	
