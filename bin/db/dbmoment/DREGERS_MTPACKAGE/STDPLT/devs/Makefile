#
# Master makefile for IGL device filters.
#
#
# To add or delete specific filters, modify the list variable DEVS
# There must be a directory for each device.
#
#DEVS= ima plotdb psigl tkx sun ver zeta xigl sun 
DEVS= psigl tkx
#
# The AUXDIRS contain code that is common to two or more filters.
# They are listed here so they can be listed and cleaned up.
#
AUXDIRS= dum com scan
#
# The following are parameters that need to get passed to subsidiary Makefiles:
#
DEVDIR =	../..
PRINT =		imprint
#

all:	compile

${DEVS}:	FORCE
	(cd $@; make)

compile:
	for i in ${DEVS}; do  (echo  dev $$i :; cd $$i; make); done

install:
	for i in ${DEVS}; do \
		(echo  dev $$i :; cd $$i; make install DEVDIR=${DEVDIR}); done

xinstall:
	for i in ${DEVS}; do \
		(echo  dev $$i :; cd $$i; make xinstall DEVDIR=${DEVDIR}); done

list:
	for i in ${DEVS} ${AUXDIRS}; do \
		(cd $$i; make list PRINT=${PRINT}); done
	
cleanup:
	for i in ${DEVS} ${AUXDIRS};    do \
		(echo  dev $$i :; cd $$i; make cleanup); done

FORCE:
