#
#               Master makefile for Standard Graphics Package
#
# This makefile controls:
#	compiling, installation, and cleanup of user subroutines
#	compiling, installation, and cleanup of device filters
#	printing of documentation
#	making a distribution tape
#	listing of source code
#
# The user library will be installed in the following place:
SGPLIB= ./libstd.a
#
# The device filters will be installed in:
#DEVDIR= ./stdplt
DEVDIR= ../..
#
# The (tape) device for distribution copies is: the default tape device
# see makefile.old for using a tape device other than the default

# all: userlib filters install cleanup
all: userlib filters install

userlib:
	(cd plotsubs; make)

filters:
	(cd devs; make)

install:
	(cd devs;     make install DEVDIR=${DEVDIR})
	(cd plotsubs; make install SGPLIB=${SGPLIB})
	#(mv tkx ..; mv psigl ..; mv libstd.a ..)

xinstall:
	(cd devs;     make xinstall DEVDIR=${DEVDIR})
	(cd plotsubs; make  install SGPLIB=/usr/lib/libxstd.a)

cleanup:
	(cd devs;     make cleanup)
	(cd plotsubs; make cleanup)

tape:	cleanup
	tar cv  .
