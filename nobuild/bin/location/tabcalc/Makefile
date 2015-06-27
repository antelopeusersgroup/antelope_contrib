
BIN=tabcalc hypotab ptt2stt
MAN1=tabcalc.1 hypotab.1
CLEAN= tabcalc hypotab ptt2stt
ldlibs=-lglp

SUBDIR=/contrib
include $(ANTELOPEMAKE)

TOBJS= tabcalc.o \
	shootup.o \
	ttable.o \
	ttime.o \
	ttout.o \
	gridtp.o \
	griderr.o \
	intrp.o \
	surftt.o \
	getp.o

tabcalc : $(TOBJS)
	$(FC) $(FFLAGS) -o $@ $(TOBJS) $(LDFLAGS) $(LDLIBS)
 
POBJS= ptt2stt.o
ptt2stt : $(POBJS)
	$(FC) $(FFLAGS) -o $@ $(POBJS) $(LDFLAGS) $(LDLIBS)

HOBJS= hypotab.o arriv1p.o toneout.o 
hypotab : $(HOBJS)
	$(FC) $(FFLAGS) -o $@ $(HOBJS) $(LDFLAGS) $(LDLIBS)
 

arriv1p.f:	table.common 
getp.f:		surface.common 
gridtp.f:	switch.common table.common model.common first.common title.common 
intrp.f:	model.common 
shootup.f:	table.common title.common upward.common surface.common \
		control.common
surftt.f:	surface.common 
toneout.f:	model.common first.common title.common
ttable.f:	switch.common table.common surface.common control.common \
		window.common 
tabcalc.f:	switch.common control.common title.common model.common \
		surface.common window.common table.common \
		upward.common lun.common
ttime.f:	table.common upward.common surface.common control.common \
		window.common
ttout.f:	lun.common 

# $Id$ 
