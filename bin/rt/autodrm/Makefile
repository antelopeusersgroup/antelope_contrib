BIN = \
	autodrm \
	autodrm2db \
	autodrm_wrapper \
	dbresp2autodrm \
	rtbulletin \
	try_autodrm

MAN1 = \
	autodrm.1 \
	autodrm2db.1 \
	autodrm_wrapper.1 \
	dbresp2autodrm.1 \
	rtbulletin.1 \
	try_autodrm.1

PF = autodrm.pf dbresp2autodrm.pf
DATADIR=autodrm
DATA=help forward

CLEAN=autodrm.pf

ldlibs=$(TRLIBS)

SUBDIR=/contrib
include $(ANTELOPEMAKE)

DIRS=

PIECES=autodrm.xpl \
	bulletin.pl \
	channel.pl \
	outage.pl \
	response.pl \
	station.pl \
	waveform.pl

autodrm : $(PIECES)
	cp -f $(ANTELOPE)/data/templates/xpl $@
	cat $(PIECES) >> $@
	chmod +x $@

autodrm_wrapper : autodrm_wrapper.id
	produce autodrm_wrapper
	chmod +x $@

autodrm.pf : autodrm.pf.id
	produce $@

help : help.id
	produce $@


try: rtbulletin
	./rtbulletin -s "orid==94" -v /opt/antelope/testdata/db/gsn/gsn 5/28/2009 24:00
