
LIB= libisi.a
DOCDIR= ext/isi
DOC= isi.pdf iacp.pdf
INCLUDE= libisi_iacp.h \
	 libisi_isi.h \
	 libisi_logio.h \
	 libisi_list.h \
	 libisi_platform.h \
	 libisi_stdtypes.h \
	 libisi_util.h 

MAN3= libisi.3

CLEAN= *.h *.c *pdf

cflags= -D$(OSNAME) 
fflags=
ldflags=
ldlibs=

include $(ANTELOPEMAKE)

DIRS=

ISI_TOOLKIT_TARBALL= ISI_Toolkit.tgz
ISI_TOOLKIT_DIR=     ISI_Toolkit_1_2_2
ISI_INCLUDE_DIR=     $(ISI_TOOLKIT_DIR)/include
ISI_LIB_DIR=         $(ISI_TOOLKIT_DIR)/lib
UNPACKED=	     $(ISI_TOOLKIT_DIR)/unpacked

clean :: 
	$(RM) -rf $(ISI_TOOLKIT_DIR)

$(UNPACKED): $(ISI_TOOLKIT_TARBALL)
	gunzip -c $(ISI_TOOLKIT_TARBALL) | tar xf - 
	-$(RM) *.c *.h
	cp $(ISI_INCLUDE_DIR)/iacp.h libisi_iacp.h
	cp $(ISI_INCLUDE_DIR)/ida.h libisi_ida.h
	cp $(ISI_INCLUDE_DIR)/isi.h libisi_isi.h
	cp $(ISI_INCLUDE_DIR)/list.h libisi_list.h
	cp $(ISI_INCLUDE_DIR)/logio.h libisi_logio.h
	cp $(ISI_INCLUDE_DIR)/platform.h libisi_platform.h
	cp $(ISI_INCLUDE_DIR)/stdtypes.h libisi_stdtypes.h
	cp $(ISI_INCLUDE_DIR)/util.h libisi_util.h
	cp $(ISI_LIB_DIR)/*.c .
	perl -p -i -e "s/isi\.h/libisi_isi.h/" *.c *.h
	perl -p -i -e "s/iacp\.h/libisi_iacp.h/" *.c *.h
	perl -p -i -e "s/util\.h/libisi_util.h/" *.c *.h
	perl -p -i -e "s/list\.h/libisi_list.h/" *.c *.h
	perl -p -i -e "s/platform\.h/libisi_platform.h/" *.c *.h
	perl -p -i -e "s/stdtypes\.h/libisi_stdtypes.h/" *.c *.h
	perl -p -i -e "s/logio\.h/libisi_logio.h/" *.c *.h
	perl -p -i -e "s/date_time/isi_util_date_time/" *.c *.h
	touch $(UNPACKED)

$(INCLUDE): $(UNPACKED) 

$(DOC):
	$(RM) $@
	-cp $(ISI_TOOLKIT_DIR)/$@ .

IACP_OBJS= \
      iacp_auth.o \
      iacp_close.o \
      iacp_connect.o \
      iacp_free.o \
      iacp_get.o \
      iacp_handshake.o \
      iacp_open.o \
      iacp_recv.o \
      iacp_send.o \
      iacp_set.o \
      iacp_stats.o \
      iacp_string.o \
      iacp_util.o

ISI_OBJS = \
      isi_cnf.o \
      isi_datreq.o \
      isi_ezio.o \
      isi_log.o \
      isi_netutil.o \
      isi_open.o \
      isi_pack.o \
      isi_raw.o \
      isi_read.o \
      isi_seqno.o \
      isi_soh.o \
      isi_string.o \
      isi_ts.o \
      isi_utils.o \
      isi_wfdisc.o 

LOGIO_OBJS = \
      logio_dump.o \
      logio_init.o \
      logio_msg.o \
      logio_string.o \
      logio_threshold.o

UTIL_OBJS = \
      util_compress.o \
      util_etoh.o \
      util_hexdump.o \
      util_htoe.o \
      util_list.o \
      util_misc.o \
      util_pack.o \
      util_parse.o \
      util_peer.o \
      util_string.o \
      util_swap.o \
      util_timefunc.o \
      util_timer.o 
      
OBJS = $(IACP_OBJS) $(ISI_OBJS) $(LOGIO_OBJS) $(UTIL_OBJS)

$(OBJS): $(UNPACKED) 
	$(CC) $(CFLAGS) -c $*.c

$(LIB): $(OBJS) $(INCLUDE) $(UNPACKED)
	$(RM) $@
	$(AR) $(ARFLAGS) $@ $(OBJS) 
	$(RANLIB) $@


