BIN= mail_parser mail_parser_wrapper

DATADIR= perl
DATA= submit_pepp.pl \
      reb_import.pl \
      cnsn_pick.pl \
      pgc_pickfile.pl \
      mp_filemail.pl

PF= mail_parser.pf

MAN1= mail_parser.1 \
      mail_parser_wrapper.1 

MAN3= submit_pepp.3 \
      reb_import.3 \
      cnsn_pick.3 \
      pgc_pickfile.3 \
      mp_filemail.3

CLEAN=

include $(ANTELOPEMAKE)

mail_parser_wrapper : mail_parser_wrapper.csh.id
	produce $@
	chmod +x $@

