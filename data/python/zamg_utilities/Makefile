
DATADIR= python/zamg

DATA= __init__.py utilities.py mailutils.py

MAN3Y=init_statefile.3y multipartemail.3y send_multipartemail.3y update_statefile.3y \
	  zamg_utilities.3y zamg_mailutils.3y utf8len.3y string_charsplit.3y string_maxbytes.3y \
	  create_dbdesc.3y rfc33392epoch.3y epoch2rfc3339.3y \
	  spherical_distance.3y haversine_distance.3y next_number.3y \
	  set_remark.3y add_remark.3y get_remark.3y mark_remark.3y

CLEAN= mkso *.pyc *.so __init__.py \
	   init_statefile.3y update_statefile.3y \
	   utf8len.3y string_charsplit.3y string_maxbytes.3y \
	   create_dbdesc.3y rfc33392epoch.3y epoch2rfc3339.3y \
	   spherical_distance.3y haversine_distance.3y \
	   multipartemail.3y send_email.3y add_remark.3y get_remark.3y set_remark.3y mark_remark.3y next_number.3y

include $(ANTELOPEMAKE)
SUBDIR=/contrib
DIRS=

mkso:
	produce $@
init_statefile.3y:	mkso
	./mkso utilities.py zamg_utilities.3y
multipartemail.3y:	mkso
	./mkso mailutils.py zamg_mailutils.3y
__init__.py:
	@touch $@
