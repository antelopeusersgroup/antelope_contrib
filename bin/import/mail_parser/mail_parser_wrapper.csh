cd ~
set ANTELOPE=/opt/antelope/4.2u
source $ANTELOPE/setup.csh
setenv PFPATH $ANTELOPE/data/pf
exec $ANTELOPE/bin/mail_parser
