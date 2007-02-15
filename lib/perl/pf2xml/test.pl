: # use perl
eval 'exec perl -S $0 "$@"'
if 0;

use lib "$ENV{ANTELOPE}/data/perl" ;

use Datascope;
use Datascope::pf2xml;

$pfname = "site";

$header = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n";

print pf2xml( "-n", "-s", $pfname, "anode", $header );
