#include "Hypocenter.h"
#include "db.h"

namespace SEISPP
{

/*! \brief Extended Hypocenter object containing all attributes in CSS3.0
  origin table.

  Sometimes it is useful to have not just the core Hypocenter object concept
  but an extended version containing all the attributes of the css3.0 
  origin table.  This object is effectively an abstraction of one row
  of an origin table. 
  */

class HypocenterCSS30 : public Hypocenter
{
public:
    /*! See Antelope CSS3.0 documentation for defintion of these attributes.*/
    long orid,evid,jdate;
    long nass,ndef,ndp;
    long grn, srn;
    char etype[3];
    char dtype[2];
    char review[5];
    char algorithm[16];
    char auth[16];
    long commid;
    double mb,ms,ml;
    long mbid,msid,mlid;
    Dbptr dbthis;  /* Holds Dbptr from which this was generated */
    HypocenterCSS30(Dbptr db,Hypocenter& h);
    HypocenterCSS30(const HypocenterCSS30& parent);
    HypocenterCSS30& operator=(const HypocenterCSS30& parent);
    long dbsave(Dbptr db);
};

}

