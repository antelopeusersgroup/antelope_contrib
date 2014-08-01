#include "stock.h"

extern Xlat	myPfxlat[];
extern int	myNPfxlat ;

Xlat myPfxlat[] = {
#ifdef PFPROMPT
    {"PFPROMPT", PFPROMPT },
#endif
    {"PFTBL", PFTBL },
    {"PFARR", PFARR },
    {"PFINVALID", PFINVALID },
    {"PFSTRING", PFSTRING },
    {"PFANY", PFANY },
    {"PFFILE", PFFILE },
} ;
 
int myNPfxlat = sizeof(myPfxlat) / sizeof(Xlat) ; 
