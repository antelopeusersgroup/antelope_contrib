
extern Xlat	myOrbxlat[];
extern int	myNOrbxlat ;

Xlat myOrbxlat[] = {
    {"ORBCURRENT", ORBCURRENT },
    {"ORBPREV", ORBPREV },
    {"ORBNEXT", ORBNEXT },
    {"ORBNEXT_WAIT", ORBNEXT_WAIT },
    {"ORBOLDEST", ORBOLDEST },
    {"ORBNEWEST", ORBNEWEST },
    {"ORBNEXTT", ORBNEXTT },
    {"ORBPREVT", ORBPREVT },
} ;
 
int myNOrbxlat = sizeof(myOrbxlat) / sizeof(Xlat) ; 

