
extern Xlat	myOrbxlat[];
extern int	myNOrbxlat ;

Xlat myOrbxlat[] = {
    {"ORBCURRENT", ORBCURRENT },
    {"ORBPREV", ORBPREV },
    {"ORBNEXT", ORBNEXT },
    {"ORBNEXT_WAIT", ORBNEXT_WAIT },
    {"ORBOLDEST", ORBOLDEST },
    {"ORBNEWEST", ORBNEWEST },
} ;
 
int myNOrbxlat = sizeof(myOrbxlat) / sizeof(Xlat) ; 

