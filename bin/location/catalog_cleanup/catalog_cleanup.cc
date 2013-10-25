#include <stdlib.h>
#include <string.h>
#include <list>
#include "coords.h"
#include "seispp.h"
#include "dbpp.h"
#include "HypocenterCSS30.h"
using namespace std;
using namespace SEISPP;
void usage()
{
	cerr << "catalog_clean dbin dbout tolerance\n";
	exit(-1);
}
Hypocenter load_hypo(Dbptr db)
{
	string method("tttaup");
	string model("iasp91");
	double lat,lon,depth,otime;
	dbgetv(db,0,"lat",&lat,
		"lon",&lon,
		"depth",&depth,
		"time",&otime,NULL);
	return(Hypocenter(rad(lat),rad(lon),depth,otime,method,model));
}

double space_time_difference(Hypocenter& h1, Hypocenter& h2)
{
	const double velocity(6.0);
	const double Re(6371.0);
	double dist=h1.distance(h2.lat,h2.lon);
	dist*=Re;
	double sqrdist=dist*dist;
	double dz,dt;
	dz=fabs(h1.z-h2.z);
	dt=fabs(h1.time-h2.time);
	dt*=velocity;
	sqrdist += dz*dz + dt*dt;
	return(sqrt(sqrdist));
}
	
	
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
	Hypocenter currenthypo,previoushypo;
	char dbrow[256];
	long orid;
	if(argc!=4)usage();
	string dbin(argv[1]);
	string dbout(argv[2]);
	double tolerance=atof(argv[3]);
	cout << "catalog_clean:  Writing to output db="<<dbout<<endl
		<<"Using input from origin table of db="<<dbin<<endl
		<< "Space time tolerance value="<<tolerance<<endl;
	int nrecin=0,nrecout=0;
	DatascopeHandle dbhi(dbin,true);
	DatascopeHandle dbho(dbout,false);
	DatascopeHandle dbhoe(dbho);
	dbho.lookup("origin");
	dbhoe.lookup("event");
	dbhi.lookup("origin");
	list<string> sortkeys;
	sortkeys.push_back("time");
	dbhi.sort(sortkeys);
	dbhi.rewind();
	if(dbhi.number_tuples()<=0)
	{
		cerr << "Origin table in input database has no rows"<<endl;
		usage();
	}
	dbhi.rewind();
	bool firstpass;
        list<HypocenterCSS30> eventlist;
        long nrows=dbhi.number_tuples();
	for(nrecin=0,firstpass=true;nrecin<nrows;
			++dbhi,++nrecin)
	{
		int irec;
		currenthypo=load_hypo(dbhi.db);
                if(firstpass)
                {
                    eventlist.push_back(HypocenterCSS30(dbhi.db,currenthypo));
                    firstpass=false;
                }
                else
		{
                    if((space_time_difference(currenthypo,previoushypo)
                            > tolerance) || (nrecin==(nrows-1)))
                    {
                        list<HypocenterCSS30>::iterator ihptr,keeper;
                        keeper=eventlist.begin();
                        if(eventlist.size()>1)
                        {
                            /*Odd construct to force iterator to begin+1*/
                            ihptr=eventlist.begin();
                            ++ihptr;
                            for(;ihptr!=eventlist.end();++ihptr)
                            {
                                if(ihptr->ndef>keeper->ndef)
                                    keeper=ihptr;
                            }
                        }
                        dbaddv(dbhoe.db,0,"evid",keeper->evid,"prefor",keeper->orid,NULL);
                        try{
			    irec=keeper->dbsave(dbho.db);
                        }catch (SeisppError& serr)
                        {
                            serr.log_error();
                            cerr << "Error while processing output record "<<nrecout<<endl;
                        }
                        previoushypo=currenthypo;
                        eventlist.clear();
                        eventlist.push_back(HypocenterCSS30(dbhi.db,currenthypo));
			++nrecout;
                    }
                    else
                    {
                        /* Fall here when we have a duplicate.  Pushed to 
                           list */
                        eventlist.push_back(HypocenterCSS30(dbhi.db,
                                    currenthypo));
                    }
		}
	}
	cout << "Input db had "<<nrecin<<" rows"<<endl
		<<"Wrote "<<nrecout<<" origin and event rows to output"<<endl;
}
