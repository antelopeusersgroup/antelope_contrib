#include <stdlib.h>
#include <string>
#include "stock.h"
#include "pf.h"
#include "pfstream.h"
#include "Metadata.h"
#include "SeisppError.h"
#include "ensemble.h"
#include "seispp.h"

namespace SEISPP
{
using namespace std;
using namespace SEISPP;

/* Gets a TimeSeriesEnsemble object from a pfstream pfh.  The
 * requested ensemble is assumed to be keyed by tag in the 
 * input pfstream.  mdlist is a list of constant metadata to 
 * copy one (the first actually) object to the ensemble metadata
 * space.  
 *
 * Normal return is a valid pointer.  A NULL pointer is
 * returned on end of input.
 *
 * This could be a constructor for a TimeSeriesEnsemble but
 * my view was it was too ugly to be made an intrinsic part 
 * of the data object.
 */
TimeSeriesEnsemble *GetNextEnsemble(Pfstream_handle *pfh, 
		char *tag,
		MetadataList& mdlist)
		throw(SeisppError)
{
	Pf *pfin;
	Pf_ensemble *pfe;
	vector<TimeSeries>::iterator t0;  // used to select trace 0
	int i;

	//  This routine gets the data required to construct this
	//  from the input pfstream.  I returns everything 
	//  encapsulated in a single pf
	pfin = pfstream_get_next_ensemble(pfh);
	if(pfin==NULL) return(NULL);

	// We next parse the input pf encapsulated in a single
	// set of curly brackets into an ensemble
	pfe = pfget_Pf_ensemble(pfin,tag);
	if(pfe==NULL)
	{
		pffree(pfin);
		throw(SeisppError("GetNextEnsemble: Failure parsing input pfstream"));
	}
	TimeSeriesEnsemble *tsptr=new TimeSeriesEnsemble();
	TimeSeriesEnsemble& tseobj = *tsptr;
	try{
	    for(i=0;i<pfe->nmembers;++i)
	    {
		TimeSeries *ts=LoadTimeSeriesUsingPf(pfe->pf[i]);
		tseobj.member.push_back(*ts);
		delete ts;
	    }
	    // copy the desired global metadata to the ensemble
	    // metadata object
	    //
	    t0=tseobj.member.begin();
	    TimeSeries& t0r = *t0;
	    copy_selected_metadata(dynamic_cast<Metadata&>(t0r),dynamic_cast<Metadata&>(tseobj),mdlist);
	}
	catch(SeisppError serr)
	{
		throw serr;
	}
	catch(MetadataError& mderr)
	{
		mderr.log_error();
		throw SeisppError("GetNextEnsemble: Problems building TimeSeriesEnsemble object");
	}
	return(tsptr);
}

/*  This is a similar function for a three component ensemble.
 *  Only a single list of metadata is passed and that list is
 *  used to control copies to both the ensemble and individual
 *  3c groups.  i.e. md of the ensemble will be the same
 *  as md in each of the ThreeComponentSeismogram objects.
 *  Overloading can be used to add a more general version of
 *  this with two different lists if wanted later, but for
 *  now I see now reason for the mess this causes.
 *
 *  Author:  Gary Pavlis
 *  Written:  May 2003
 */

////////////////////////////////////////////////////////////////////////
/* Taken out of commission for the time being.  Higher level routine that will
take some work to get right.  There are other design changes likely so I won't
get it working right now.



ThreeComponentEnsemble *GetNext3cEnsemble(Pfstream_handle *pfh, 
		char *tag,
		MetadataList& mdlist)
		throw(SeisppError)
{
	Pf *pfin;
	Pf_ensemble *pfe;
	int i,j;
	vector <ThreeComponentSeismogram>::iterator t0;

	//  This routine gets the data required to construct this
	//  from the input pfstream.  I returns everything 
	//  encapsulated in a single pf
	pfin = pfstream_GetNextEnsemble(pfh);
	if(pfin==NULL) return(NULL);

	// We next parse the input pf encapsulated in a single
	// set of curly brackets into an ensemble
	pfe = pfget_Pf_ensemble(pfin,tag);
	if(pfe==NULL)
	{
		pffree(pfin);
		throw(SeisppError("GetNext3cEnsemble:  Failure parsing input pfstream"));
	}
	if(pfe->ngroups<=0 || pfe->group_keys==NULL) 
		throw(SeisppError("GetNext3cEnsemble:  Three-component traces required grouping of pfstream to define componets"));

	ThreeComponentEnsemble *tceptr=new ThreeComponentEnsemble();
	ThreeComponentEnsemble& tceobj = *tceptr;
	try{
	// this is a bit prone to seg faults, but excessive error
	// checking can also cause other problems
	    for(i=0;i<pfe->ngroups;++i)
	    {
		    int is=pfe->group_start[i];
		    int ie=pfe->group_end[i];
		    ThreeComponentSeismogram seis;
		    if(ie-is+1<3)
		    {
			    cerr << "GetNext3cEnsemble: incomplete three-component group in input stream.  Found only "
				    << ie-is+1 << "entries in group"<<endl;
			    cerr << "Data skipped"<<endl;
			    continue;
		    }
		    else if(ie-is+1>3)
		    {
			    cerr << "GetNext3cEnsemble (warning):  extra data in three-component grouping"<<endl;
			    cerr << "Found " << ie-is+1 << "entries while looking for exactly three\nUsing first three in group"<<endl;
		    }
		    for(int jj=0,j=pfe->group_start[i];
				    j<=pfe->group_end[i] && jj<3;++j,++jj)
		    {
			    seis.x[jj]=TimeSeries(pfe->pf[j]);
		    }

		    try {
		    	copy_selected_metadata(seis.x[0].md,
				    seis.md,mdlist);
			// assume all pieces of the ensemble have these
			// defined so we can extract the global for the
			// ensemble from the irst one in the list
			seis.tref=seis.x[0].tref;
			seis.components_are_cardinal
				=seis.x[0].md.get_bool("components_are_orthogonal");
			seis.components_are_cardinal
				=seis.x[0].md.get_bool("components_are_cardinal");
		    	tceobj.member.push_back(seis);
		    }
		    catch (MetadataError& mde)
		    {
			    mde.log_error();
			    throw SeisppError("Error copying metadata\nEnsemble metadata may be incomplete\n");
		    }
	    }
	    // copy the global metadata from the first 3c entry
	    // using same list as for individual components
	    //
	    t0 = tceobj.member.begin();
	    ThreeComponentSeismogram& t0r = *t0;
	    copy_selected_metadata(t0r.md,tceobj.md,mdlist);
	}
	catch(SeisppError serr)
	{
		throw serr;
	}
	catch(MetadataError& mderr)
	{
		mderr.log_error();
		throw SeisppError("Problems building TimeSeriesEnsemble object");
	}
	return(tceptr);
}
void PfstreamSave3cseis(ThreeComponentSeismogram *sptr,
	string tag,
	string dir,
	string dfile,
	Pfstream_handle *pfh) throw(SeisppError)
{
	Pf_ensemble *pfe;
	ThreeComponentSeismogram& seis=*sptr;

	pfe = create_Pf_ensemble(3,0); //this function always returns or dies: no error trap
	try {
		for(int i=0;i<3;++i)
		{
			long int foff;
			pfe->pf[i]=seis.x[i].md.extract_all_metadata_to_pf();
			foff = vector_fwrite(&(seis.x[i].s[0]),seis.x[i].ns,dir,dfile);
		}
	}
	catch (SeisppError err)
	{
		err.log_error();
		throw SeisppError("Data not saved.  Watch for partially written files");
	}
	Pf *pf=build_ensemble(1,tag.c_str());
	free_Pf_ensemble(pfe);
	// This is not a memory leak because the pf is cleared after it is popped from
	// the fifo.
	pfstream_put_ensemble(pfh,pf);
}

End of section needing work later. */
////////////////////////////////////////////////////////////////////////
} // Termination of namespace SEISPP definitions
