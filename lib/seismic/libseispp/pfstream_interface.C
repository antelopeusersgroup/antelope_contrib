#include <stdlib.h>
#include <string>
#include "stock.h"
#include "pf.h"
#include "pfstream.h"
#include "metadata.h"
#include "seispp.h"

using namespace std;

/* Gets a Time_Series_Ensemble object from a pfstream pfh.  The
 * requested ensemble is assumed to be keyed by tag in the 
 * input pfstream.  mdlist is a list of constant metadata to 
 * copy one (the first actually) object to the ensemble metadata
 * space.  
 *
 * Normal return is a valid pointer.  A NULL pointer is
 * returned on end of input.
 *
 * This could be a constructor for a Time_Series_Ensemble but
 * my view was it was too ugly to be made an intrinsic part 
 * of the data object.
 */
Time_Series_Ensemble *get_next_ensemble(Pfstream_handle *pfh, 
		char *tag,
		list<Metadata_typedef>& mdlist)
		throw(seispp_error)
{
	Pf *pfin;
	Pf_ensemble *pfe;
	vector<Time_Series>::iterator t0;  // used to select trace 0
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
		throw(seispp_error("get_next_ensemble: Failure parsing input pfstream"));
	}
	Time_Series_Ensemble *tsptr=new Time_Series_Ensemble();
	Time_Series_Ensemble& tseobj = *tsptr;
	try{
	    for(i=0;i<pfe->nmembers;++i)
	    {
		Time_Series *ts=new Time_Series(pfe->pf[i]);
		tseobj.tse.push_back(*ts);
		delete ts;
	    }
	    // copy the desired global metadata to the ensemble
	    // metadata object
	    //
	    t0=tseobj.tse.begin();
	    Time_Series& t0r = *t0;
	    copy_selected_metadata(t0r.md,tseobj.md,mdlist);
	}
	catch(seispp_error serr)
	{
		throw serr;
	}
	catch(Metadata_error mderr)
	{
		mderr.log_error();
		throw seispp_error("get_next_ensemble: Problems building Time_Series_Ensemble object");
	}
	return(tsptr);
}

/*  This is a similar function for a three component ensemble.
 *  Only a single list of metadata is passed and that list is
 *  used to control copies to both the ensemble and individual
 *  3c groups.  i.e. md of the ensemble will be the same
 *  as md in each of the Three_Component_Seismogram objects.
 *  Overloading can be used to add a more general version of
 *  this with two different lists if wanted later, but for
 *  now I see now reason for the mess this causes.
 *
 *  Author:  Gary Pavlis
 *  Written:  May 2003
 */

Three_Component_Ensemble *get_next_3c_ensemble(Pfstream_handle *pfh, 
		char *tag,
		list<Metadata_typedef>& mdlist)
		throw(seispp_error)
{
	Pf *pfin;
	Pf_ensemble *pfe;
	int i,j;
	vector <Three_Component_Seismogram>::iterator t0;

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
		throw(seispp_error("get_next_3c_ensemble:  Failure parsing input pfstream"));
	}
	if(pfe->ngroups<=0 || pfe->group_keys==NULL) 
		throw(seispp_error("get_next_3c_ensemble:  Three-component traces required grouping of pfstream to define componets"));

	Three_Component_Ensemble *tceptr=new Three_Component_Ensemble();
	Three_Component_Ensemble& tceobj = *tceptr;
	try{
	// this is a bit prone to seg faults, but excessive error
	// checking can also cause other problems
	    for(i=0;i<pfe->ngroups;++i)
	    {
		    int is=pfe->group_start[i];
		    int ie=pfe->group_end[i];
		    Three_Component_Seismogram seis;
		    if(ie-is+1<3)
		    {
			    cerr << "get_next_3c_ensemble: incomplete three-component group in input stream.  Found only "
				    << ie-is+1 << "entries in group"<<endl;
			    cerr << "Data skipped"<<endl;
			    continue;
		    }
		    else if(ie-is+1>3)
		    {
			    cerr << "get_next_3c_ensemble (warning):  extra data in three-component grouping"<<endl;
			    cerr << "Found " << ie-is+1 << "entries while looking for exactly three\nUsing first three in group"<<endl;
		    }
		    for(int jj=0,j=pfe->group_start[i];
				    j<=pfe->group_end[i] && jj<3;++j,++jj)
		    {
			    seis.x[jj]=Time_Series(pfe->pf[j]);
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
		    	tceobj.tcse.push_back(seis);
		    }
		    catch (Metadata_error mde)
		    {
			    mde.log_error();
			    throw seispp_error("Error copying metadata\nEnsemble metadata may be incomplete\n");
		    }
	    }
	    // copy the global metadata from the first 3c entry
	    // using same list as for individual components
	    //
	    t0 = tceobj.tcse.begin();
	    Three_Component_Seismogram& t0r = *t0;
	    copy_selected_metadata(t0r.md,tceobj.md,mdlist);
	}
	catch(seispp_error serr)
	{
		throw serr;
	}
	catch(Metadata_error mderr)
	{
		mderr.log_error();
		throw seispp_error("Problems building Time_Series_Ensemble object");
	}
	return(tceptr);
}
void pfstream_save_3cseis(Three_Component_Seismogram *sptr,
	string tag,
	string dir,
	string dfile,
	Pfstream_handle *pfh) throw(seispp_error)
{
	Pf_ensemble *pfe;
	Three_Component_Seismogram& seis=*sptr;

	pfe = create_Pf_ensemble(3,0); //this function always returns or dies: no error trap
	try {
		for(int i=0;i<3;++i)
		{
			long int foff;
			pfe->pf[i]=seis.x[i].md.extract_all_metadata_to_pf();
			foff = vector_fwrite(seis.x[i].s,seis.x[i].ns,dir,dfile);
		}
	}
	catch (seispp_error err)
	{
		err.log_error();
		throw seispp_error("Data not saved.  Watch for partially written files");
	}
	Pf *pf=build_ensemble(1,tag.c_str());
	free_Pf_ensemble(pfe);
	// This is not a memory leak because the pf is cleared after it is popped from
	// the fifo.
	pfstream_put_ensemble(pfh,pf);
}
