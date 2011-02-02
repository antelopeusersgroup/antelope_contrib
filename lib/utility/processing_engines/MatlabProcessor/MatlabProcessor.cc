#include "MatlabProcessor.h"

namespace SEISPP {
using namespace std;
using namespace SEISPP;

MatlabProcessor::MatlabProcessor()
{
	ios::sync_with_stdio();
	string base_error("MatlabProcessor default constructor: ");
	// This starts matlab on the current host
	ep=engOpen("\0");
	if(ep==NULL)
		throw SeisppError(base_error
			+string("Could not start MATLAB engine on this host"));
	verbose=false;
	fplog=NULL;
}
MatlabProcessor::MatlabProcessor(FILE *fpout)
{
	ios::sync_with_stdio();
	string base_error("MatlabProcessor logging constructor: ");
	// This starts matlab on the current host
	ep=engOpen("\0");
	if(ep==NULL)
		throw SeisppError(base_error
			+string("Could not start MATLAB engine on this host"));
	if(fpout==NULL)
		throw SeisppError(base_error
			+string("FILE pointer for output log is NULL"));
	verbose=true;
	fplog=fpout;
	engOutputBuffer(ep,logbuffer,LogBufferSize);
}
MatlabProcessor::MatlabProcessor(string hostname)
{
	ios::sync_with_stdio();
	string startcmd("ssh ");;
	// This generates a start string using ssh which is simpler than
	// the rsh example in matlab documentation due to the fact ssh handles
	// DISPLAY automatically.  Result is of the form:  ssh hostname matlab
	startcmd=startcmd + hostname + string(" matlab");
	ep=engOpen(startcmd.c_str());
	if(ep==NULL)
		throw SeisppError(string("MatlabProcessor remote host constructor: ")
			+ string("engOpen failed for hostname=")+hostname);
	verbose=false;
	fplog=NULL;
}
MatlabProcessor::~MatlabProcessor()
{
	if(engClose(ep))
		throw SeisppError("MatlabProcessor::destructor engClose returned an error code");
}
// Internal helper to log errors
void log_error(char *message,FILE *fpo)
{
	if(strlen(message)<2) 
		return;
	else
		fprintf(fpo,"%s\n",message);
}
void MatlabProcessor::load(double *x,int ns, string name)
{
	const string base_error("MatlabProcessor::load(vector version):  ");
	int ierr;
	// This is modeled after engdemo.c in matlab's documentation
	mxArray *vec=NULL;
	vec=mxCreateDoubleMatrix(1,ns,mxREAL);
	if(vec==NULL)
		throw SeisppError(base_error
			+ string("mxCreateDoubleMatrix failed"));
#ifdef OLDMATLAB
	mxSetName(vec,name.c_str());
#endif
	double *vptr=mxGetPr(vec);
	for(int i=0;i<ns;++i)vptr[i]=x[i];
#ifdef OLDMATLAB
	ierr = engPutArray(ep,vec);
#else
	ierr = engPutVariable(ep,name.c_str(),vec);
#endif
	mxDestroyArray(vec);
	if(ierr) throw SeisppError(base_error
		+ string("mxPutArray for vector")
		+name
		+ string(" failed."));
}
void MatlabProcessor::load(TimeSeries& d,string name)
{
	this->load(&(d.s[0]),d.ns,name);
}
void MatlabProcessor::load(dmatrix& d, string name)
{
	const string base_error("MatlabProcessor::load(matrix version):  ");
	int ierr;
	// This is modeled after engdemo.c in matlab's documentation
	mxArray *mat=NULL;
	mat=mxCreateDoubleMatrix(d.rows(),d.columns(),mxREAL);
	if(mat==NULL)
		throw SeisppError(base_error
			+ string("mxCreateDoubleMatrix failed"));
#ifdef OLDMATLAB
	mxSetName(mat,name.c_str());
#endif
	double *mptr=mxGetPr(mat);
	int i,j,iv;
	for(j=0,iv=0;j<d.columns();++j)
		for(i=0;i<d.rows();++i,++iv)
			mptr[iv]=d(i,j);
#ifdef OLDMATLAB
	ierr = engPutArray(ep,mat);
#else
	ierr = engPutVariable(ep,name.c_str(),mat);
#endif
	mxDestroyArray(mat);
	if(ierr) throw SeisppError(base_error
		+ string("mxPutArray for matrix")
		+name
		+ string(" failed."));
}
/* This will load 3c seismograms in a 3xns matrix.  
*/
void MatlabProcessor::load(ThreeComponentSeismogram& d,string name)
{
	this->load(d.u,name);
}
const int SIZELIMIT(1000000);
void MatlabProcessor::load(TimeSeriesEnsemble& d,string name)
{
	int i,j,id;
	const string  base_error("MatlabProcessor::load(TimeSeriesEnsemble): ");
	try {
		double xmin,xmax;
		int nmember=d.member.size();
		double dttest;
		double t;
		bool need_initializing=true;
    		for(i=0;i<nmember;++i) 
		{
		    if(d.member[i].live)
		    {
			// Set this field to allow correct cross
			// reference when a result is returned.
			d.member[i].put(EnsembleIndexKeyword,i);
			if(need_initializing)
			{
   			    xmin=d.member[i].t0;
    			    xmax=d.member[i].endtime();
			    dttest=d.member[i].dt;
			}
			else
			{
        		    xmin=MIN(d.member[i].t0,xmin);
        		    xmax=MAX(d.member[i].endtime(),xmax);
			    if(d.member[i].dt != dttest)
			    {
				throw SeisppError(string(
				   "MatlabProcessor::load TimeSeries Ensemble must")
				+ string(" have constant sample rate"));
			    }
			}
		    }
    		}
		int n1;
		n1=static_cast<int>((xmax-xmin)/dttest);
		if( (n1<=0) || (nmember<=0) )
			throw SeisppError(base_error+string("input ensemble is empty"));
		// sanity check on nl
		if(n1>SIZELIMIT)
			throw SeisppError(base_error
			+string("number of samples absurdly large.  Check input time range."));
		dmatrix z(n1,nmember);
		z.zero();

		for(j=0;j<nmember;++j)
		{
		    if(d.member[j].live)
		    {
			for(i=0,t=xmin;i<n1;++i,t+=dttest)
			{
			    if(!d.member[j].is_gap(t))
			    {
				id=d.member[j].sample_number(t);
				if( (id>=0) && (id<d.member[j].ns) )
					z(i,j)=d.member[j].s[id];
			    }
			}
		    }
		}
		this->load(z,name);
	}catch (...) {throw;};
}
void MatlabProcessor::load(ThreeComponentEnsemble& d,string name[3])
{
	try {
		auto_ptr<TimeSeriesEnsemble> compens;
		for(int i=0;i<3;++i)
		{
			d.member[i].put(EnsembleIndexKeyword,i);
			compens=ExtractComponent(d,i);
			this->load(*compens,name[i]);
		}
	} catch (...) {throw;};
}
void MatlabProcessor::process(string text)
{
	// Silently do nothing if input is empty
	if(text.size()<=0) return;
	istringstream instrm(text);
	// Now work through text line by line sending each to matlab
	// one line at a time
	char command[256];
	int ierr;
	while(instrm.good())
	{
		instrm.getline(command,256);
		ierr=engEvalString(ep,command);
		if(verbose) 
			log_error(logbuffer,fplog);
		if(ierr) throw SeisppError(string("Matlabprocessor::process:  ")
				+ string("engEvalString returned an error on the following command.\n")
				+ command);
	}
}
	
void MatlabProcessor::process(char *s)
{
	this->process(string(s));
}
void MatlabProcessor::process(ifstream& instrm)
{
	char line[255];
	while(instrm.good())
	{
		instrm.getline(line,255);
		this->process(string(line));
	}	
} 
vector<double> MatlabProcessor::retrieve_vector(string name)
{
	const string base_error("MatlabProcessor::retrieve_vector: ");
	int nrow,ncol;
	mxArray *VectorReturned;
#ifdef OLDMATLAB
	VectorReturned = engGetArray(ep,name.c_str());
#else
	VectorReturned = engGetVariable(ep,name.c_str());
#endif
	if(VectorReturned==NULL)
		throw SeisppError(base_error
		+ string("engGetArray procedure throw an error while retrieving array tagged as")
		+ name);
	nrow=mxGetM(VectorReturned);
	ncol=mxGetN(VectorReturned);
	int nsize=nrow*ncol;
	if( !( (nrow==nsize) || (ncol==nsize) ))
	{
		mxDestroyArray(VectorReturned);
		throw SeisppError(base_error
			+ name +string(" is not a vector"));
	}
	// Need to copy result to avoid external need for matlab structures
	vector<double> result;
	result.reserve(nsize);
	double *vptr=mxGetPr(VectorReturned);
	for(int i=0;i<nsize;++i,++vptr) result.push_back(*vptr);
	mxDestroyArray(VectorReturned);
	return(result);
}
	
auto_ptr<dmatrix> MatlabProcessor::retrieve_matrix(string name)
{
	string base_error("MatlabProcessor::retrieve_matrix:  ");
	int nrow,ncol;
	mxArray *mp;
#ifdef OLDMATLAB
	mp=engGetArray(ep,name.c_str());
#else
	mp=engGetVariable(ep,name.c_str());
#endif
	if(mp==NULL)
		throw SeisppError(base_error
		+ string("engGetArray procedure throw an error while retrieving array tagged as")
		+ name);

	nrow=mxGetM(mp);
	ncol=mxGetN(mp);
	auto_ptr<dmatrix> result(new dmatrix(nrow,ncol));
	double *mpptr=mxGetPr(mp);
	dcopy(nrow*ncol,mpptr,1,result->get_address(0,0),1);
	mxDestroyArray(mp);
	return(result);	
}
void MatlabProcessor::run_interactive()
{
	if(fplog==NULL)
	{
		fplog=stdout;
		verbose=true;
		engOutputBuffer(ep,logbuffer,LogBufferSize);
	}
	const string endkeyword("exit_interactive");
	const string prompt("MatlabProcessor>");
	cout << "Entering matlab interactive loop." << endl
		<< "Type " << endkeyword << " or ^D to exit this command loop."
		<< endl;
	char inbuf[256];

	while(cin.good())
	{
		cout << prompt;
		cin.getline(inbuf,256);
		// Always return when user enters this magic string
		if(string(inbuf)==endkeyword) break;
		this->process(inbuf);
	}
}
} // End SEISPP namespace declaration
