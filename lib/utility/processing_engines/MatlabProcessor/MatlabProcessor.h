#ifndef _MATLAB_PROCESSOR_H_
#define _MATLAB_PROCESSOR_H_

#include <stdio.h>
#include <fstream>
#include <sstream>

// SEISPP includes
#include "ExternalProcessor.h"
#include "dmatrix.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
// Matlab includes
#include "engine.h"
#include "matrix.h"

namespace SEISPP {
using namespace std;
using namespace SEISPP;

/*! Output buffer size for output from matlab command interpreter.  If more
than this number of bytes are blasted from a command the output will be 
silently truncated. */
const int LogBufferSize=1024;
/*! \brief A processing object that can be used to process data through Matlab.

Matlab has become the FORTRAN of seismology and this processing object provides
a simple way to integrate matlab m file scripts into a processing program using
the SEISPP library.  It implements the abstract base class ExternalProcessor.
The way this processing object would normally be used is to implement
the following four steps:
(1) create the object, (2) load a data object into matlab through one of the
overloaded load methods, (3) run a processing script with one of the overloaded
process methods, and (optionally) (4) retrieve the result with on of the 
retrieve methods.  

This object uses the creations is initialization mode of OOP.  That is,
the model is that when the object is created the handle to matlab is
created.  That handle is assumed to remain active until it is released
by the explicit or implict call to the destructor of the object.
This is VERY IMPORTANT to recognize for this object because a processing
script can very easily tell matlab to exit.  The caller should not do 
this or an error will be generated when the destructor is called when
the object is destroyed.

\author Gary L. Pavlis 
*/
class MatlabProcessor : public ExternalProcessor
{
public:
	/*! Exec matlab using the default command. 
	\exception SeisppError is thrown if creation fails.
	*/
	MatlabProcessor();
	/*! Exec matlab on local host with output echoed to a file. 
	\par
	Sometimes we want to run matlab in a verbose mode that saves
	output or echoes output to stdout.  Use this constructor if 
	you want the output captured and sent to a standard C file 
	handle.  I chose a raw FILE pointer instead of an ostream
	because the matlab interface just returns data into a char *
	buffer and this is easily handled through C stdio.

	\param fp is a C FILE pointer to receive output from matlab.  
		Note use stdout to echo result to output.  Alternatively
		use a fifo if you want to connect to some display gizmo.
	\exception SeisppError is thrown if creation fails.
	*/
	MatlabProcessor(FILE *fp);
	/*! Exec matlab on remote host. 
	\exception SeisppError is thrown if creation fails.
	*/
	MatlabProcessor(string hostname);
	/*! Standard destructor.   
	
	Nontrivial as it has to control termination of matlab.
	\exception SeisppError is thrown if destruction failed.  The most
		likely cause of this condition is matlab exiting 
		before the destructor is called.
	*/
	~MatlabProcessor();
	/*! \brief Load a TimeSeries object.
	
	\param d TimeSeries data to be loaded for processing.
	\param name Symbolic name to be attached to these data. 
		Matlab uses symbols to tag all data objects.  This is the
		symbol that can be used in processing scripts to handle
		these data.
	\exception SeisppError is thrown if there are problems.
	*/  
	void load(TimeSeries& d,string name);
	/*! \brief Load a simple vector of data.
	
	\param x is data to be loaded for processing.
	\param ns number of samples in vector x.
	\param name Symbolic name to be attached to these data. 
		Matlab uses symbols to tag all data objects.  This is the
		symbol that can be used in processing scripts to handle
		these data.
	\exception SeisppError is thrown if there are problems.
	*/  
	void load(double *x,int ns, string name);
	/*! \brief Load a ThreeComponentSeismogram object.
	\par
	Three component seismograms are a unique seismological concept.
	Different systems handle them very differently, but matlab 
	is matrix oriented so here the seismogram is loaded as a 
	3xnumber_samples matrix.  Note that in SEISPP such a matrix
	is a public data attribute that is transferred directly to 
	matlab through the dmatrix version of this overloaded method.
	
	\param d is data to be loaded for processing.
	\param name Symbolic name to be attached to these data. 
		Matlab uses symbols to tag all data objects.  This is the
		symbol that can be used in processing scripts to handle
		these data.
	\exception SeisppError is thrown if there are problems.
	*/  
	void load(ThreeComponentSeismogram& d,string name);
	/*! \brief Load a matrix into matlab for processing.
	\par
	The core data type in matlab is the matrix.  This method loads a
	dmatrix object as a specified symbol name.
	
	\param d is data to be loaded for processing.
	\param name Symbolic name to be attached to these data. 
		Matlab uses symbols to tag all data objects.  This is the
		symbol that can be used in processing scripts to handle
		these data.
	\exception SeisppError is thrown if there are problems.
	*/  
	void load(dmatrix& d, string name);
	/*! Load a time series ensemble as a matrix into matlab.
	
	Matlab treats all data as a matrix while the concepts in a 
	TimeSeries and TimeSeries ensemble are more general.  All the
	Metadata (header) components are lost in the translation. 
	To handle this cross-reference problem this method loads 
	an index position for each member of the ensemble as a new
	Metadata attribute with a key string defined below.  
	The SyncEnsemble template procedure can be used to copy
	contents from the parent to the processed result even if the
	order of the parent gets changed. 
	\par
	Note also that the input ensemble is not assumed to be time
	aligned.  Data are time shifted to the nearest sample and 
	zero padded if start times are irregular.  Be very conscious
	of this if this method is used on data in an absolute time
	reference frame as it is very easy to accidentally ask for
	an enormous matrix.  There is an internal sanity check to 
	keep this from getting out of hand, but the user is still
	cautioned to be careful on this point.
	
	\param tse is the input data ensemble
	\param name is the symbolic name that is to be assigned to
		the matrix in matlab.
	\exception SeisppError is thrown if there are problems.
	*/
	void load(TimeSeriesEnsemble& tse,string name);
	/*! \brief Load an ensemble of ThreeComponentSeismograms.
	\par
	An ensemble of three component seismograms could be respresented
	in one of two ways in matlab.  This data object could be 
	represented as a 3d array or (as here) a set of three different
	matrices with three different names.  I chose to use the later
	as a simple matrix is the core concept of matlab and it is 
	relatively easy to convert to the 3d version of one needs to
	do so.
	\par
	Be aware the three matrices that are loaded are produced by 
	three calls to the TimeSeriesEnsemble method.  As a result the
	same warnings about irregular start times apply here as well.
	\par
	Note also that the input ensemble metadata is altered with the
	index position of each member posted.  This allows application
	of the SyncEnsemble template to a retrieved result.

	\param tce is data to be loaded for processing.
	\param name Symbolic name to be attached to these data. 
		Matlab uses symbols to tag all data objects.  This is the
		symbol that can be used in processing scripts to handle
		these data.
	\exception SeisppError is thrown if there are problems.
	*/  
	void load(ThreeComponentEnsemble& tce,string name[3]);
	/*! \brief Run a set of matlab instructions.
	\par
	This method is used to actually run a series of matlab commands.
	The input string is parsed into lines and set to matlab one
	line at a time.  
	\exception SeisppError object is thrown with an error message if
		a process line sent to matlab generates an error.  User
		should catch this exception and call the log_error() 
		method for the return to handle such errors.
	\param text is the string containing one more more command
		lines.  
	*/
	void process(string text);
	/*! \brief Run a set of matlab instructions.
	\par
	This method is used to actually run a series of matlab commands.
	The input char * variable is converted to a string and the 
	string version of this method is called.  i.e. this overloaded
	function is just a convenient interface routine to allow
	passing a raw char *.
	\exception SeisppError object is thrown with an error message if
		a process line sent to matlab generates an error.  User
		should catch this exception and call the log_error() 
		method for the return to handle such errors.
	\param s is the string containing one more more command
		lines.  
	*/
	void process(char *s);
	/*! \brief Pass data from a stream into matlab for processing.
	\par
	This process method takes input from a file and passes it into
	matlab one line at a time.  The input can be a plain m file
	(i.e. not tagged as a function) or a connection to an interactive
	input.  The abstraction using the stream object makes no distinction.
	The input is processed until an eof is encountered.  

	\exception SeisppError object is thrown with an error message if
		a process line sent to matlab generates an error.  User
		should catch this exception and call the log_error() 
		method for the return to handle such errors.
	\param strm is an (already open) stream variable to use for
		fetch input for matlab.
	*/
	void process(ifstream& strm); 
	/*! \brief Retrieves a vector from matlab.
	\par
	This method is the inverse of the load method for an input
	vector.  It retrieves a vector tagged with a particular
	symbolic name as an STL vector container.  
	\param name symbolic name attached to desire matlab vector.
	\return STL vector of doubles containing desired result.
	\exception SeisppError is thrown if the attempt to retrieve
		the desired matrix failed.
	*/
	vector<double> retrieve_vector(string name);
	/*! \brief Retrieves a matrix from matlab.
	\par
	This method is the inverse of the load method for an input
	matrix.  It retrieves a matrix tagged with a particular
	symbolic name as a dmatrix.  Note the actual return is
	a memory managed auto pointer to reduce the overhead of
	returning a potentially large matrix.

	\param name symbolic name attached to desire matlab vector.
	\return shared_ptr to a dmatrix of the desired result.
	\exception SeisppError is thrown if the attempt to retrieve
		the desired matrix failed.
	*/
	shared_ptr<dmatrix> retrieve_matrix(string name);
	/*! \brief Run an matlab session through this processor interactively.
	\par
	Sometimes one wants to talk directly to Matlab to debug a problem or
	just as a quick check.  Running this method connects stdin and stdout
	from the parent session much like runing matlab as "matlab -nojvm" 
	except the prompt is different.  The interactive session can be 
	exited with the standard EOF sentinel (Ctrl-D) or a special keyword
	announced when this method is started (current exit_interactive, but this
	could change and not be recorded here.).  Be aware that using EOF has
	the side issue that stdin may be closed and no more input from the 
	keyboard may be allowed until the calling program exits.  
	*/
	void run_interactive();
private:
	Engine *ep;
	bool verbose;  // if true will write stuff to FILE handle fplog
	FILE *fplog;

	char logbuffer[LogBufferSize];
};
/*! Used to tag integer index for cross referencing a returned matrix
to a parent ensemble. */
const string EnsembleIndexKeyword("MatlabEnsembleIndex");
/*! This procedure will synchronize Metadata from an ensemble returned by
one of ensemble retrieve methods with a parent. */
template <class Tensemble> void CloneMetadata(Tensemble& parent, shared_ptr<Tensemble *>(child))
{
	cerr << "CloneMetadata procedure not yet implemented\n"<<endl;
}
/*! \brief Loads metadata attributes from an ensemble as a matlab vector.
\par
For ensembles of data it is often useful to examine various attributes
attached to the data.  For example, in reflection processing it is 
common to make plots of CDP fold versus CDP number or some other position
variable.  This is a generic function for doing this process and loading
the result into matlab as a vector.  Note that any ensemble members 
marked dead are simply skipped.  This is a potential indexing problem 
if multiple elements of any ensemble are extracted and compared in 
matlab.  i.e. chaos can result if the input ensemble is edited or
sorted between any loading of data to matlab.

\param ens input ensemble.
\param metadata_name is the internal name of the attribute to be sent
	to matlab for processing.
\param matlab_name is the symbolic name to be assigned to this attribute
	vector in matlab.
\param am AttributeMap object of namespace used for attributes in this
	program.  The Metadata object used in SEISPP to define data
	attributes uses an AttributeMap to define it's internal namespace.
	It is needed here mainly to allow type to be determined without
	requiring another template parameter.
\param matp the MatlabProcessor object to which data are to be loaded.

\exception SeisppError is thrown if the requested attribute's name is
	not found in the AttributeMap namespace.  Note if the name is
	known but the attribute requested is not found in the ensmble
	member's data a 0 will be loaded for any member with that
	condition and a message stating this will be posted to stderr.
*/
template <class Tensemble> void LoadMetadata(Tensemble& ens,
	string metadata_name,
		string matlab_name, 
			AttributeMap& am,
				MatlabProcessor& matp)
{
        // Make sure the attribute name is valid and get it's type
        MDtype mdtype;
        const double null_data_flag(0.0);
        map<string,AttributeProperties>::iterator ap;
        ap=am.attributes.find(metadata_name);
        if(ap==am.attributes.end())
                throw SeisppError(string("LoadMetadata:  ")
                        + string("Attribute name=")+metadata_name
                        + string(" is not defined in AttributeMap") );
        mdtype=ap->second.mdt;
	vector<double> mdvector;
	for(int i=0;i<ens.member.size();++i)
	{
		int ival;
		double dval;
		if(ens.member[i].live)
		{
			try {
				switch (mdtype)
				{
				case MDint:
					ival=ens.member[i].get_int(metadata_name);
					mdvector.push_back(static_cast<double>(ival));
					break;
				case MDreal:
					dval=ens.member[i].get_double(metadata_name);
					mdvector.push_back(dval);
					break;
				default:
					throw SeisppError(string("LoadMetadata:  ")
						+string("illegal metadata type.  ")
						+string("Only allow int and real") );
				}
			} catch (MetadataGetError mderr)
			{
				cerr << "LoadMetadata: error retrieving metadata"<<endl;
				mderr.log_error();
				cerr << "Sending a 0 value to matlab"<<endl;
				mdvector.push_back(0.0);
			}
		}
		else
			mdvector.push_back(0.0);
	}
	matp.load(&(mdvector[0]),mdvector.size(),matlab_name);
}


} // End SEISPP namespace declaration
#endif
