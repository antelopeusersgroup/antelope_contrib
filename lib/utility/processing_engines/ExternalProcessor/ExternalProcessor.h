#ifndef _EXTERNALPROCESSOR_H_
#define _EXTERNALPROCESSOR_H_

#include <vector>
#include <string>
#include <fstream>
#include "dmatrix.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
#include "ensemble.h"
namespace SEISPP {
using namespace SEISPP;
/*! Abstract base class for a generic external processor engine. 
\par
There are many applications that are packaged in a way that is more
convenient to access them from an application through a well defined
interface.  An example is matlab, which is the first processor I've
implemented through this interface definition.  This is a base class
that describes a basic interface that should be implementable on a 
three common applications I've aimed this toward:  (1) matlab (the
most general application); (2) the Seismic Analysis Code (SAC); and
(3) various seismic reflection processing packages (e.g. seismic unix
or Igor Mirozov's SIA).  I've attempted to encapsulate all common 
concepts of these applications in this base class.  The presumption
is each can and should involk extensioned method appropriate for
less general concepts they contain (e.g. CDP bins are a reflection 
processing concept only appropriate to an interface aimed at something
like seismic unix).  
\author Gary L. Pavlis
*/

class ExternalProcessor
{
public:
	/*! \brief Load a simple vector for processing.
	
	\param x vector of data to be loaded for processing.
	\param n number of elements in x.
	\param name Symbolic name to be attached to this data. 
		This may not be relevant for all systems.  If not
		needed use a default to link to this virtual base.
	*/  
	virtual void load(double *x,int n, string name)=0;
	/*! \brief Load a TimeSeries object for processing.
	
	\param d TimeSeries data to be loaded for processing.
	\param name Symbolic name to be attached to this data. 
		This may not be relevant for all systems.  If not
		needed use a default to link to this virtual base.
	*/  
	virtual void load(TimeSeries& d,string name)=0;
	/*! \brief Load a matrix for processing.
	\par
	Most seismic systems treat ensembles of data as a matrix.
	This function allows that to happen through a dmatrix
	object known to the SEISPP library.
	
	\param d  data to be loaded for processing.
	\param name Symbolic name to be attached to this data. 
		This may not be relevant for all systems.  If not
		needed use a default to link to this virtual base.
	*/  
	virtual void load(dmatrix& d, string name)=0;
	/*! \brief Load a ThreeComponentSeismogram object for processing.
	\par
	Three component seismograms are a unique seismological concept.
	Different systems handle them very differently, but this method
	should allow loading one set of seismogram for processing.
	
	\param d  data to be loaded for processing.
	\param name Symbolic name to be attached to this data. 
		This may not be relevant for all systems.  If not
		needed use a default to link to this virtual base.
	*/  
	virtual void load(ThreeComponentSeismogram& d,string name)=0;
	/*! \brief Load a TimeSeriesEnsemble object for processing.
	
	\param tse  data to be loaded for processing.
	\param name Symbolic name to be attached to this data. 
		This may not be relevant for all systems.  If not
		needed use a default to link to this virtual base.
	*/  
	virtual void load(TimeSeriesEnsemble& tse,string name)=0;
	/*! \brief Load a ThreeComponentEnsemble object for processing.
	
	\param tce  data to be loaded for processing.
	\param name Symbolic name to be attached to this data. 
		This may not be relevant for all systems.  If not
		needed use a default to link to this virtual base.
	*/  
	virtual void load(ThreeComponentEnsemble& tce,string name[3])=0;
	/*! \brief Run processing using instructions in a text string.
	\par
	All processors I know of can be driven through some form of 
	command parser.  This method should allow passing these instructions
	to data initialized through previous load instructions.
	\param text is the text string to define processing.
	*/  
	virtual void process(string text)=0;
	/*! \brief  Run processing using instructions in a text string.
	\par
	All processors I know of can be driven through some form of 
	command parser.  This method should allow passing these instructions
	to data initialized through previous load instructions.  This is
	very similar to the string arg version except it is overloaded to 
	allow passing a raw char *.  
	\param s is the text string to define processing.
	*/  
	virtual void process(char *s)=0;
	/*! \brief Run processing using instructions passed through a stream object.
	\par
	The intent of this method, if implemented, is to allow a user to 
	interactively drive an external processor.  The idea is to connect the
	user to the processor through a stream that echoes what they type to 
	the input of the processor.  Alternatively, this could also be used to
	open a file and pass it downstream to a processor.
	\param strm is the input stream to be passed into the processor.  The 
		ifstream declaration assures this is not a bidirectional stream
		but one that only allows data to be read and pushed to the 
		external processor.  A tacit expectation is that processing would
		continue till eof on the istream.
	*/  
	virtual void process(ifstream& strm)=0; 
	/*! \brief Retrieve a vector of data from an external processor.
	\param name is a symbolic name used to refer to a specific vector member.
	*/  
	virtual vector<double> retrieve_vector(string name)=0;
	/*! \brief Retrieve a matrix of data from an external processor.
	\param name is a symbolic name used to refer to a specific matrix member.
	*/  
	virtual shared_ptr<dmatrix> retrieve_matrix(string name)=0;
};

} // End SEISPP namespace declaration
#endif
