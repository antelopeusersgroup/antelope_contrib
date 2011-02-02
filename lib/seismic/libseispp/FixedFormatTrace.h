#ifndef _FIXEDFORMATTRACE_H_
#define _FIXEDFORMATTRACE_H_
#include <stdio.h>
#include <string>
#include "HeaderMap.h"
namespace SEISPP{
using namespace std;
using namespace SEISPP;

/*! \brief Generic object that can be used to easily access seismic data
in a number of common external formats or user defined formats.

Most seismic data is stored externally in files with trace header
having a fixed set of attributes in specific spots.  Classic examples
are SEGY and SAC.  The object was designd to provide a common interface
into any seismic data format that is structured as a header in one
distinct binary block and data in another.  The concept used here is
to simply read the data in as a raw binary object and provide 
an series of indices into the the binary blob through the API.  

This object is also a low level interface for three component 
seismgrams stored with the same concept:  header followed by
data.  This is a low level interface because it assumes the data
are still stored as a vector of sample data.  An attribute is 
used to define the order of the sample data for this case.  
In the SEISPP library 3c data are abstracted as a 3xns matrix.
For a matrix the data can be stored in either column order 
(ala fortran) or in row order (ala C 2d arrays).  The interface
handles this through a public boolean attribute.  The constructors
set this when parsing the format description.  The most important
thing to reiterate is that the interface here will only view the
sample data as a vector of data.  The caller will in a higher 
level interface should simplify this user to build 3C data objects.
This was done intentionally to make this object more general.

The construction of one of these beasts is a bit unusual and 
worth noting up front.  Because the mapping operation of data
and trace attributes in formats like this is fixed, we don't 
want to rebuild the structure over and over again.  Hence the
normal usage is to call a simple constuctor that builds a skeleton
for the particular external format.  One then uses this skeleton
in secondary calls that have a FILE pointer as an argument.  
Reading then involves cloning the skeleton and filling in the
actual data with raw binary read (putting flesh on the bones). 
*/
class FixedFormatTrace : public BasicTimeSeries
{
public:
        /*! The default constructor.

          This exists to avoid default behaviour which for the
          current implementation is problematic.  Creates an 
          empty and invalid container.
          */
        FixedFormatTrace();
	/*! Construct an empty trace object defined by name type.

	This constructor is used in two very different contexts.
	First, if one want to read a set of data in a specific
	format one should first call this constructor defaulting
	the nsamp parameter (zero value).  When nsamp is zero
	this contructor just builds the skeleton of the object
	with no data.  Later reads with a FILE parameter can be 
	used to load many traces without the overhead of building
	the complicated indices required when this particular
	constructor is called.

	The second use of this constructor is to build an empty 
	object of this type with a specified number of samples
	and all the attributes set to default values.  

	Note the both uses of this constructor hit a special parameter
	file that is assumed to have been installed in 
	the standard Antelope location for parameter files
	($ANTELOPE/data/pf).  The indexing for this particular
	format defined by the parameter passed as the argument type
	is defined in this parameter file.  This is a complicated
	parameter file with nested parameters.  Full description of
	it will wait until this code is in production form.  At 
	this writing it is experimental. 

	\param type data format name.  This name is used to search
	for a tag name in the parameter file that defines the
	layout of this particular data format.

	\param nsamp number of samples in data are to create for this 
	object.  If 0 (default) no data are is created and the 
	result can be used as a skeleton to efficiently clone objects
	of this type. 

	\exception throws a SeisppError for a variety of conditions
	possible in parsing the complicated parameter file used to
	describe a data format.
	*/
	FixedFormatTrace(string type,int nsnew=0);
	/*! \brief standard copy constructor. 

	Not trivial because of need to manage internal pointers.
	New block of memory is allocated and copied by this 
	constructor so be aware. */
	FixedFormatTrace(const FixedFormatTrace& parent);
	/*! Main constructor for this object.  

	This data object is designed to allow raw binary reads of 
	trace data from a file with fixed length binary header that
	can be loaded with fread.   It is assumed the interface 
	takes care of all details of mapping the actual bits to 
	something that can be extracted.  This constructor clones
	the skeleton that defines the indexing from parent and 
	reads ns samples from the file pointer fp.  This is possible
	because the skeleton allows the number of bytes to be 
	read by fread to be computed.
	\param parent whose skeleton is used to build this object
	\param fp plain C i/o FILE handle to read binary data
	\param nsamp number of samples to read from fp
	\param key keyword used to extract dt sample interval from 
		header after the data are read.
	\param key_is_dt some formats stored dt and other store
		samprate=1/dt.  When true (default) attribute extracted
		by key is assumed to be dt.  Otherwise the attribute
		will be converted to dt by 1/value.
	*/
	FixedFormatTrace(const FixedFormatTrace& parent,FILE *fp,int nsamp,
		string key, bool key_is_dt=true);
	/*! Similar to above, but ns is extracted from the header using
	the keyword defined by nsamp_keyword */
	FixedFormatTrace(const FixedFormatTrace& parent,
		FILE *fp,string nsamp_keyword,string dt_keyword,
		bool key_is_dt=true);
	/*! Standard destructor.  Not trivial here because this
	object has hidden raw pointers. */
	~FixedFormatTrace(); 
        /*! Get a name that defines this format.

          This is a query routine that is linked to constructors.
          That is, one will sometimes want to know some tag
          that defines the format this object is linked to.  For
          example, this can allow an application to not have frozen
          in a format type.  In any case this method returns the
          name given that defines this format to the original 
          constructor.

          \return Name of this format.
          */
        string format_name()
        {
            return dataformat;
        };
        /*! Change trace size for variable ns data.

          Some formats allow variable length data while some 
          require all data to have the same length.  Variable length
          formats will need to call this method when writing to 
          change the buffer size.  This should normally called before 
          loading data or one cannot guarantee the integrity of the 
          header data let alone the danger of writing to the data 
          vector when the size is wrong.  

          \param nsnew new number of samples 
          \return size of newly created data buffer in bytes = size
             in bytes of new data buffer.
          */
        long int resize(int ns);

	template <class T> T get(string name);
	template <class T> void put(string name, T value);
	template <class T> T get(const char *name);
	template <class T> void put(const char *name, T value);
	string get_string(string name);
	string get_string(const char *name);
	void put_string(string name, const char *value);
	void put_string(string name, string value);
	/*! Initialize all data samples to 0.

	When writing into this object is is nearly always desirable
	to first initialize the contents to all zeros.  This method does this.
	*/
	void zero();
	/*! General data copy for double array.

	This method provides a general way to copy the contents of a data vector
	into this object (e.g. before writing).  nsamp data samples are copied 
	to internal storage beginning at offset.  i.e. sample 0 of d will be 
	placed internally at vector position offset.  The internal contents
	are cleared before the copy and and will be silently truncated if 
	nsamp overflows internal trace length.  

	\param nsamp is the length of the vector d
	\param offset is the first position result to start copying d (see above).
		Must be nonnegative.
	\param d is the data vector to copy.

	\exception SeisppError will be thrown if passed a negative offset.
	*/
	
	void put(int nsamp,int offset, double *d);
	/*! General data copy for stl double vector.

	This method provides a general way to copy the contents of a data vector
	into this object (e.g. before writing).  The samples in d are copied 
	to internal storage beginning at position offset.  i.e. sample 0 of d will be 
	placed internally at vector position offset.  The internal contents
	are cleared before the copy and and will be silently truncated if 
	nsamp overflows internal trace length.  

	\param offset is the first position result to start copying d (see above).
		Must be nonnegative.
	\param d is the data vector to copy.
	\exception SeisppError will be thrown if passed a negative offset.
	*/
	void put(int offset,vector<double> d);
	/*! \brief Return sample data as an STL vector.

	Usually one wants the entire vector of sample data to manipulate.
	For efficiency it is usually much better to call this method to
	retrieve all the than manipulating the contents one sample at
	a time with the operator() method.  In fact this method just
	calls operator() ns times to retrieve and convert all the contents
	to doubles.
	*/
	vector<double> data();
	/*! Return one sample value.  

	This is comparable in function to the data method, but retrieves
	only one sample at a time.  Sample retrieved is by sample number
	isap.*/
	double operator()(int isap);
	/*! Load a vector of data into the object.

	This method is the main one used to load data into this generic
	object.   For a variety of reasons my design choice or this was
        made intentionally less general than it could have been.  That is,
        one might want to use operator [] to access samples, but this proved
        both awkward and inefficient.  This led to this interface method that
        simply requires one supply a vector of some rational type. */
	template <class T>void load(vector<T> d);
	void zero_gaps();
	/*! Binary write to an output stream.

	This friend function will call the write method of ostream
	to write the binary blob that is used internally as a 
	direct image of the contents of this object.  Make sure
	the output file has ios::binary set or results could be
	unpredictable, but certainly system dependent. 

	\param ostr output C++ stream object. 
	*/
	void write(ostream& ostrm);
        /* Return start time = time of first sample. 

           Different formats store time differently.  For example,
           in segy start time is not really stored but is implicitly assumed
           to be a shot time.  In contrast, SAC stores time in
           multiple header attributes used to store a date string.
           As a result this is a virtual method that is expected
           to return an epoch time.  The expectation is specific
           implementations would use polymophism to define this 
           method that are format dependent.  For the two examples
           above, segy can just always return 0 while a SAC 
           version will require computing an epoch time from the
           suite of header values used in SAC to define time 0. 
           Note this method also closely links to the public 
           attribute tref used to define the time standard.  That is,
           if tref is relative the time returned is "relative" to 
           some implicit time standard like shot time while "absolute"
           implies a real epoch time. */
        virtual double tstart(){
            return t0;
        };
        /*! Switch that says if the format is for three component data.

          Although there are no common three component seismogram
          data formats this is intended as a low level inteface that
          allows this model.  See overview of this object for more
          details. When this attribute is true the data samples
          are assumed to define three component seismgrams. */
        bool data_are_3c;
        /*! Define order of three component data.

           3C data can be in one of two orders.  Because 3c data are
           abstracted in this library as a 3xns matrix one can think
           of the two orders as column order or row order.  Alternatively
           one can talk about channel order of time multiplexed format.
           When this boolean is true the data are presume to be in
           channel order.  When false they data are assumed time 
           multiplexed. */
        bool channel_order;
        /*! Return format description of this data object.

          Sometimes we want some kind of description of what type 
          of data object this refers to.  Necessary, for example,
          if a program was handling several formats.  This simply
          returns a string that describes the format.  Up to 
          implementation to define what that is */
        string format_description(){return(dataformat);};
private:
	HeaderMap header;
	AttributeType stype;
        string dataformat;
	unsigned char *h;  /* points to start of header data */
	unsigned char *d;  /* points to start of data samples */
	bool data_loaded;
	size_t size_of_this;
        /* This attribute is tightly linked to the t0 attribute 
           inherited from BasicTimeSeries and the related attribute tref.
           t0_default should be loaded by any constructor as teh default
           start time for this seismogram.  That should almost always be
           zero, but still best defined explicitly.  When the object
           referenced actually contains data the BasicTimeSeries 
           attribute t0 may or may not be relevant.  For formats like
           SEGY t0 should just always be set to 0 and this base object
           can be used directly.  Formats like SAC, in contrast, should
           use polymorphism to redefine the start method to compute
           t0 from header entries.  In this case, t0_default can be
           used only as a possible recovery to blunder on. */
        double t0_default;
};
template <class T> T FixedFormatTrace::get(string name)
{
    try {
	const string base_error("FixedFormatTrace::get method: ");
	if(h==NULL)
		throw SeisppError(base_error
		 + string("data pointer for this object is NULL.\n")
		 + string("Probable coding error.  Check documentation.") );
	AttributeType rdtype=this->header.dtype(name);
        long lival;
        int ival;
        short int sival;
        bool bval;
	unsigned char ucval;
	float rval;
        double dval;
	T result;
	switch (rdtype)
	{
	case INT64:
		lival=this->header.get<long>(name,h);
		result=static_cast<T>(lival);
		break;
	case INT32:
		ival=this->header.get<int>(name,h);
		result=static_cast<T>(ival);
		break;
	case INT16:
		sival=this->header.get<short int>(name,h);
		result=static_cast<T>(sival);
		break;
	case BOOL:
		bval=this->header.get_bool(name,h);
		/* this does nothing but seems necessary to 
		handle the general typing */
		result=static_cast<T>(bval);
		break;
	case BYTE:
		ucval=this->header.get<unsigned char>(name,h);
		result=static_cast<T>(ucval);
		break;
	case REAL32:
		rval=this->header.get<float>(name,h);
		result=static_cast<T>(rval);
		break;
	case REAL64:
		dval=this->header.get<double>(name,h);
		result=static_cast<T>(dval);
		break;
	case STRING:
		if(typeid(T)==typeid(string))
		{
			string sval;
			sval=this->header.get_string(name,h);
			/* This oddity seems necessary to fool the compiler
			to make this template work on both string and 
			numeric values.  Basically the opaque pointer 
			intermediary and the reiterpret cast is an 
			I'm sure of this trick */
			T *stemp;
			stemp=reinterpret_cast<T *>(&sval);
			result = *stemp;
		}
		else
		{
			throw SeisppError(base_error
			 + string("cannot convert attribute=")
			 + name 
			 + string("\nType mismatch does not allow conversion. ")
			 + string("Check header attribute definitions.") );
		}
		break;
	case HDRINVALID:
	default:
		throw SeisppError(base_error
		 + string("Attribute tagged with name=")
		 + name
		 + string(" is not defined for data format requested") );
	}
	return(result);
    } catch (...) {throw;}

}
template <class T> void FixedFormatTrace::put(string name,T value)
{
    try {
	const string base_error("FixedFormatTrace::put method: ");
	int64_t lival;
	int32_t ival;
	int16_t sival;
	unsigned char ucval;
	bool bval;
        float rval;
        double dval;
	if(h==NULL)
		throw SeisppError(base_error
		 + string("data pointer for this object is NULL.\n")
		 + string("Probable coding error.  Check documentation.") );
	AttributeType rdtype=this->header.dtype(name);
	switch (rdtype)
	{
	case INT64:
		/* this assumes int is 32 bits */
		lival=static_cast<int64_t>(value);
		this->header.put<long>(name,lival,h);
		break;
	case INT32:
		/* this assumes int is 32 bits */
		ival=static_cast<int32_t>(value);
		this->header.put<int>(name,ival,h);
		break;
	case INT16:
		sival=static_cast<int16_t>(value);
		this->header.put<short int>(name,sival,h);
		break;
	case BYTE:
		ucval=static_cast<unsigned char>(value);
		this->header.put<unsigned char >(name,ucval,h);
		break;
	case BOOL:
		bval=static_cast<bool>(value);
		this->header.put_bool(name,bval,h);
	case REAL32:
		rval=static_cast<float>(value);
		this->header.put<float>(name,rval,h);
		break;
	case REAL64:
		dval=static_cast<double>(value);
		this->header.put<double>(name,dval,h);
		break;
	case STRING:
		throw SeisppError(base_error
			+ "Attribute with name="
			+ name
			+ " is a string variable.  Use put_string method instead of template");
	case HDRINVALID:
	default:
		throw SeisppError(base_error
		 + string("Attribute tagged with name=")
		 + name
		 + string(" is not defined for data format requested") );
	}
    } catch (...) {throw;}
}
template<class T> T FixedFormatTrace::get(const char *name)
{
	try {
		T result=this->get<T>(string(name));
		return(result);
	} catch(...){throw;};
}
template<class T> void FixedFormatTrace::put(const char *name, T value)
{
	try {
		this->put<T>(string(name),value);
	} catch(...){throw;};
}
template <class T>void FixedFormatTrace::load(vector<T> dvec)
{
        int bytes_per_sample;
        switch(stype)
        {
        case INT16:
                bytes_per_sample=2;
                break;
        case REAL64:
        case INT64:
                bytes_per_sample=8;
                break;
        case INT32:
        case REAL32:
        default:
                bytes_per_sample=4;
        }
	int dsize=dvec.size();
	size_t datasize;
	if(dsize!=ns)
	{
		datasize=bytes_per_sample*dsize;
		size_of_this=datasize+header.size();
		h=(unsigned char *)realloc(static_cast<void *>(h),size_of_this);
		if(h==NULL) 
		  throw SeisppError(string("FixedFormatTrace::load:  ")
			+string("realloc failed."));
		d=h+header.size();
		ns=dsize;
	}
	else
	{
		datasize=bytes_per_sample*ns;
	}
	int i;
        long int *liptr;
	short int *siptr;
	int *iptr;
	float *fptr;
	double *dptr;
	switch (stype)
	{
	case INT16:
		siptr=new short int[ns];
		for(i=0;i<ns;++i) 
			siptr[i]=static_cast<short int>(dvec[i]);
		memcpy(static_cast<void *>(d),static_cast<const void *>(siptr),datasize);
		delete [] siptr;
		break;
	case INT32:
		iptr=new int[ns];
		for(i=0;i<ns;++i) 
			iptr[i]=static_cast<int>(dvec[i]);
		memcpy(static_cast<void *>(d),static_cast<const void *>(iptr),datasize);
		delete [] iptr;
		break;
	case INT64:
		liptr=new long[ns];
		for(i=0;i<ns;++i) 
			liptr[i]=static_cast<long>(dvec[i]);
		memcpy(static_cast<void *>(d),static_cast<const void *>(iptr),datasize);
		delete [] iptr;
		break;
	case REAL32:
		fptr=new float[ns];
		for(i=0;i<ns;++i) 
			fptr[i]=static_cast<float>(dvec[i]);
		memcpy(static_cast<void *>(d),static_cast<const void *>(fptr),datasize);
		delete [] fptr;
		break;
	case REAL64:
		dptr=new double[ns];
		for(i=0;i<ns;++i) 
			dptr[i]=static_cast<double>(dvec[i]);
		memcpy(static_cast<void *>(d),static_cast<const void *>(dptr),datasize);
		delete [] dptr;
		break;
	default:
		throw SeisppError(string("FixedFormatTrace::load:  ")
			+ string("Unsupported sample type format.  Cannot convert"));
		
	}
}
} // End SEISPP namespace encapsulation
#endif
