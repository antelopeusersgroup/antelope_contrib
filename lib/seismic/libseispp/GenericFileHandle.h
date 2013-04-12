#ifndef _LINEAR_FILE_HANDLE_
#define _LINEAR_FILE_HANDLE_
#include <fstream>
#include "Metadata.h"
#include "TimeSeries.h"
#include "ensemble.h"
#include "FixedFormatTrace.h"
#include "AttributeCrossReference.h"
namespace SEISPP
{
using namespace std;
using namespace SEISPP;
/* \brief Generic file handle for seismic data stored with fixed length headers.

   Much multichannel processing was born from the concept of linear file 
   formats born of tape processing.  This model remains useful for 
   many types of data processing.  This object attempts to standardize 
   reading and writing of data into such file formats with the only 
   restriction being that the data have to have header data stored in 
   a fixed length structure.  The header structure itself is defined in 
   the writer by constructors and is considered absolutely immutable.
   */

class GenericFileHandle
{
    public:
        /*! Full constructor. 

          This is a fully parameterized constructor that defines
          all the components needed to make the object valid.  
          A file accessed by this handle is in either read or write
          mode, but it cannot be both. 

          \param filename is the data file to be read or created.
          \param tracetype is a unique data type identifier used 
            internally to define format (implementation dependent)
          \param namemap is an object that provides a cross referencing
            between the internal and external namespace.
          \param orderkeys is a list of attributes used to define
            internal groupings during reads.  Ignored in writing 
            so writers can leave this empty. 
          \param ensmdlist is a list of internal names to be loaded
            as global for the entire ensemble.  These are extracted
            for input and pushed to all headers in output.  
          \param tmdlist is a list of internal names for metadata to
            be loaded for every object read or to be stored for
            each object written.
          \param read_only is true if as name implies this is to
            be used only in read mode. 
          \param ns_key is a Metadata tag (internal name, not external
            name for given file format) for number of samples.
          \param dt_key is a Metadata tag (internal name, not external)
            used to define sample interval in seconds. 
          \param dtscl is a scale factor to be applied to the external
            files stored sample rate.  This is necessary, for example,
            with segy/su files where dt is defined in microseconds. 
          \param using_sample_interval is used as a flag to define
            the physical meaning of the dt_key parameter.  When 
            this parameter is true it is assumed the dt_key attribute
            is the sample interval.  When false it is assumed the
            value stored is sampling frequency = 1/dt.  (default true)
          \param nowritedead defines how to handle traces maked dead.
            When true (the default) traces marked dead (live=false)
            are ignored.  When false a zero pad version will be inserted.
            The later, for example, is used by segy.
          \exception A SeisppError can be thrown for a number of reasons.
            There are the obvious ones of open failure, but a more 
            subtle issue needs to be recognized.  The following must
            be defined in the required metadata list or the constructor
            will throw an excpetion:  nsamp and dt.
         */
        GenericFileHandle(string filename, string tracetype, 
                AttributeCrossReference& namemap, list<string> orderkeys,
                list<string> ensmdlist, list<string> tmdlist, 
                bool read_only, string ns_key, string dt_key, double dtscl,
                bool using_sample_interval=true,bool nowritedead=true);
        /*! \brief Simplified constructor with file name and type only.

          Children of this handle for particular trace formats 
          will normally use this constructor.   It builds only core
          components that have to be initialized by a "construction is
          initialization" algorithm.   Any use of this constructor 
          will require a later call to the protected method called
          set_required.  The object itself has 
          internal checks for validation of required parameters.   
          The intent is most uses of this object will use this 
          constructor in the standard colon syntax followed by calls 
          to set_required.   This allows the details of how these
          messy elements are constructed to be hidden behind the 
          interface.

          \param filename is the data file to be read or created.
          \param tracetype is a unique data type identifier used 
            internally to define format (implementation dependent)
          \param read_only is boolean as the name implies.
          */
        GenericFileHandle(string filename, string tracetype,
                bool read_only=true);
        /*! Copy constructor.

          The current implementation does not actually allow this
          handle to be copied.  If you try to call this constructor
          a SeisppError exception will be thrown.  The idea is that
          if you need to move copies of this handle around wrap the
          master copy in a boost::shared_pointer or manage the 
          destruction of the object yourself.  The problem is that
          the handle maintains an open file that needs to be closed
          and something like a shared_pointer is required for counting
          to avoid closing the file prematurely. */
        GenericFileHandle(const GenericFileHandle& parent);
        ~GenericFileHandle();
        auto_ptr<TimeSeriesEnsemble> GetNextEnsemble();
        TimeSeries GetNextSeismogram();
        /* \brief Test for end of file during reading.

           Data driven loops require a mechanism to inform the 
           caller when there is no more data to read.  This method
           is comparable to the eof method in the stardard
           istream operator >>.  That is, it returns true when 
           the last read touches eof.  The bottom line is that
           the read logic when using this handle should have
           a while loop with a call to the eof method and 
           the top of the while loop should issue a call to one
           of the read methods followed by code to do something
           to data read.  This allows proper handling of the last
           object in the file. 
           */
        bool eof();
        /*! Write one seismogram to output.  

          \param d is the TimeSeries object containing data to be written.

          \return 1 on success (not normally useful)
          \exception SeisppError can be thrown for a variety of potential
            problems.
            */
        int put(TimeSeries& d);
        /*! Write ensemble of data.  

        \param d contains the data ensemble to be written to output.
        \return number of traces written.
        \exception SeisppError will be thrown if there are problems.
        */
        int put(TimeSeriesEnsemble& d);
        /*! Write a binary blob.

          Some formats (e.g. true SEGY as opposed to Seismic Unix
          format) have other stuff that needs to be written to 
          a file that does not match the fixed length record
          model exactly on which this object is built.  An example
          are the so called reel headers of SEGY.  We allow writing
          such a thing through an opaque pointer that needs to 
          normally be cast by the caller to an const char 
          pointer.   This has the universal warning that we
          implicitly assume the pointer is directed at where
          you think it is and chaos will result if it is anything
          else.  The only exception is that a NULL pointer will
          create an exception.

          \param b is a pointer to the first byte of the block
          of data to be written to output at the current file position.
          \param nb is the number of bytes from b to write.

          \return number of bytes successfully written
          
          \exception SeisppError is thrown if b is nill.
          */

        int put(const char *b, int nb);
        /* \brief Load and return a ThreeComponentSeismgram object.

           Some formats can and should allow gathers of three component
           seismgrams organized in one of two orders:  channel order or
           time frame order.  The interface should take care of handling
           this important detail in the constructor as is done in this
           implementation.  Since ThreeComponentSeismograms objects
           contain a transformation matrix the interface and format
           definition should define this and assume these attributes
           in each member are initialized.  In this implementation
           the user can always assume the returned seismgrams have
           been rotated to standard coordinates as describe in the
           documentation for a ThreeComponentSeismgram object.  

           \exception can throw a SeisppError object for a range of
           possible problems. */
        auto_ptr<ThreeComponentEnsemble> GetNext3CEnsemble();
        /* \brief read and load one ThreeComponentSeismogram object.


           Some formats can and should allow simple reads of three component
           seismgrams organized in one of two orders:  channel order or
           time frame order.  The interface should take care of handling
           this important detail in the constructor as is done in this
           implementation.  Since ThreeComponentSeismograms objects
           contain a transformation matrix the interface and format
           definition should define this and assume these attributes
           in each member are initialized.  In this implementation
           the user can always assume the returned seismgram has
           been rotated to standard coordinates as describe in the
           documentation for a ThreeComponentSeismgram object.  

           \exception can throw a SeisppError object for a range of
           */
        ThreeComponentSeismogram GetNext3CSeismogram();

        /*! Write one three component seismogram to output.  

          Although this interface was designed to insulate users from 
          details of a format, the implmentation will force the output
          to be written as a header followed by the sample data in 
          either time or channel order (a format variation).  
          \param d is the TimeSeries object containing data to be written.

          \return 1 on success (not normally useful)
          \exception SeisppError can be thrown for a variety of potential
            problems.
            */
        int put(ThreeComponentSeismogram& d);
        /*! Write a full ensemble of ThreeComponentSeismogram objects.

          In this implementation this amounts to only slightly more than a 
          loop over members of the input ensemble with members written 
          using the put(ThreeComponentSeismgram) method.  
         
         \param d is the ensemble to be written.  
         
         \exception a SeisppError object may be thrown for a variety 
         of possible errors.
         */

        int put(ThreeComponentEnsemble& d);
        /*! return current file size in bytes. */
        long filesize();
        /*! Position file pointer to beginning of file.  */
        void rewind();
    protected:
        /* \brief Sets all required format dependent variables.
          
           This procedure is a companion to the partial constructor 
           above. It exists mainly to provide a way to build
           simplified constructors for specific formats.  This procedure
           sets the required parameters and an internal boolean that
           validates the handle is complete and in working order.  
           
          \param namemap is an object that provides a cross referencing
            between the internal and external namespace.
          \param orderkeys is a list of attributes used to define
            internal groupings during reads.  Ignored in writing 
            so writers can leave this empty. 
          \param ensmdlist is a list of internal names to be loaded
            as global for the entire ensemble.  These are extracted
            for input and pushed to all headers in output.  
          \param tmdlist is a list of internal names for metadata to
            be loaded for every object read or to be stored for
            each object written.
          \param ns_key is a Metadata tag (internal name, not external
            name for given file format) for number of samples.
          \param dt_key is a Metadata tag (internal name, not external)
            used to define sample interval in seconds. 
          \param dtscl is a scale factor to be applied to the external
            files stored sample rate.  This is necessary, for example,
            with segy/su files where dt is defined in microseconds. 
          \param using_sample_interval is used as a flag to define
            the physical meaning of the dt_key parameter.  When 
            this parameter is true it is assumed the dt_key attribute
            is the sample interval.  When false it is assumed the
            value stored is sampling frequency = 1/dt.  (default true)
          \param nowritedead defines how to handle traces maked dead.
            When true (the default) traces marked dead (live=false)
            are ignored.  When false a zero pad version will be inserted.
            The later, for example, is used by segy.
           */
        void set_required(AttributeCrossReference& namemap,
                list<string> orderkeys,
                    list<string> ensmdlist,
                        list<string> tmdlist,
                            string ns_key,
                                string dt_key,
                                    double dtscl,
                                        bool using_sample_interval,
                                            bool nowritedead);
    private:
        bool handle_not_ready;
        /* This is little routine opens files and stores appropriate
           handle in this object.  Used by all constructors. */
        void initialize_iohandle(string filename, bool read_only);
        FixedFormatTrace dbuffer;
        AttributeCrossReference xref;
        /* Current implementation uses the file name to create an empty 
           lock file derived from the original file name.  This string
           holds the original file name, NOT the lock file name */
        string fname;
        bool readmode;
        FILE *fp;   // defined only when reading.  
        ofstream outstrm;   // Defined only when writing
        list<string> orderkeys;
        list<string> ensemble_mdlist;
        list<string> trace_mdlist;
        /* File size in bytes.  Cached during read for efficiency. */
        long fsize;  
        /* Current file position */
        long current_file_position;
        /* These are needed by FixedFormatTrace constructors.  
        First are internal namespace */
        string nsamp_keyword;
        string dt_keyword;
        /* These are cached external namespace for nsamp and dt respectively
           that are stored for efficiency and so we don't have to constantly
           trap errors for these absolutely required parameters */
        string nskey_ext;
        string dtkey_ext;
        bool key_is_dt;
        bool no_write_dead;
        /* This is used only for 3c data.  When true data in output are
           presumed to be in channel order.  This is effectively the
           transpose of the dmatrix that holds data in 3c data objects.
           For efficiency most 3c formats using the library should
           put data in time multiplex order in which case this 
           attribute should be set false. */
        bool channel_order;  
        /* Some external formats like segy store dt in nonstandard units.
           This is a generic way to rescale the external dt to something
           more appropriate for internal use. */
        double dtscale; //WARNING: PERHAPS SHOULD BE IN FIXEFORMATTRACE OBJECT
        /* The next two methods define common code for get methods.
           First is a method that is the get equivalent of put_metadata_to_dbuffer
           below.  That is, this method standardizes loading of a list of 
           attributes defined by mdlist.  Note that mdlist is a list of
           internal metadata names NOT external. These are converted
           to external names and extracted for the external representation
           abstracted in dext*/
        void LoadMetadata(FixedFormatTrace& dext, 
                Metadata& d, list<string> mdlist);
        /* This is a comparable method for loading required BasicTimeSeries
           attributes with a common set of code. */
        void LoadCommonAttributes(FixedFormatTrace& d, BasicTimeSeries& bts);
        /* This method is used to test order keys */
        bool keys_match(Metadata& current, Metadata& previous);
        /* private method to avoid repetitious code. Loads 
           a list of metadata from d (internal names) to 
         dbuffer (external names)*/
        void put_metadata_to_dbuffer(Metadata& d,
                list<string>& mdlist);
        /* Because sample interval handling is variable with
           format we use this private method to handle 
           if it is stored as dt of 1/dt and if there is a scaling
           as in segy */
        void put_sample_interval(BasicTimeSeries& d);
        /* This file handle is designed to work in a multithreaded 
           environment as one of the primary goals was a way to read 
           and write data on a massively parallel machine.  
           A timeout method is used in trying to obtain a file lock
           during write operations.  Specifically, write methods all
           call the lock function below.  If a lock cannot be obtained 
           the process will sleep for sleep_interval seconds before
           trying again.  That processes is repeated up to retry_limit
           before an exception is thrown.  It may prove useful in the
           future to use a finer granularity than the traditional 
           sleep (1 s intervals) but for now we use that.  Note
           these constants are set in the constructor.*/
        int retry_limit;
        unsigned int sleep_interval;
        /* These two private methods are needed to avoid collisions 
           with multiple threads.  C++ streams library stupidly does
           not have a locking mechanism defined in the interface 
           considering it a system dependent issue - what do those 
           bozos thing an interface is for.  Anyway, these methods
           implement locking on the output stream.  Retry limit is defined
           above to avoid deadlocks. */
        void lock();
        void unlock();
};
} // end SEISPP namespace encapsulation
#endif
