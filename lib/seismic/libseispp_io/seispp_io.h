#ifndef _SEISPP_IO_H_
#define _SEISPP_IO_H_

#include <string>
#include <iostream>
#include <fstream>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include "seispp.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP {
/*! This string is written after each object when more data follows..*/
const string more_data_tag("MORE");
/*! This is written at the end of the file immediately before text conversion
of nobject count */
const string eof_tag("ENDF");
/* For binary files it is preferable to use a simpler set of keywords to
 * define blocks with a fixed size.  This should be the size of the above +1.
 Binary tag is text tag values above result with c_str() method.
 e.g. eof_tag.c_str()*/
#define BINARY_TAG_SIZE 5
/*! We write the number of objects in set of concatenated serial objects
   at the end of the file.  We seek back this many bytes to read the
   number of objects written */
const int TextIOStreamEOFOffset(64);
const int BinaryIOStreamEOFOffset(BINARY_TAG_SIZE+sizeof(long));
/*! Legacy writer for archive connected to stdout - original seispp_filters */
template <class OutputObject> void write_object(OutputObject& d,
        boost::archive::text_oarchive& oa)
{
    try {
        oa << d;
    }catch(...)
    {
        throw SeisppError(string("write_object failed\n")
                +"Is serialization defined for this object type?\n"
                +"Do you have write permission for output directory?");
    }
}
/*! Legacy reader for archive connected to stdin- original seispp_filters */

template <class InputObject> InputObject
    read_object(boost::archive::text_iarchive& ia)
{
    InputObject d;
    try{
        ia>>d;
    }catch(...)
    {
        /* This template ignores errors thrown by boost
           and converts to a standard error for the seispp
           library - perhaps not ideal but what is done here. */
        throw SeisppError(string("read_object failed:  ")
                    + "Check that input file is a boost text archive file");
    }
    return d;
}
} // End namespace SEISPP declaration
#endif
