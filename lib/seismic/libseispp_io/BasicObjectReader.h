#ifndef _BASICOBJECTREADER_H_
#define _BASICOBJECTREADER_H_
#include "Metadata.h"
namespace SEISPP{
using namespace SEISPP;
/*! \brief Abstract base class for generic object reader.

This is the base class for a collection of C++ classes designed to
abstract and simplify the process of reading and writing of seispp data
objects.   The purpose of this base class is to provide the foundation for
a set of polymorphic objects for reading data in various generic forms.
*/
template <typename T>
   class BasicObjectReader
{
  public:
    /*! Pure virtual method used to allow an abstract base class.*/
    virtual bool good()=0;
    virtual long number_available()=0;
    T read();
};
}
#endif
