#ifndef _BASICOBJECTWRITER_H_
#define _BASICOBJECTWRITER_H_
namespace SEISPP{
using namespace SEISPP;
/*! \brief Abstract base class for generic object writer.

This is the base class for a collection of C++ classes designed to
abstract and simplify the process of reading and writing of seispp data
objects.   The purpose of this base class is to provide the foundation for
a set of polymorphic objects for saving data in varous forms.
*/
template <class T> class BasicObjectWriter
{
  public:
    virtual long number_already_written()=0;
    /*! Writer - cannot be virtual for reasons unclear to me. */
    void write(T);
};
}
#endif
