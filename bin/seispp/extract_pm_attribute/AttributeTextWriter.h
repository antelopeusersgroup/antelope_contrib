#include <string>
#include "seispp.h"
#include "TimeSeries.h"
using namespace SEISPP;
/*! \brief Writes PMTimeSeries attributes to a text file.

  This is a special object made as a companion to extract_pm_attributes.
  The objective is to write simple text files with time, lower error, center,
  and upper error (i.e. 4 columns of text data).  These can be send to 
  shell scripts to be plotted with gnuplot or gmt. */
class AttributeTextWriter
{
public:
    /*! \brief Only constructor.
     *
     \param outdir - directory to write files generated from each call to write
     \param skipgaps - when true drop areas marked as gaps (default)
     */
    AttributeTextWriter(string outdir,bool skipgaps=true);
    /*! \brief Writer
     *
     \param fname - file name to write to data (writer ALWAYS prepends dir 
        set during construction
     \param  d - data (center) to be plotted
     \param  e - parallel (to d) error data.
     \returns - number of samples written. 
     */
    int write(string fname, TimeSeries& d, TimeSeries& e);
private:
      string dir;
      bool killgaps;
};
