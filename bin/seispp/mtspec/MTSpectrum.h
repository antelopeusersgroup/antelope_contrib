#include <vector>
#include "dmatrix.h"
#include "MatlabProcessor.h"
template typename<Tens,Tdata> class MTSpectrum
{
public:
  MTSpectrum();
  MTSpectrum(int time_bandwidth_product);
  MTSpectrum(const MTSpectrum& parent);
  TimeSeries spectrum(Metadata& md, vector<double>& d);
  TimeSeries spectrum(Metadata& md, double *d, int nd);
  /* This series of methods all use call by value because it allows
  simple cloning of the copy received.  We then just return the
  modified copy. */
  TimeSeries spectrum(TimeSeries d);
  /*! Process a 3C seismogram object.

  The dmatrix returned in this case contains the spectra in the columnes
  of thematrix.  Note that is the transpose of the data matrix */
  ThreeComponentSeismogram spectrum(ThreeComponentSeismogram d);
  /* Ensembles are made templates here to eliminate repetitious code. */
  template typename<Tens,Tdata> Tens spectrum(Tens d);
  /* This method is actually called by the above.  The distinction is that
  in a 3c seismogram the matrix has to be transposed to match matlab.
  When this is used the dmatrix is passed directly */
  TimeSeriesEnsemble spectrum(TimeSeriesEnsemble d);
  ThreeComponentEnsemble spectrum(ThreeComponentEnsemble d);
  int time_bandwidth_product(){return tbp;};
  MTSpectrum& operator=(const MTSpectrum& parent);
private:
  int tbp;
  /* We make this a shared ptr to allow copying although  it complicates
  the constructors */
  shared_ptr<MatlabProcessor> matlab;
};

template typename<Tens,Tdata> Tens MTSpectrum::spectrum(Tens d)
{
  try{
    int i;
    for(i=0;i<d.member.size();++i)
    {
      Tdata spec;
      spec=this->spectrum(d.member[i]);
      d.member[i]=spec;
    }
    return d;
  }catch(...){throw;};
}
