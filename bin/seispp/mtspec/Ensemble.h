#include <vector>
#include "Metadata.h"
#include "TimeSeries.h"
#include "ThreeComponentSeismogram.h"
using namespace SEISPP;
template <typename Tdata> class Ensemble : public Metadata
{
public:
  vector<Tdata> member;
  Ensemble(Metadata& ensmd, int nmembers);
  Ensemble(const Ensemble<Tdata>& parent);
  Ensemble& operator=(const Ensemble<Tdata>& parent);
private:
  friend class boost::serialization::access;
  template<class Archive>void serialize(Archive & ar, const unsigned int version)
  {
    ar & boost::serialization::base_object<Metadata>(*this);
    ar & member;
  };
};
template <typename Tdata>
  Ensemble<Tdata>::Ensemble(Metadata& md, int nmembers) : Metadata(md)
{
  member.reserve(nmembers);
}
template <typename Tdata>
  Ensemble<Tdata>::Ensemble(const Ensemble& parent)
      : Metadata(dynamic_cast<Metadata&>( parent))
{
  member=parent.member;
}
template <typename Tdata>
  Ensemble<Tdata>& Ensemble<Tdata>::operator=(const Ensemble& parent)
{
  if(this!=(&parent))
  {
    *this=Metadata::operator=(parent);
    member=parent.member;
  }
  return *this;
}
typedef  Ensemble<TimeSeries> TimeSeriesEnsemble;
typedef Ensemble<ThreeComponentSeismogram> ThreeComponentEnsemble;
