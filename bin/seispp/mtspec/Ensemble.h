#include <vector>
#include "Metadata.h"
template typename<Tdata> Ensemble : public Metadata
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
typedef TimeSeriesEnsemble Ensemble<TimeSeries>;
typedef ThreeComponentEnsemble Ensemble<ThreeComponentEnsemble>;
template<Tdata>
  Ensemble<Tdata>::Ensemble(Metadata& md, int nmembers) : Metadata(md)
{
  member.reserve(nmembers);
}
template<Tdata>
  Ensemble<Tdata>::Ensemble(const Ensemble& parent) : Metadata(md)
{
  member=parent.member;
}
template<Tdata>
  Ensemble<Tdata>::operator=(const Ensemble& parent)
{
  if(this!=(&parent))
  {
    *this=Metadata::operator=(parent);
    member=parent.member;
  }
  return *this;
}
