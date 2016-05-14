#include "ThreeCEnsembleTimePicker.h"
#include "seiswplot.h"
using namespace std;
using namespace SEISPP;
/* key for arrival time - only used in the scope of this file */
const string TCPICKKEY("arrivaltime");
/* TCPICKKEY values are initialzied to this value to mark them as not having a
pick */
const double null_pick(-9.999e99);
/* this is the test value - smaller than this defines null */
const double null_pick_test(-1e20);
namespace SEISPP
{
  /* This is the callback routine used for component 1 (0)*/
void pick_times_callback1(Widget w, void *cdata, void *udata)
{
  ThreeCEnsembleTimePicker *tcp=reinterpret_cast<ThreeCEnsembleTimePicker *>(cdata);
  tcp->set_active_component(0);
  vector<SeismicPick> picks= tcp->pick_times();
  /* This sets the TCPICKKEY fields of each seismogram with pick times.
  Made a procedure as this is common code to all 3 component callbacks. */
  tcp->set_pick_times(picks);
}
/* This is the callback routine used for component 2 (1)*/
void pick_times_callback2(Widget w, void *cdata, void *udata)
{
ThreeCEnsembleTimePicker *tcp=reinterpret_cast<ThreeCEnsembleTimePicker *>(cdata);
tcp->set_active_component(1);
vector<SeismicPick> picks= tcp->pick_times();
/* This sets the TCPICKKEY fields of each seismogram with pick times.
Made a procedure as this is common code to all 3 component callbacks. */
tcp->set_pick_times(picks);
}
/* This is the callback routine used for component 3 (2)*/
void pick_times_callback3(Widget w, void *cdata, void *udata)
{
ThreeCEnsembleTimePicker *tcp=reinterpret_cast<ThreeCEnsembleTimePicker *>(cdata);
tcp->set_active_component(2);
vector<SeismicPick> picks= tcp->pick_times();
/* This sets the TCPICKKEY fields of each seismogram with pick times.
Made a procedure as this is common code to all 3 component callbacks. */
tcp->set_pick_times(picks);
}
/* This is a helper for the constructors that sets up the control widgets in
on place.  Avoids code duplication in multiple constructors */
void ThreeCEnsembleTimePicker::build_pick_menu()
{
  /* We use this workspace to use the (nasty) BuildMenu procedure taken
  from dbxcor.   The biggest problem with BuildMenu is that it uses a
  crazy null struct to define the length of the array rather than required
  a count. This is an ugly C initialization */
  MenuItem pickdata[4]={
      {(char *)"Pick C1",
      &xmPushButtonGadgetClass,'1',
      (char *)"<Key>1",(char *)"Pick C1",
      pick_times_callback1,
      this,NULL,(MenuItem *)NULL},
      {(char *)"Pick C2",
          &xmPushButtonGadgetClass,'2',
          (char *)"<Key>2",(char *)"Pick C2",
          pick_times_callback2,
          this,NULL,(MenuItem *)NULL},
      {(char *)"Pick C3",
              &xmPushButtonGadgetClass,'3',
              (char *)"<Key>3",(char *)"Pick C3",
              pick_times_callback3,
              this,NULL,(MenuItem *)NULL},
      {NULL,NULL,'\0',NULL,NULL,NULL,NULL,NULL,NULL}};
  /* menu_bar here comes from protected SeismicPlot.   We enable all
  e plot windows with the same structure. */
  for(int k=0;k<3;++k)
  {
    this->pick_menu[k]=BuildMenu(components[k].menu_bar,XmMENU_PULLDOWN,
          (char *)"Pick Times",'E',
          false,pickdata);
    XtManageChild(components[k].menu_bar);
  }
}
ThreeCEnsembleTimePicker::ThreeCEnsembleTimePicker()
{
  data_loaded=false;
  nd=0;
  try{
    /* These constructors will all fail UNLESS the default pf for
    a SeismicPlot is scalar by default.  That is the current situation
    but this is a nasty mysterious feature that could bite someone.  */
    int k;
    GenericTimePicker *ptr;
    ptr=&(components[0]);
    for(k=0;k<3;++k,ptr++)
      ptr=new GenericTimePicker;
    this->build_pick_menu();
    active_component=0;
  }catch(...){throw;};
}
ThreeCEnsembleTimePicker::ThreeCEnsembleTimePicker(Metadata& md)
{
  data_loaded=false;
  nd=0;
  try{
    int k;
    GenericTimePicker *ptr;
    ptr=&(components[0]);
    for(k=0;k<3;++k,ptr++)
      ptr=new GenericTimePicker(md);
    this->build_pick_menu();
    active_component=0;
  }catch(...){throw;};
}
ThreeCEnsembleTimePicker::~ThreeCEnsembleTimePicker()
{
  try{
    int k;
    GenericTimePicker *ptr;
    for(k=0;k<3;++k)
    {
      ptr=&(components[k]);
      delete ptr;
    }
  }catch(...){throw;};
}
int ThreeCEnsembleTimePicker::plot(ThreeComponentEnsemble& din)
{
  try{
    d0=din;
    /* Initialize pick field for all data.  We assume the components
    will inherit this value.  Current implementation does that, but that
    should not be considered a given. */
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=d0.member.begin();dptr!=d0.member.end();++dptr)
      dptr->put(TCPICKKEY,null_pick);
    int k;
    for(k=0;k<3;++k)
    {
      /* This is a obnoxious collision of an interface issue.   This
      procedure returns an auto_ptr which collides with the definition of
      d */
      auto_ptr<TimeSeriesEnsemble> ptr;
      ptr=ExtractComponent(d0,k);
      d[k]=TimeSeriesEnsemble(*ptr);
      components[k].plot(d[k]);
    }
    data_loaded=true;
    nd=d0.member.size();
    return nd;
  }catch(...){throw;};
}
IndexTimes ThreeCEnsembleTimePicker::get_member_times()
{
  try{
    IndexTimes result;
    if(data_loaded)
    {
      int i;
      double arrivaltime;
      vector<ThreeComponentSeismogram>::iterator dptr;
      for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++i,++dptr)
      {
        arrivaltime=dptr->get_double(TCPICKKEY);
        if(arrivaltime>null_pick_test)
        {
            result.insert(pair<int,double>(i,arrivaltime));
        }
      }
    }
    else
    {
      cerr << "ThreeCEnsembleTimePicker::get_member_times:  no data loaded"
        <<endl << "Returning empty pick table"<<endl;
    }
    return result;
  }catch(...){throw;};
}
StaTimes ThreeCEnsembleTimePicker::get_sta_times()
{
  StaTimes result;
  try{
    int i;
    string sta;
    double arrivaltime;
    vector<ThreeComponentSeismogram>::iterator dptr;
    if(data_loaded)
    {
      for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++i,++dptr)
      {
        arrivaltime=dptr->get_double(TCPICKKEY);
        if(arrivaltime>null_pick_test)
        {
          result.insert(pair<string,double>(sta,arrivaltime));
        }
      }
    }
    else
    {
      cerr << "ThreeCEnsembleTimePicker::get_member_times:  no data loaded"
        <<endl << "Returning empty pick table"<<endl;
    }
    return result;
  }catch(...){throw;};
}
IndexTimes ThreeCEnsembleTimePicker::get_absolute_member_times()
{
  try{
    IndexTimes result;
    if(data_loaded)
    {

      int i;
      double t0,arrivaltime;
      vector<ThreeComponentSeismogram>::iterator dptr;
      for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++i,++dptr)
      {
        arrivaltime=dptr->get_double(TCPICKKEY);
        if(arrivaltime>null_pick_test)
        {
          /* This assumes time contains the utc time of the t0 reference
          point */
          t0=dptr->get_double("time");
          result.insert(pair<int,double>(i,t0+arrivaltime));
        }
      }
    }
    else
    {
      cerr << "ThreeCEnsembleTimePicker::get_absolute_member_times:  no data loaded"
        <<endl << "Returning empty pick table"<<endl;
    }
    return result;
  }catch(...){throw;};
}
StaTimes ThreeCEnsembleTimePicker::get_absolute_sta_times()
{
  try{
    StaTimes result;
    if(data_loaded)
    {

    int i;
    string sta;
    double t0,arrivaltime;
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++i,++dptr)
    {
      arrivaltime=dptr->get_double(TCPICKKEY);
      if(arrivaltime>null_pick_test)
      {
        sta=dptr->get_string("sta");
        t0=dptr->get_double("time");
        result.insert(pair<string,double>(sta,arrivaltime+t0));
      }
    }
  }
  else
  {
    cerr << "ThreeCEnsembleTimePicker::get_absolute_sta_times:  no data loaded"
      <<endl << "Returning empty pick table"<<endl;
  }
    return result;
  }catch(...){throw;};
}
int ThreeCEnsembleTimePicker::set_pick_times(vector<SeismicPick> p)
{
  try{
    PointPick pp;
    int i,k;
    int count;
    vector<SeismicPick>::iterator ptr;
    for(count=0,ptr=p.begin();ptr!=p.end();++ptr)
    {
      /* Silently drop picks not tagged as point poicks */
      if(ptr->type == POINT)
      {
        ++count;
        pp=ptr->get_point();
        i=ptr->get_trace_number();
        d0.member[i].put(TCPICKKEY,pp.time);
        for(k=0;k<3;++k)
            d[k].member[i].put(TCPICKKEY,pp.time);
      }
    }
    return count;
  }catch(...){throw;};
}
/* This algorithm has strong parallels to the get_times methods but
it does something more drastic as it alters the t0 variable (start time)
attribute for all the component plots.  The effect is to align all
three plots by the picks so that the phase being picked defines 0 for
the updated plots.   Absolute times are handled by shifting the attribute
time to be conssitent with the pick realignment.   Note we apply this to
the 3C ensemble as well as teh components so the reset method will not
be able tor remove these shifts after they are applied.  That could be
handled, but for now we do not.  If you want a full reset save
parent ensemble and just start over if you need a full reboot. */
void ThreeCEnsembleTimePicker::align()
{
  try{
    int i,k;
    for(i=0;i<nd;++i)
    {
      double t0s,arrivaltime;
      arrivaltime=d0.member[i].get_double(TCPICKKEY);
      /*Silently skip unpicked data*/
      if(arrivaltime<null_pick_test) continue;
      t0s=d0.member[i].get_double(t0shift_key);
      t0s+=arrivaltime;
      d0.member[i].put(t0shift_key,t0s);
      d0.member[i].t0-=arrivaltime;
      /* arrival time needs to now be reset to 0 */
      d0.member[i].put(TCPICKKEY,0.0);
      /* Do exactly the same thing to the components and call the plot method
      to display the changes */
      for(k=0;k<3;++k)
      {
        d[k].member[i].t0-=arrivaltime;
        d[k].member[i].put(t0shift_key,t0s);
        d[k].member[i].put(TCPICKKEY,0.0);
      }
    }
    for(k=0;k<3;++k)components[k].plot(d[k]);
  }catch(...){throw;};
}
void ThreeCEnsembleTimePicker::reset()
{
  try{
    int k;
    for(k=0;k<3;++k)
    {
      auto_ptr<TimeSeriesEnsemble> ptr;
      ptr=ExtractComponent(d0,k);
      d[k]=(*ptr);
      components[k].plot(d[k]);
    }
  }catch(...){throw;};
}
void ThreeCEnsembleTimePicker::refine_picks()
{
  try{
    int k;
    for(k=0;k<3;++k)
    {
      components[k].plot(d[k]);
    }
  }catch(...){throw;};
}
} // End SEISPP encapsulation
