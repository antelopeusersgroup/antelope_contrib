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
    //DEBUG
    cerr << "Entered pick_times_callback1"<<endl;
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
    //DEBUG
    cerr << "Entered pick_times_callback2"<<endl;
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
    //DEBUG
    cerr << "Entered pick_times_callback3"<<endl;
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
      switch(k)
      {
          case 0:
            this->pick_menu[k]=BuildMenu(comp0.menu_bar,
                    XmMENU_PULLDOWN,(char *)"Pick Times",'P',false,pickdata);
            XtManageChild(comp0.menu_bar);
            break;
          case 1:
            this->pick_menu[k]=BuildMenu(comp1.menu_bar,
                    XmMENU_PULLDOWN,(char *)"Pick Times",'P',false,pickdata);
            XtManageChild(comp1.menu_bar);
            break;
          case 2:
            this->pick_menu[k]=BuildMenu(comp2.menu_bar,
                    XmMENU_PULLDOWN,(char *)"Pick Times",'P',false,pickdata);
            XtManageChild(comp2.menu_bar);
      };
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
    this->build_pick_menu();
    active_component=0;
    //this->set_active_component(0);
  }catch(...){throw;};
}
ThreeCEnsembleTimePicker::ThreeCEnsembleTimePicker(Metadata md)
    : comp0(md), comp1(md), comp2(md)
{
    //DEBUG
    cerr << "Entered ThreeCEnsembleTimePicker constructor"<<endl;
  data_loaded=false;
  nd=0;
  try{
    int k;
    char title[50];
    for(k=0;k<3;++k)
    {
      sprintf(title,"ThreeCEnsemblePicker:  component %d",k);
      switch(k)
      {
          case 0:
              comp0.put("title",title);
              break;
          case 1:
              comp1.put("title",title);
              break;
          case 2:
              comp2.put("title",title);
      };
    }
    this->build_pick_menu();
    try{
        active_component=md.get_int("initial_active_component");
    }catch(...)
    {
        active_component=0;
    }
    //this->set_active_component(active_component);
  }catch(...){throw;};
}
ThreeCEnsembleTimePicker::~ThreeCEnsembleTimePicker()
{
    //DEBUG
    cerr << "In Destructor for ThreeCEnsembleTimePicker"<<endl;
    comp0.ExitDisplay();
    comp1.ExitDisplay();
    comp2.ExitDisplay();
}
void ThreeCEnsembleTimePicker::set_active_component(int ic)
{
    /* Make sure all the windows event handler threads are killed */
    //comp0.ExitDisplay();
    //comp1.ExitDisplay();
    //comp2.ExitDisplay();
    active_component=ic;
    switch(active_component)
    {
        case 0:
            comp0.Activate();
            break;
        case 1:
            comp1.Activate();
            break;
        case 2:
            comp2.Activate();
    }
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
      switch(k)
      {
          case 0:
              comp0.plot(d[k],false);
              break;
          case 1:
              comp1.plot(d[k],false);
              break;
          case 2:
              comp2.plot(d[k],false);
      };
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
    comp0.plot(d[0]);
    comp1.plot(d[1]);
    comp2.plot(d[2]);
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
      switch(k)
      {
          case 0:
              comp0.plot(d[0]);
              break;
          case 1:
              comp1.plot(d[1]);
              break;
          case 2:
              comp2.plot(d[2]);
      };
    }
  }catch(...){throw;};
}
vector<SeismicPick>  ThreeCEnsembleTimePicker::pick_times()
{
    try{
        //DEBUG
        cerr<<"Entered ThreeCEnsembleTimePicker::pick_times()"<<endl;
        vector<SeismicPick> result;
        switch(active_component)
        {
            case 0:
                comp0.enable_blocking();
                result=comp0.pick_all();
                break;
            case 1:
                comp1.enable_blocking();
                result=comp1.pick_all();
                break;
            case 2:
                comp2.enable_blocking();
                result=comp2.pick_all();
                break;
            default:
                cerr << "Illegal value for active_component="<<active_component<<endl
                    << "This should not happen - FATAL ERROR"<<endl;
                exit(-1);
        };
        return result;
    }catch(...){throw;};
}
void ThreeCEnsembleTimePicker::refine_picks()
{
    set_active_component(active_component);
    vector<SeismicPick> picks=this->pick_times();
    this->set_pick_times(picks);
} 
} // End SEISPP encapsulation
