#include "ThreeCEnsembleTimePicker.h"
#include "seiswplot.h"
using namespace std;
using namespace SEISPP;
namespace SEISPP
{
  /* This is the callback routine used for component 1 (0)*/
void pick_times_callback1(Widget w, void *cdata, void *udata)
{
    //DEBUG
    //cerr << "Entered pick_times_callback1"<<endl;
  ThreeCEnsembleTimePicker *tcp=reinterpret_cast<ThreeCEnsembleTimePicker *>(cdata);
  tcp->set_active_component(0);
  //vector<SeismicPick> picks= tcp->run_picker();
  /* This sets the TCPICKKEY fields of each seismogram with pick times.
  Made a procedure as this is common code to all 3 component callbacks. */
  //tcp->set_pick_times(picks);
}
/* This is the callback routine used for component 2 (1)*/
void pick_times_callback2(Widget w, void *cdata, void *udata)
{
    //DEBUG
    //cerr << "Entered pick_times_callback2"<<endl;
ThreeCEnsembleTimePicker *tcp=reinterpret_cast<ThreeCEnsembleTimePicker *>(cdata);
tcp->set_active_component(1);
}
/* This is the callback routine used for component 3 (2)*/
void pick_times_callback3(Widget w, void *cdata, void *udata)
{
    //DEBUG
    //cerr << "Entered pick_times_callback3"<<endl;
ThreeCEnsembleTimePicker *tcp=reinterpret_cast<ThreeCEnsembleTimePicker *>(cdata);
tcp->set_active_component(2);
//vector<SeismicPick> picks= tcp->run_picker();
/* This sets the TCPICKKEY fields of each seismogram with pick times.
Made a procedure as this is common code to all 3 component callbacks. */
//tcp->set_pick_times(picks);
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
  const string base_error("ThreeCEnsembleTimePicker Metadata driven constructor:");
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
    /* Make sure all picking is not active in current active window */
    switch(active_component)
    {
        case 0:
            if(comp0.picker_is_active())
                comp0.stop_picker();
            break;
        case 1:
            if(comp1.picker_is_active())
                comp1.stop_picker();
            break;
        case 2:
            if(comp1.picker_is_active())
                comp1.stop_picker();
    }
    active_component=ic;
    /* This assumes the Activate method is stateless and won't launch 
     * a new thread handler.   Current SeismicPlot does that. */
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
int ThreeCEnsembleTimePicker::plot(ThreeComponentEnsemble& din,
        bool initialize)
{
  try{
    d0=din;
    d0_original=din;
    pick_state.clear();
    pick_state.reserve(d0.member.size());
    /* Initialize pick field for all data.  We assume the components
    will inherit this value.  Current implementation does that, but that
    should not be considered a given. */
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=d0.member.begin();dptr!=d0.member.end();++dptr)
    {
      if(initialize) dptr->put(TCPICKKEY,null_pick);
      if(dptr->live)
      {
          pick_state.push_back(MOSTLY_DEAD_ALL_DAY);
      }
      else
          pick_state.push_back(ALLDEAD);
    }
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
/* The next several members are getters in various forms to 
 * fetch picks made so far.   This is seriously complicated by state
 * and earlier versions messed this up.   A key issue is these
 * members should ONLY be called after the align method has been called
 * or the most recent picks will not be used.
 *
 * The approach is to first ignore any member marked dead (!live).  
 * The secondary test is a null pick test.   A tertiary test against
 * pick state will spew errors for unexpected states under the
 * assumption these will need fixing and are a state I missed */
IndexTimes ThreeCEnsembleTimePicker::get_member_times()
{
  try{
    IndexTimes result;
    if(data_loaded)
    {
      int i;
      double pickdata, arrivaltime;
      vector<ThreeComponentSeismogram>::iterator dptr;
      for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++i,++dptr)
      {
        if(dptr->live)
        {
          pickdata=dptr->get_double(TCPICKKEY);
          if(pickdata>null_pick_test)
          {
            if(pick_state[i]!=PICKED)
            {
                cerr << "ThreeCEnsembleTimePicker::get_member_times"
                    << " WARNING:  member "<<i<<" does not have state "
                    << "marked PICKED but pick attributes are not null"
                    <<endl
                    << "This is a state problem that is a bug.  "
                    << "Report to author"<<endl;
            }
            arrivaltime=dptr->time_reference();
            /* If align hasn't been called we can get times 
             * as sum of arrivaltime+pickdata.  We assume that 
             * when align is called TCPICKEY references a 0 value */
            arrivaltime+=pickdata;
            result.insert(pair<int,double>(i,arrivaltime));
          }
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
    double pickdata, arrivaltime;
    vector<ThreeComponentSeismogram>::iterator dptr;
    if(data_loaded)
    {
      for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++i,++dptr)
      {
        if(dptr->live)
        {
          pickdata=dptr->get_double(TCPICKKEY);
          if(pickdata>null_pick_test)
          {
            sta=dptr->get_string("sta");
            if(pick_state[i]==PICKED)
            {
                cerr << "ThreeCEnsembleTimePicker::get_sta_times"
                    << " WARNING:  member "<<i<<" for station "
                    << sta<<" does not have state "
                    << "marked PICKED but pick attributes are not null"
                    <<endl
                    << "This is a state problem that is a bug.  "
                    << "Report to author"<<endl;
            }
            arrivaltime=dptr->time_reference();
            /* If align hasn't been called we can get times 
             * as sum of arrivaltime+pickdata.  We assume that 
             * when align is called TCPICKEY references a 0 value */
            arrivaltime+=pickdata;
            result.insert(pair<string,double>(sta,arrivaltime));
          }
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
int ThreeCEnsembleTimePicker::set_pick_times(vector<SeismicPick> p)
{
  try{
    PointPick pp;
    int i,k;
    int count;
    int nd=d0.member.size();
    vector<SeismicPick>::iterator ptr;
    for(count=0,ptr=p.begin();ptr!=p.end();++ptr)
    {
        //DEBUG
        //cout << "Pick number "<<count<<" has type="<<ptr->type<<endl;
      /* Silently drop picks not tagged as point poicks */
      if(ptr->type == POINT)
      {
        ++count;
        pp=ptr->get_point();
        i=ptr->get_trace_number();
        if(i>=0 && i<nd)
        {
            pick_state[i]=PICKED;
            d0.member[i].put(TCPICKKEY,pp.time);
            for(k=0;k<3;++k)
                d[k].member[i].put(TCPICKKEY,pp.time);
        }
        else
        {
            cerr << "Warning:  dropping eroneous pick with trace number "
                << i << " which is out of range 0 to "<<nd-1<<endl;
        }
        //DEBUG
        //cout << "Set pick for trace number "<<i<<" to "<<pp.time<<endl;
      }
    }
    return count;
  }catch(...){throw;};
}
/* This is a key method that should normally be called after any picks.
 * It permanantely alters t0 of each live seismogram and the t0shift variable.
 * It shifts t0 by lags stored
 * in TCPICKKEY, alters and resets t0shift, and then zeros the content of
 * the TCPICKKEY attribute.
 */
void ThreeCEnsembleTimePicker::align()
{
  try{
    int i,k;
    for(i=0;i<nd;++i)
    {
      /* Do nothing to anything marked dead */
      if(!d0.member[i].live) continue;
      double t0s,arrivaltime;
      arrivaltime=d0.member[i].get_double(TCPICKKEY);
      /*Silently skip unpicked data*/
      if(arrivaltime<null_pick_test) continue;
      t0s=d0.member[i].time_reference();
      t0s+=arrivaltime;
      d0.member[i].put(t0shift_key,t0s);
      d0.member[i].shift(arrivaltime);
      /* arrival time needs to now be reset to 0 */
      d0.member[i].put(TCPICKKEY,0.0);
      /* Do exactly the same thing to the components and call the plot method
      to display the changes */
      for(k=0;k<3;++k)
      {
        d[k].member[i].shift(arrivaltime);
        d[k].member[i].put(t0shift_key,t0s);
        d[k].member[i].put(TCPICKKEY,0.0);
      }
      pick_state[i]=PICKED;
    }
    /* This should be redundant, but can reduce probability of state 
     * problems */
    comp0.plot(d[0],false);
    comp1.plot(d[1],false);
    comp2.plot(d[2],false);
  }catch(...){throw;};
}
void ThreeCEnsembleTimePicker::align(vector<double>& lags)
{
    const string base_error("ThreeCEnsembleTimePicker::align(vector<double>& lags) method:  ");
    if(lags.size()!=nd)
        throw SeisppError(base_error
        + "vector of lags size is different from ensemble size being displayed");
    try{
        int i,k;
        double t0s;
        for(i=0;i<nd;++i)
        {
            if(d0.member[i].live)
            {
                if(lags[i]>null_pick_test)
                {
                    t0s=d0.member[i].time_reference();
                    t0s+=lags[i];
                    d0.member[i].put(t0shift_key,t0s);
                    d0.member[i].shift(lags[i]);
                    d0.member[i].put(TCPICKKEY,0.0);
                    for(k=0;k<3;++k)
                    {
                        d[k].member[i].shift(lags[i]);
                        d[k].member[i].put(t0shift_key,t0s);
                        d[k].member[i].put(TCPICKKEY,0.0);
                    }
                    pick_state[i]=PICKED;
                }
            }
        }
        comp0.plot(d[0],false);
        comp1.plot(d[1],false);
        comp2.plot(d[2],false);
    }catch(...){throw;};
}


void ThreeCEnsembleTimePicker::reset()
{
  try{
    int k;
    /* Note d0 is edited but d0_original is the unaltered original */
    d0=d0_original;
    pick_state.clear();
    vector<ThreeComponentSeismogram>::iterator dptr;
    for(dptr=d0.member.begin();dptr!=d0.member.end();++dptr)
    {
        if(dptr->live)
            pick_state.push_back(MOSTLY_DEAD_ALL_DAY);
        else
            pick_state.push_back(ALLDEAD);
    }
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
vector<SeismicPick>  ThreeCEnsembleTimePicker::run_picker()
{
    try{
        //DEBUG
        cerr<<"Entered ThreeCEnsembleTimePicker::run_picker()"<<endl;
        vector<SeismicPick> result;
        switch(active_component)
        {
            case 0:
                comp0.enable_blocking();
                result=comp0.pick_all();
                comp0.disable_blocking();
                break;
            case 1:
                comp1.enable_blocking();
                result=comp1.pick_all();
                comp1.disable_blocking();
                break;
            case 2:
                comp2.enable_blocking();
                result=comp2.pick_all();
                comp2.disable_blocking();
                break;
            default:
                cerr << "Illegal value for active_component="<<active_component<<endl
                    << "This should not happen - FATAL ERROR"<<endl;
                exit(-1);
        };
        return result;
    }catch(...){throw;};
}
int ThreeCEnsembleTimePicker::pick()
{
    set_active_component(active_component);
    vector<SeismicPick> picks=this->run_picker();
    int nset;
    nset=this->set_pick_times(picks);
    return nset;
} 
ThreeComponentEnsemble ThreeCEnsembleTimePicker::picked_data()
{
    /* Because we are storing the picks in the headers of d0 this
     * routine is trivial.   Could have put it in the include file
     * but it is possible this might change so thought it better to
     * put it in the cc file */
    return d0;
}
void ThreeCEnsembleTimePicker::kill_unpicked()
{
    int i,j,k;
    vector<ThreeComponentSeismogram>::iterator d0ptr;
    /* This loop trusts components in d array are the same size
     * as d0 */
    for(d0ptr=d0.member.begin(),i=0;d0ptr!=d0.member.end();++d0ptr,++i)
    {
        double arrivaltime;
        arrivaltime=d0ptr->get_double(TCPICKKEY);
        //DEBUG
        //cout << "kill_unpicked: i="<<i<<" atime="<<arrivaltime<<" state="<<pick_state[i]<<endl;
        /* This is true if the pick is null  - reverse numeric test of
         * every other occurence like this. Assumes null_pick_test is
         * a large positive number.*/
        if(arrivaltime<=null_pick_test)
        {
            switch(pick_state[i])
            {
                case MOSTLY_DEAD_ALL_DAY:
                case ALLDEAD:
                    /* This is redundant, for DEAD case but small cost for
                     * safety.  Notice lack of break makes this executed
                     * for MOSTLY_DEAD_ALL_DAY case too. */
                    d0ptr->live=false;
                    for(k=0;k<3;++k)
                        d[k].member[i].live=false;
                    break;
                case PICKED:
                    d0ptr->live=true;
                    for(k=0;k<3;++k)
                        d[k].member[i].live=true;
            };
        }
    }
    comp0.plot(d[0],false);
    comp1.plot(d[1],false);
    comp2.plot(d[2],false);
}
 void ThreeCEnsembleTimePicker::mark_all_live_picked()
{
    /* Silently do nothing if data are not loaded */
    if(data_loaded)
    {
        int i,k;
        vector<ThreeComponentSeismogram>::iterator dptr;
        for(i=0,dptr=d0.member.begin();dptr!=d0.member.end();++dptr,++i)
        {
            if(dptr->live)
            {
                pick_state[i]=PICKED;
                dptr->put(TCPICKKEY,0.0);
                for(k=0;k<3;++k)
                    d[k].member[i].put(TCPICKKEY,0.0);
            }
        }
    }
}
void ThreeCEnsembleTimePicker::resurrect()
{
    int i,j,k;
    /* We assume d0 and the components are all the same size */
    for(i=0;i<d0.member.size();++i)
    {
        if(pick_state[i]==MOSTLY_DEAD_ALL_DAY) 
        {

            d0.member[i].live=true;
            for(k=0;k<3;++k)
                d[k].member[i].live=true;
        }
    }
    comp0.plot(d[0]);
    comp1.plot(d[1]);
    comp2.plot(d[2]);
}

} // End SEISPP encapsulation
