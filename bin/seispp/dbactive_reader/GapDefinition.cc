#include <iostream>
#include <fstream>
#include <istream>
#include "GapDefinition.h"
/* This is a silly convenience function to get around an antelope api
   problem with a const problem */
double stringtotime(string timestring)
{
    double t;
    t=str2epoch(const_cast<char *>(timestring.c_str()));
    return t;
}
/* We use this procedure to allow read data in as good periods not
bad periods. */
vector<TimeWindow> negate_time_windows(vector<TimeWindow>& twv)
{
    vector<TimeWindow> result;
    int ntw=twv.size();
    //DEBUG
    //cout << "size of twv in negate_time_windows="<<ntw<<endl;
    result.reserve(ntw);
    TimeWindow work;
    /* The following loop does not check the time windows are sequential and
       not overlapping.  If this code is ever made more generic fix that */
    work.start=0.0;  // Start window at epoch 0 time
    int i;
    for(i=0;i<ntw;++i)
    {
        work.end=twv[i].start;
        //DEBUG
        /*
        cout <<"Defining gap in interval="<<strtime(work.start)
            <<" to "<<strtime(work.end)<<endl;
            */
        result.push_back(work);
        work.start=twv[i].end;
    }
    work.end=stringtotime("2020001:00:00:00");  // arbitrary time in future
    result.push_back(work);
    return result;
}
GapDefinition::GapDefinition(string fname)
{
    string base_error("GapDefinition constructor: ");
    try{
        ifstream ifs;
        ifs.open(fname.c_str());
        if(ifs.fail())throw SeisppError(base_error
                + "Open failed for filename="+fname);
        string sta,laststa;
        string tsstr,testr;
        vector<TimeWindow> work,workinv;
        int i(0);
        do
        {
            TimeWindow twread;
            ifs >> sta;  ifs>>tsstr;  ifs>> testr;
            //DEBUG
            /*
            cout << sta<< " read times "
                << tsstr << " "<<testr<<endl;
                */
            /* A bit confusing because the cleanup block for the end of the file
               is at the top of the conditional chain.  */
            if(ifs.eof())
            {
                /* Cleanup block for last station*/
                workinv=negate_time_windows(work);
                gapmap.insert(pair<string,vector<TimeWindow> >
                        (laststa,workinv));
            }
            else
            {
                if(i==0)
                {
                    laststa=sta;
                    twread=TimeWindow(stringtotime(tsstr.c_str()),stringtotime(testr.c_str()));
                    work.push_back(twread);
                }
                else if(laststa==sta)
                {
                    twread=TimeWindow(stringtotime(tsstr.c_str()),stringtotime(testr.c_str()));
                    work.push_back(twread);
                }
                else
                {
                    // the time windows need to be turned around (we input good sections not bad)
                    workinv=negate_time_windows(work);
                    gapmap.insert(pair<string,vector<TimeWindow> >
                            (laststa,workinv));
                    work.clear();
                    twread=TimeWindow(stringtotime(tsstr.c_str()),
                            stringtotime(testr.c_str()));
                    work.push_back(twread);
                    laststa=sta;
                }
            }
            ++i;
        }while(!ifs.eof());
    }catch(...){throw;};
}


vector<TimeWindow> GapDefinition::get(string staname)
{
    map<string,vector<TimeWindow> >::iterator mptr;
    mptr=gapmap.find(staname);
    if(mptr!=gapmap.end())
        return mptr->second;
    else
    {
        /* If a station is not defined we assume we don't want
           any data from it at all */
        vector<TimeWindow> empty;
        TimeWindow twall(0.0,999999999999.9);  //all epoch time is a gap
        empty.push_back(twall);
        return empty;
    }
}
