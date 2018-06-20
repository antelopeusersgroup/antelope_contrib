#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <fstream>
#include <float.h>
#include "seispp.h"
#include "seispp_io.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
#include "ensemble.h"
#include "interpolator1d.h"
using namespace std;
using namespace SEISPP;

void usage()
{
    cerr << "agc windlength [-text -g gainfunction] < infile > outfile"
        <<endl
        << "Applies three-component AGC operator of duraction winlength second"
        <<endl
        << "Optionally save ensemble of gain function as TimeSeriesEnsemble  object with winlength/2 sample interval"
        <<endl;
    exit(-1);
}

/* This uses the same algorithm as seismic unix BUT with a vector ssq 
 * instead of the scalar form.   Returns a gain function a the same sample
 * rate as teh original data with the gain factor applied to each 3c sample.
 * The gain is averaged over scale twin ramping on and off using the same
 * cumulative approach used in seismic unix algorithm. */
TimeSeries do_agc(ThreeComponentSeismogram& d, double twin)
{
    try{
        dmatrix agcdata(3,d.ns);
        double val,rms,ssq,gain,lastgain;
        int i,k,iw,iwbreak;
        TimeSeries gf(dynamic_cast<Metadata&>(d),false);
        gf.t0=d.t0+gf.dt;
        gf.s.clear();
        int nwin,iwagc;
        nwin=nint(twin/(d.dt));
        iwagc=nwin/2;
        if(iwagc<=0) throw SeisppError("do_agc:  illegal time window - resolves to less than one sample");
        /* this shouldn't happen but avoids a seg fault for a dumb input */
        if(iwagc>d.ns) iwagc=d.ns;
        /* First compute sum of squares in initial wondow to establish the
         * initial scale */
        for(i=0,ssq=0.0;i<iwagc;++i)
        {
            for(k=0;k<3;++k)
            {
                val=d.u(k,i);
                ssq+=val*val;
            }
        }
        int normalization;
        normalization=3*iwagc;
        rms=ssq/((double)normalization);
        if(rms>0.0)
        {
            gain=1.0/sqrt(rms);
            for(k=0;k<3;++k) 
            {
                agcdata(k,0) = gain*d.u(k,0);
            }
            gf.s.push_back(gain);
        }
        else
        {
            gf.s.push_back(0.0);
            lastgain=0.0;
        }
        //DEBUG
        //cout <<endl<< "i="<<i<<" gain="<<gain<<endl;
        /* Ramping on */
        //DEBUG
        //cout << "Ramping on section"<<endl;
        for(i=1;i<=iwagc;++i)
        {
            for(k=0;k<3;++k)
            {
                val=d.u(k,i+iwagc);
                ssq+=val*val;
                ++normalization;
//DEBUG
//cout << "val="<<val<<" ssq="<<ssq<<"normalization="<<normalization<<endl;
            }
            rms=ssq/((double)normalization);
            if(rms>0.0) 
            {
                lastgain=gain;
                gain=1.0/sqrt(rms);
            }
            else
            {
                if(lastgain==0.0)
                    gain=0.0;
                else
                    gain=lastgain;

            }
            gf.s.push_back(gain);
            lastgain=gain;
            for(k=0;k<3;++k) agcdata(k,i) = gain*d.u(k,i);
        //DEBUG
        //cout << "i="<<i<<" rms="<<rms<<" gf.s[i]="<<gf.s[i]<<endl;
        }
        /* mid range - full rms window */
        //DEBUG
        //cout << "Mid range section"<<endl;
        int isave;
        for(i=iwagc+1,isave=iwagc+1;i<d.ns-iwagc;++i,++isave)
        {
           for(k=0;k<3;++k)
           {
               val=d.u(k,i+iwagc);
               ssq+=val*val;
//DEBUG
//cout << "Add number at i+iwagc="<<i+iwagc<<endl;
//cout << "val="<<val<<" ssq="<<ssq<<"normalization="<<normalization<<endl;
               val=d.u(k,i-iwagc);
               ssq-=val*val;
//DEBUG
//cout << "Subtract number at i-iwagg="<<i-iwagc<<endl;
//cout << "val="<<val<<" ssq="<<ssq<<"normalization="<<normalization<<endl;
           }
           rms=ssq/((double)normalization);
            if(rms>0.0) 
            {
                lastgain=gain;
                gain=1.0/sqrt(rms);
            }
            else
            {
                if(lastgain==0.0)
                    gain=0.0;
                else
                    gain=lastgain;

            }
            gf.s.push_back(gain);
            lastgain=gain;
            for(k=0;k<3;++k) agcdata(k,i) = gain*d.u(k,i);
        //DEBUG
        //cout << "i="<<i<<" rms="<<rms<<" gf.s[i]="<<gf.s[i]<<endl;
        }
        //DEBUG
        //cout << "Ramping off data"<<endl;
        /* ramping off */
        for(i=isave;i<d.ns;++i)
        {
            for(k=0;k<3;++k)
            {
                val=d.u(k,i-iwagc);
                ssq -= val*val;
                --normalization;
//DEBUG
//cout << "Subtract number at i="<<i<<endl;
//cout << "val="<<val<<" ssq="<<ssq<<"normalization="<<normalization<<endl;
            }
            rms=ssq/((double)normalization);
            if(rms>0.0) 
            {
                lastgain=gain;
                gain=1.0/sqrt(rms);
            }
            else
            {
                if(lastgain==0.0)
                    gain=0.0;
                else
                    gain=lastgain;

            }
            gf.s.push_back(gain);
            lastgain=gain;
            for(k=0;k<3;++k) agcdata(k,i) = gain*d.u(k,i);
        //DEBUG
        //cout << "i="<<i<<" rms="<<rms<<" gf.s[i]="<<gf.s[i]<<endl;
        }
        d.u=agcdata;
        gf.live=true;
        gf.ns=gf.s.size();
        return gf;
    }catch(...){throw;};
}

bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{

    int i;
    const int narg_required(1);
    if(argc<2) usage();
    double agcwinlen=atof(argv[1]);
    bool save_gain_function(false);
    string gainfile;
    bool binary_data(true);

    for(i=narg_required+1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="-g")
        {
            ++i;
            if(i>=argc)usage();
            gainfile=string(argv[i]);
            save_gain_function=true;
        }
        if(sarg=="-text")
            binary_data=false;
        else
            usage();
    }
    try{
        shared_ptr<StreamObjectReader<ThreeComponentEnsemble>> ia;
        if(binary_data)
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>('b'));
        }
        else
        {
          ia=shared_ptr<StreamObjectReader<ThreeComponentEnsemble>>
             (new StreamObjectReader<ThreeComponentEnsemble>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>> oa;
        if(binary_data)
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>('b'));
        }
        else
        {
          oa=shared_ptr<StreamObjectWriter<ThreeComponentEnsemble>>
             (new StreamObjectWriter<ThreeComponentEnsemble>);
        }
        ThreeComponentEnsemble d;
        d=ia->read();
        int n=d.member.size();
        TimeSeriesEnsemble gains(dynamic_cast<Metadata&>(d),n);
        int i;
        for(i=0;i<n;++i)
        {
            TimeSeries g;
            g=do_agc(d.member[i],agcwinlen);
            gains.member.push_back(g);
            //cout << g;
        }
        oa->write(d);
        if(save_gain_function)
        {
          try{
            /* These are always written as a text file for now*/
            StreamObjectWriter<TimeSeriesEnsemble> ofs(gainfile);
            ofs.write(gains);
          }catch(SeisppError& serr)
          {
              cerr << "Writer for gain function to file="<<gainfile
                  << " failed."<<endl<<"SeisppError message posted follow:"
                  <<endl;
              serr.log_error();
          }
        }
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}
