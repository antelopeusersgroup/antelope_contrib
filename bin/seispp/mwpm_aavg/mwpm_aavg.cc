#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <string>
#include <iostream>
#include <vector>
#include <memory>
#include "minicsv.h"
#include "Vector3DBootstrapError.h"
using namespace std;   
//using namespace SEISPP;
void usage()
{
    cerr << "mwpm_aavg infile outfile [-band n -v --help]"
        <<endl
        << "Reads output of mwpmavg and computes array average particle motion"
        <<endl
        << "Key used to define grouping is the tag attribute (first entry in each input line)"
        <<endl
        << "infile - csv file output of mwpmavg (primary input file)"<<endl
        << "outfile - csv output file with attributes parallel to infile"<<endl
        << " -band - only average data from band n (default averages all)"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl;
    exit(-1);
}
bool FileExists(const std::string& file) {
  struct stat buf;
  return (stat(file.c_str(), &buf) == 0);
}
typedef vector<double> Vec;
typedef pair<double,double> Stats;
/* Returns weighted mean sigma estimate from vector x assuming dx
   contains estimate of uncertainty of each x */
Stats weighted_mean(Vec& x,Vec& dx)
{
  int i;
  int nx=x.size();
  double wt,sumwt,sumwx;
  /* The formula used to estimate uncertainty may be in error here. 
     Check theorical basis before use */
  for(sumwt=0.0,sumwx=0.0,i=0;i<nx;++i)
  {
    wt=1.0/dx[i];
    sumwx += wt*x[i];
    sumwt += wt;
  }
  double dxmean;
  dxmean=sqrt((double)nx)/sumwt;
  Stats result;
  result.first=sumwx/sumwt;
  result.second=dxmean;
  return result;
}
int process(Vec& majornrm,Vec& dmajornrm,Vec& majoraz,Vec& dmajoraz,
  Vec& majorinc,Vec& dmajorinc,Vec& minornrm,Vec& dminornrm,Vec& minoraz,
  Vec& dminoraz,Vec& minorinc,Vec& dminorinc,
  Vec& rect,Vec& drect,Vec& major3c,Vec& minor3c,const string outfile)
{
  ofstream ofs(outfile.c_str(),std::ofstream::out);
  if(!ofs.is_open())
  {
    cerr << "mwpm_aavg:  open failed for output file="<<outfile<<endl;
    usage();
  }
  Stats s;
  /* Note the amplitudes are in original units but error estimates
     for amplitudes are in db.  Hence, we have to convert amps to db
     before computing averages and then convert them back for consistent
     output */
  int nx;
  nx=majornrm.size();  // We can assume all vectors are this length 
  int i;
  for(i=0;i<nx;++i)
  {
    majornrm[i]=20.0*log10(majornrm[i]);
    minornrm[i]=20.0*log10(minornrm[i]);
  }
  s=weighted_mean(majornrm,dmajornrm);
  s.first=pow(10.0,(s.first)/20.0);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  s=weighted_mean(majorinc,dmajorinc);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  s=weighted_mean(minornrm,dminornrm);
  s.first=pow(10.0,(s.first)/20.0);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  s=weighted_mean(minoraz,dminoraz);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  s=weighted_mean(minoraz,dminoraz);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  s=weighted_mean(rect,drect);
  ofs << s.first << ","<<s.second<<",";
  s=weighted_mean(majoraz,dmajoraz);
  /* We compute the vectors using the boostrap method by mwpm */
  dmatrix x3c(3,nx);
  int ii;
  for(i=0,ii=0;i<nx;++i,ii+=3)
  {
    x3c(0,i)=major3c[ii];
    x3c(1,i)=major3c[ii+1];
    x3c(2,i)=major3c[ii+2];
  }
  /* The default confidence interval for PMTimeSeries estimates
     with multiwavelets is 95% at the time I wrote this.  Will set
     the confidence to that value but be aware this could become
     inconsistent with other error estimates for different
     confidence values. Number of trials is a bit arbitrary but 
     has an implicit assumption nx is of the order of 10.  This 
     calculation is so simple overkill is preferable to undersampling */
  Vector3DBootstrapError majerr(x3c,0.95,nx*1000);
  Vec mv=majerr.mean_vector();
  for(i=0;i<3;++i) ofs<<mv[i]<<",";
  ofs<<majerr.angle_error()<<",";
  /* Repeat for minor - a bit repetitious but most of this procedure 
     would profit from a bit of consolidation.  */
  for(i=0,ii=0;i<nx;++i,ii+=3)
  {
    x3c(0,i)=minor3c[ii];
    x3c(1,i)=minor3c[ii+1];
    x3c(2,i)=minor3c[ii+2];
  }
  /* Vector3DBootstrapError currently lacks operator= so will
     create a new copy for minor */
  Vector3DBootstrapError minorerr(x3c,0.95,nx*1000);
  mv=minorerr.mean_vector();
  for(i=0;i<3;++i) ofs<<mv[i]<<",";
  ofs<<minorerr.angle_error()<<",";
  /* The last column of mwpm contains a comparable count variable */
  ofs<<nx<<endl;
  ofs.close();
  return nx;
}
//bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i,k;
    if(argc<3) usage();
    string infile(argv[1]);
    string outfile(argv[2]);
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool average_all(true);
    int band_to_use(0);
    bool Verbose;
    for(i=3;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-band")
        {
          ++i;
          if(i>=argc) usage();
          average_all=false;
          band_to_use=atoi(argv[i]);
        }
        else if(sarg=="-v")
          Verbose=true;
        else
            usage();
    }
    try{
      csv::ifstream ifs(infile.c_str());
      if(!ifs.is_open())
      {
        cerr << "Cannot open input file="<<infile<<endl;
        usage();
      }
      if(FileExists(outfile))
      {
        cerr << "Requested output file="<<outfile<<endl
          << "exists.   Will not overwrite - exiting"<<endl;
        usage();
      }
      csv::ofstream ofs(outfile.c_str());
      if(!ofs.is_open())
      {
        cerr << "Open failed for output file="<<outfile<<endl;
        usage();
      }
      string tag,lasttag;
      int band;
      int lines_read(0),lines_used(0);
      vector<double> majornrm,dmajornrm,majoraz,dmajoraz,majorinc,dmajorinc;
      vector<double> minornrm,dminornrm,minoraz,dminoraz,minorinc,dminorinc;
      vector<double> rect,drect;
      double dval;
      /* These are used as buffers to hold major and minor 3 vectors 
         before converting them to dmatrix used as input to bootstrap
         routine */
      vector<double> major3c, minor3c; 
      int count;
      while(ifs.read_line())
      {
        ifs>>tag;
        ifs>>band;
        if(lines_used<=0) lasttag=tag;
        if(average_all || (band==band_to_use))
        {
          ++lines_used;
          if(tag!=lasttag)
          {
            /* In this condition we compute averages and write
               results. Writing the tag and band first is a bit
               awkward, but the arg list to the process procedure
               is ridiculous already */
            ofs << tag;
            ofs << band;
            count=process(majornrm,dmajornrm,majoraz,dmajoraz,
                majorinc,dmajorinc,minornrm,dminornrm,minoraz,
                dminoraz,minorinc,dminorinc,rect,drect,major3c,minor3c,
                outfile);
            if(Verbose)
            {
              cout << "Processed data from ensemble with tag = "<<tag
                << " with "<<count<<" particle motion estimates"<<endl;
            }
          }
          /* Always read the next available line */
          ifs>>dval;
          majornrm.push_back(dval);
          ifs>>dval;
          dmajornrm.push_back(dval);
          ifs>>dval;
          majoraz.push_back(dval);
          ifs>>dval;
          dmajoraz.push_back(dval);
          ifs>>dval;
          majorinc.push_back(dval);
          ifs>>dval;
          dmajorinc.push_back(dval);
          ifs>>dval;
          minornrm.push_back(dval);
          ifs>>dval;
          dminornrm.push_back(dval);
          ifs>>dval;
          minoraz.push_back(dval);
          ifs>>dval;
          dminoraz.push_back(dval);
          ifs>>dval;
          minorinc.push_back(dval);
          ifs>>dval;
          dminorinc.push_back(dval);
          ifs>>dval;
          rect.push_back(dval);
          ifs>>dval;
          drect.push_back(dval);
          for(k=0;k<3;++k)
          {
            ifs>>dval;
            major3c.push_back(dval);
          }
          /* Skip the error term */
          ifs>>dval;
          for(k=0;k<3;++k)
          {
            ifs>>dval;
            minor3c.push_back(dval);
          }
        }
        ++lines_read;
    }
        /*
    catch(SeisppError& serr)
    {
        serr.log_error();
    }
    */
    }catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

