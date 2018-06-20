#include <stdio.h>
#include <fstream>
#include <sstream>
#include "AttributeTextWriter.h"
AttributeTextWriter::AttributeTextWriter(string outdir, bool skipgaps)
{
    dir=outdir;
    killgaps=skipgaps;
    /* Test to be sure the directory is writable and throw and error if
     * it is not */
    int iac;
    iac=access(outdir.c_str(),W_OK);
    if(iac)
    {
        throw SeisppError(string("AttributeTextWriter constructor: ")
                + "cannot write data in directory=" + outdir
                + "\nMake sure directory exists and has write permission");
    }
}
int AttributeTextWriter::write(string fname, TimeSeries& d,
        TimeSeries& e)
{
    try{
        const string base_error("AttributeTextWriter write method:  ");
        /* For the current way the particle motion tools work d and
         * e must be the same length.  A simple sanity check */
        int ns=d.ns;
        if((d.ns)!=ns)
        {
            stringstream ss;
            ss << base_error << "Mismatched input data"<<endl
                << "Attribute vector length="<<ns<<endl
                << "Error vector length="<<e.ns<<endl
                << "These must be equal - coding error likely"<<endl;
            throw SeisppError(ss.str());
        }
        /* similar test for t0 */
        if((d.t0)!=(e.t0))
        {
            stringstream ss;
            ss << base_error << "Mismatched input data"<<endl
                << "Attribute vector t0="<<d.t0<<endl
                << "Error vector t0="<<e.t0<<endl
                << "These must be equal - coding error likely"<<endl;
            throw SeisppError(ss.str());
        }
        string fullpath;
        fullpath=dir + "/" + fname;
        ofstream ofs;
        ofs.open(fullpath.c_str(),std::ofstream::out);
        if(ofs.fail())
            throw SeisppError(base_error + "Cannot open file="
                    + fullpath);
        int i,count;
        for(i=0,count=0;i<ns;++i)
        {
            double t,lower,center,upper;
            if(d.is_gap(i) || e.is_gap(i)) continue;
            center=d[i];
            lower=d[i]-e[i];
            upper=d[i]+e[i];
            /* This order is native order to use errorlines method of gnuplot*/
            ofs<<d.time(i)<<" "<<center<<" "<<lower<<" "<<upper<<endl;
            ++count;
        }
        ofs.close();
        /* Silent remove the file if count is 0.  Assume caller will
         * issue an error message in this condition */
        if(count<=0) std::remove(fullpath.c_str());
        return count;
    }catch(...){throw;};
}
