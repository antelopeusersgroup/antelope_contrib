#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <memory>
#include "seispp.h"
#include "ThreeComponentSeismogram.h"
#include "StreamObjectReader.h"
#include "StreamObjectWriter.h"
using namespace std;   
using namespace SEISPP; 
void usage()
{
    cerr << "transform3C < in > out [-force_cardinal -pf pffile -v --help -text]"
        <<endl
        << "Applies a fixed transformation matrix to a file of serialized"
        << endl
        << "ThreeComponentSeismogram objects.  The transformation matrix is"
        <<endl
        << "defined in parameter file (default transform3C.pf)."
        << " -force_cardinal - when this appears the transformation is "<<endl
        << "   treated as definitive to fix metadata errors.  The objects"<<endl
        << "   internal transformation matrix is set to an identity matrix"<<endl
        << "   Default multiplies input transformation with current matrix"<<endl
        << " -pf - uses pffile instead of default transform3C.pf"<<endl
        << " -v - be more verbose"<<endl
        << " --help - prints this message"<<endl
        << " -text - switch to text input and output (default is binary)"<<endl;
    exit(-1);
}
void reset_tmatrix(ThreeComponentSeismogram& d)
{
    d.components_are_cardinal=true;
    d.components_are_orthogonal=true;
    for(int i=0;i<3;++i)
        for(int j=0;j<3;++j)
        {
            if(i==j)
                d.tmatrix[i][j]=1.0;
            else
                d.tmatrix[i][j]=0.0;
        }
}
bool SEISPP::SEISPP_verbose(false);
int main(int argc, char **argv)
{
    int i;
    if(argc>1)
      if(string(argv[1])=="--help") usage();
    bool binary_data(true);
    bool force_cardinal(false);
    string pffile("transform3C.pf");

    for(i=1;i<argc;++i)
    {
        string sarg(argv[i]);
        if(sarg=="--help")
        {
            usage();
        }
        else if(sarg=="-pf")
        {
            ++i;
            if(i>=argc)usage();
            pffile=string(argv[i]);
        }
        else if(sarg=="-text")
        {
            binary_data=false;
        }
        else if(sarg=="-force_cardinal")
        {
            force_cardinal=true;
        }
        else if(sarg=="-v")
          SEISPP_verbose=true;
        else
            usage();
    }
    try{
        PfStyleMetadata control=pfread(pffile);
        list<string> matdata;
        matdata=control.get_tbl("Transformation_Matrix");
        if(matdata.size() != 3)
        {
            cerr << "transform3C:  error in TransformationMatrix Tbl in pf"
                << " file="<<pffile<<endl
                << "Size of list parsed = "<<matdata.size()<<endl
                << "Must be exactly 3 to define a 3x3 matrix"<<endl;
        }
        /* parse this list inline */
        double A[3][3];
        int k;
        list<string>::iterator mptr;
        for(k=0,mptr=matdata.begin();k<3;++k,++mptr)
        {
            double a1,a2,a3;
            sscanf(mptr->c_str(),"%lf%lf%lf",&a1,&a2,&a3);
            A[k][0]=a1;
            A[k][1]=a2;
            A[k][2]=a3;
        }

        shared_ptr<StreamObjectReader<ThreeComponentSeismogram>> inp;
        if(binary_data)
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
             (new StreamObjectReader<ThreeComponentSeismogram>('b'));
        }
        else
        {
          inp=shared_ptr<StreamObjectReader<ThreeComponentSeismogram>>
             (new StreamObjectReader<ThreeComponentSeismogram>);
        }
        shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>> out;
        if(binary_data)
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>('b'));
        }
        else
        {
          out=shared_ptr<StreamObjectWriter<ThreeComponentSeismogram>>
             (new StreamObjectWriter<ThreeComponentSeismogram>);
        }
        ThreeComponentSeismogram d;
        int n(0);
        while(inp->good())
        {
            d=inp->read();
            d.rotate_to_standard();
            d.apply_transformation_matrix(A);
            if(force_cardinal) reset_tmatrix(d);
            out->write(d);
            ++n;
        }
        cerr << "Total number of ensembles copied ="<<n<<endl;
    }catch(SeisppError& serr)
    {
        serr.log_error();
    }
    catch(std::exception& stexc)
    {
        cerr << stexc.what()<<endl;
    }
}

