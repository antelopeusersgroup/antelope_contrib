#include <fstream>
#include <boost/config.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include "Metadata.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
/* This procedure probably should be added to mdlist.cc.  I writes the contents of a MetadataList to 
   stdout */
void print_mdlist(MetadataList& mdl)
{
    MetadataList::iterator mdlptr;
    for(mdlptr=mdl.begin();mdlptr!=mdl.end();++mdlptr)
    {
        cout << mdlptr->tag;
        switch(mdlptr->mdt)
        {
            case MDreal:
                cout << " is real attribute";
                break;
            case MDint:
                cout << " is integer attribute";
                break;
            case MDboolean:
                cout << " is boolean attribute";
                break;
            case MDstring:
                cout << " is string attribute";
                break;
            default:
                cout << " has unrecognized type";
        }
        cout << endl;
    }
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    string pfname("test_tcs");
    string amname="css3.0";
    int i;
    int ierr;

    Pf *pf;
    ios::sync_with_stdio();

    if(pfread(const_cast<char *>(pfname.c_str()),&pf))
    {
           cerr << "Pfread error"<<endl;
           exit(-1);
    }
   try
   {
	int i,j;
	cout << "Testing simple constructors" << endl;
	ThreeComponentSeismogram s1;
        cout << "Success"<<endl;
        cout << "Testing space allocating constructor"<<endl;
	ThreeComponentSeismogram s2(100);
        cout << "Success"<<endl;
	// Initialize the contents of s2 and apply a rotation 
	// matrix
	cout << "trying rotation" << endl;
	for(i=0;i<3;++i)
		for(j=0;j<100;++j)
		{
			if(i==2) s2.u(i,j)=1.0;
			else
				s2.u(i,j)=0.0;
		}
        s2.live=true;
	SphericalCoordinate sc;
	sc.phi=0.0;
	sc.theta=M_PI_4;
	s2.rotate(sc);
	cout << "one sample of (0,0,1) rotated by phi=0, theta=45 deg:"
		<< s2.u(0,0) << ", "
		<< s2.u(1,0) << ", "
		<< s2.u(2,0) << endl;
	cout << "Restoring to standard" << endl;
	s2.rotate_to_standard();
	cout << "One sample of restored (0,0,1): "
		<< s2.u(0,0) << ", "
		<< s2.u(1,0) << ", "
		<< s2.u(2,0) << endl;
        cout << "Trying theta=45 deg and phi=-45 deg "<<endl;
        sc.phi=-M_PI_4;
        double *nu1=SphericalToUnitVector(sc);
        s2.u(0,0)=nu1[0];  s2.u(1,0)=nu1[1];  s2.u(2,0)=nu1[2];
        delete [] nu1;
        s2.rotate(sc);
	cout << "one sample of unit vector pointing  phi=-45, theta=45 deg after rotation:  "
		<< s2.u(0,0) << ", "
		<< s2.u(1,0) << ", "
		<< s2.u(2,0) << endl;
        cout << "Should be approximately (0,0,1)"<<endl;
	cout << "Restoring to standard" << endl;
	s2.rotate_to_standard();
	cout << "One sample of restored phi-45 theta=45 vector:  "
		<< s2.u(0,0) << ", "
		<< s2.u(1,0) << ", "
		<< s2.u(2,0) << endl;

	ThreeComponentSeismogram s3(100);
	for(i=0;i<3;++i)
		for(j=0;j<100;++j)
		{
			if(i==0) s3.u(i,j)=1.0;
			else
				s3.u(i,j)=0.0;
		}
        s3.live=true;
	// set some other vectors used in test below
	s3.u(0,1)=1.0;
	s3.u(1,1)=1.0;
	s3.u(2,1)=1.0;

	s3.u(0,2)=1.0;
	s3.u(1,2)=1.0;
	s3.u(2,2)=0.0;

	s3.u(0,3)=0.0;
	s3.u(1,3)=0.0;
	s3.u(2,3)=1.0;

	cout << "Trying unit vector rotation to (1,1,1) with (1,0,0) data"<<endl;
	double nu[3]={sqrt(3.0)/3.0,sqrt(3.0)/3.0,sqrt(3.0)/3.0};
	s3.rotate(nu);
	cout << "one sample of (1,0,0) rotated to (1,1,1)"
		<< s3.u(0,0) << ", "
		<< s3.u(1,0) << ", "
		<< s3.u(2,0) << endl;
	cout << "one sample of (1,1,1) rotated to (1,1,1)"
		<< s3.u(0,1) << ", "
		<< s3.u(1,1) << ", "
		<< s3.u(2,1) << endl;
	cout << "Restoring to standard" << endl;
	s3.rotate_to_standard();
	cout << "One sample of restored (1,0,0): "
		<< s3.u(0,0) << ", "
		<< s3.u(1,0) << ", "
		<< s3.u(2,0) << endl;
	cout << "One sample of restored (1,1,1): "
		<< s3.u(0,1) << ", "
		<< s3.u(1,1) << ", "
		<< s3.u(2,1) << endl;
	cout << "Trying spherical coordinate transformation to rotate phi=45 degrees,theta=0"
	<< endl;
	sc.phi=M_PI_4;
	sc.theta=0.0;
	s3.rotate(sc);
	cout << "one sample of rotated (1,0,0)"
		<< s3.u(0,0) << ", "
		<< s3.u(1,0) << ", "
		<< s3.u(2,0) << endl;
	cout << "one sample of rotated (1,1,0)"
		<< s3.u(0,2) << ", "
		<< s3.u(1,2) << ", "
		<< s3.u(2,2) << endl;
	cout << "one sample of rotated (0,0,1)"
		<< s3.u(0,3) << ", "
		<< s3.u(1,3) << ", "
		<< s3.u(2,3) << endl;
	s3.rotate_to_standard();
	cout << "Applying nonorthogonal transformation"<<endl;
	double a[3][3];
	a[0][0]=1.0;  a[0][1]=1.0;  a[0][2]=1.0;
	a[1][0]=-1.0;  a[1][1]=1.0;  a[1][2]=1.0;
	a[2][0]=0.0;  a[2][1]=-1.0;  a[2][2]=0.0;
	s3.apply_transformation_matrix(a);
	cout << "one sample of transformed (1,0,0)"
		<< s3.u(0,0) << ", "
		<< s3.u(1,0) << ", "
		<< s3.u(2,0) << endl;
	cout << "one sample of transformed (1,1,0)"
		<< s3.u(0,2) << ", "
		<< s3.u(1,2) << ", "
		<< s3.u(2,2) << endl;
	cout << "one sample of transformed (0,0,1)"
		<< s3.u(0,3) << ", "
		<< s3.u(1,3) << ", "
		<< s3.u(2,3) << endl;
	cout << "Testing back conversion to standard coordinates";
	s3.rotate_to_standard();
	cout << "One sample of restored (1,0,0): "
		<< s3.u(0,0) << ", "
		<< s3.u(1,0) << ", "
		<< s3.u(2,0) << endl;
	cout << "One sample of restored (1,1,1): "
		<< s3.u(0,1) << ", "
		<< s3.u(1,1) << ", "
		<< s3.u(2,1) << endl;
	cout << "One sample of restored (1,1,0): "
		<< s3.u(0,2) << ", "
		<< s3.u(1,2) << ", "
		<< s3.u(2,2) << endl;
	cout << "One sample of restored (0,0,1): "
		<< s3.u(0,3) << ", "
		<< s3.u(1,3) << ", "
		<< s3.u(2,3) << endl;

	cout << "Testing multiple, accumulated transformations" << endl;
	s3.rotate(nu);
	s3.apply_transformation_matrix(a);
	s3.rotate(sc);
	s3.rotate_to_standard();
	cout << "One sample of restored (1,0,0): "
		<< s3.u(0,0) << ", "
		<< s3.u(1,0) << ", "
		<< s3.u(2,0) << endl;
	cout << "One sample of restored (1,1,1): "
		<< s3.u(0,1) << ", "
		<< s3.u(1,1) << ", "
		<< s3.u(2,1) << endl;
	cout << "One sample of restored (1,1,0): "
		<< s3.u(0,2) << ", "
		<< s3.u(1,2) << ", "
		<< s3.u(2,2) << endl;
	cout << "One sample of restored (0,0,1): "
		<< s3.u(0,3) << ", "
		<< s3.u(1,3) << ", "
		<< s3.u(2,3) << endl;

	cout << "Trying Db constructor" << endl;
	int ierr;
	// dbsort and dbgroup have to have been done correctly for
	// this to have a hope of working.
        string testdbname("t3ctest");
        string tag("test_tcs_view");
        // This is the input database - open read only
        DatascopeHandle dbhraw(testdbname,true);
	DatascopeHandle dbh(dbhraw,pf,tag);
	list<string>groupkeys;
	groupkeys.push_back(string("sta"));
	cout << "Table number before group = "<<dbh.db.table<<endl;
	dbh.group(groupkeys);
	cout << "Table number after group = "<<dbh.db.table<<endl;
	AttributeMap am(amname);
	MetadataList mdl=pfget_mdlist(pf,"metadata_input");
        cout << "Input data MetadataList Contents:"<<endl;
        print_mdlist(mdl);
	MetadataList mdlout=pfget_mdlist(pf,"metadata_wfprocess_output");
        cout << "Test wfprocess output  MetadataList Contents:"<<endl;
        print_mdlist(mdlout);
	MetadataList mdlout2=pfget_mdlist(pf,"metadata_wfdisc_output");
        cout << "Test wfdisc output  MetadataList Contents:"<<endl;
        print_mdlist(mdlout2);
        dbh.rewind();
	ThreeComponentSeismogram s4(dynamic_cast<DatabaseHandle&>(dbh),mdl,am);
        cout << "Success"<<endl;
	cout << "Metadata for 3c seismogram constructed from database"<<endl;
	cout << dynamic_cast<Metadata&>(s4);
	cout << "Transformation matrix of this seismogram"<<endl;
	cout << s4.tmatrix[0][0]   
		<< ", "	<<  s4.tmatrix[0][1] 
		<< ", "	<<  s4.tmatrix[0][2]<<endl;
	cout << s4.tmatrix[1][0] 
		<< ", "	<<  s4.tmatrix[1][1] 
		<< ", "	<<  s4.tmatrix[1][2]<<endl;
	cout << s4.tmatrix[2][0] 
		<< ", "	<<  s4.tmatrix[2][1] 
		<< ", "	<<  s4.tmatrix[2][2]<<endl;
	// output test
	DatascopeHandle dbho("testout",false);
	cout << "Testing dbsave function to wfprocess"<<endl;
	s4.put("dir",".");
	s4.put("dfile","testdata_out.w");
        /* wfprocess uses the absolute relative time concept which is not
           part of wfdisc.  Have to save it to metadata for now, but be
           aware this may someday get wired into dbsave. */
        s4.put("timetype","a");  
	dbsave(s4,dbho.db,"wfprocess",mdlout,am);
	cout << "Successful write to testout database"<<endl;
	cout << "Trying to read another 3c object from database"<<endl;
	++dbh;
	ThreeComponentSeismogram s5(dynamic_cast<DatabaseHandle&>(dbh),mdl,am);
	s5.put("dir",".");
	s5.put("dfile","testdata_out.w");
	cout << "Metadata for next 3c trace read"<<endl;
	cout << dynamic_cast<Metadata&>(s5);
	cout << "Transformation matrix of this seismogram"<<endl;
	cout << s5.tmatrix[0][0]   
		<< ", "	<<  s5.tmatrix[0][1] 
		<< ", "	<<  s5.tmatrix[0][2]<<endl;
	cout << s5.tmatrix[1][0] 
		<< ", "	<<  s5.tmatrix[1][1] 
		<< ", "	<<  s5.tmatrix[1][2]<<endl;
	cout << s5.tmatrix[2][0] 
		<< ", "	<<  s5.tmatrix[2][1] 
		<< ", "	<<  s5.tmatrix[2][2]<<endl;
	cout << "Testing free_surface_transformation algorithm on first station read"<<endl;
	s4.rotate_to_standard();  // This probably isn't needed
	SlownessVector uvec;
	uvec.ux=0.17085;  // cos(-20deg)/5.5
	uvec.uy=-0.062185; // sin(-20deg)/5.5
	//s4.free_surface_transformation(uvec,8.0,3.5); // test exception
	s4.free_surface_transformation(uvec,5.0,3.5);
	vector<string>chanmap2;
	chanmap2.push_back("T");
	chanmap2.push_back("R");
	chanmap2.push_back("L");
	dbsave(s4,dbho.db,"wfdisc",mdlout2,am,chanmap2,false);
        cout << "Successfully saved T,R,L data."<<endl;
	cout << "Testing dbsave function with chanmap arguments"<<endl;
	vector<string> chanmap;
	chanmap.reserve(3);
	chanmap.push_back("BHE");
	chanmap.push_back("BHN");
	chanmap.push_back("BHZ");
	bool oswitch;
	oswitch=true;
	dbsave(s4,dbho.db,"wfdisc",mdlout2,am,chanmap,oswitch);
        cout << "Finished:  Verify that input B channel data match output exactly.  "<<endl
            <<"Final dbsave run in this program runs rotate_to_standard before writing result"<<endl
            <<"This is an important test of reversibility"<<endl;
        cout << "Test boost serialization code to save restored test data to file testserial.dat"<<endl;
        ofstream ofs("testserial.dat");
        boost::archive::text_oarchive oa(ofs);
        oa << s4;
        ofs.close();
        cout << "Success"<<endl
            << "Attempting restore"
            <<endl;
        ThreeComponentSeismogram s6;
        ifstream ifs("testserial.dat");
        boost::archive::text_iarchive ia(ifs);
        ia >> s6;
        ifs.close();
        cout << "Comparing before and after serialize" <<endl;
        dmatrix d4=s4.u;
        dmatrix d6=s6.u;
        dmatrix dio=d4-d6;
        double testsum;
        testsum=0.0;
        for(i=0;i<dio.rows();++i)
            for(j=0;j<dio.columns();++j)
                testsum += dio(i,j)*dio(i,j);
        cout << "Sum of squares of difference input - output"
            << testsum<<endl;;

    }
    catch (SeisppError&  serr)
    {
	serr.log_error();
    }
}
