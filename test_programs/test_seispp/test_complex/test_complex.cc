#include "ComplexTimeSeries.h"
#include "seispp.h"
using namespace std;
using namespace SEISPP;
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
    try {
	cout << "Calling simple constructor"<<endl;
	ComplexTimeSeries junk();
	cout << "Creating two 10 point empty object"<<endl;
	ComplexTimeSeries z1(10),z2(10);
	z1.live=true;
	z2.live=true;
	z1.put("nsamp",10);
	z2.put("nsamp",10);
	z1.put("samprate",1.0);
	z1.put("time",0.0);
	z1.put("teststring",string("Sample string metadata"));
	z1.add_gap(TimeWindow(1.0,4.0));
	cout << "Testing simple operators"<<endl;
	Complex cz1(2.0,0.0);
	Complex cz2(0.0,2.5);
	z1.initialize(cz1);
	z2.initialize(cz2);
	cout << "z1"<<endl<<z1;
	cout << "z2"<<endl<<z2;
	cout << "Setting gap in z1 to zeros"<<endl;
	z1.zero_gaps();
	cout << "z1"<<endl<<z1;
	cout << "z1 times 2.0"<<endl;
	z1=z1*2.0;
	cout << z1;
	cout << "z1+z2"<<endl;
	z1+=z2;
	cout << z1;
	cout << "Testing conversion routines"<<endl
	<<"Fetch real part of z1"<<endl;
	TimeSeries rz1=z1.real();
	cout << rz1;
	cout << "Imag part z1"<<endl;
	TimeSeries iz1=z1.imag();
	cout << iz1;
	cout << "magnitude z1"<<endl;
	TimeSeries magz1=z1.mag();
	cout << magz1;
	cout << "phase z1"<<endl;
	TimeSeries pz1=z1.phase();
	cout << pz1;
	cout << "Conjugate of z1"<<endl;
	ComplexTimeSeries conjz1=z1.conj();
	cout << conjz1;
	cout << "Trying out dbsave to db test"<<endl;
	Pf *pf;
	pfread("test_complex",&pf);
	MetadataList mdl=pfget_mdlist(pf,string("trace_metadata"));
	AttributeMap am(string("css3.0"));
        DatascopeHandle dbh(string("testdb"),false,false);
	z1.put("wfprocess.dir",".");
	z1.put("wfprocess.dfile","testdata.w");
	z1.put("wfprocess.datatype","cx");
	z1.put("wfprocess.timetype","relative");
	z1.put("timetype","relative");
	z1.put("wfprocess.algorithm","test_complex");
        dbh.lookup("wfprocess");
	long foff;
	foff=dbsave(z1,dbh.db,string("wfprocess"),mdl,am);
	cout << "z1 saved successfully.  Trying to read it back."<<endl;
	MetadataList mdli=pfget_mdlist(pf,string("input_metadata"));
	dbh.lookup("wfprocess");
	dbh.db.record=0;
	ComplexTimeSeries z1read(dynamic_cast<DatabaseHandle&>(dbh),mdli,am);
	cout << z1read;
    } 
    catch (SeisppError& serr)
    {
        serr.log_error();
    }
}
