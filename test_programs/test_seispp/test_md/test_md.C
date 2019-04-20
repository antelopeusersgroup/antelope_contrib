#include "stock.h"
#include "pf.h"
#include "db.h"
#include "Metadata.h"
#include "PfStyleMetadata.h"
#include "seispp.h"
using namespace SEISPP;
// test routine for metadata functions
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
	//string pfname("test_md");
	char *pfname=strdup("test_md");
	string amname("css3.0");
	int ierr;
	SEISPP_verbose=true;

	Pf *pf;
	
	//if(pfread(const_cast<char *>(pfname.c_str),&pf))
	if(pfread(pfname,&pf))
	{
		cerr << "Pfread error"<<endl;
		exit(-1);
	}
	try {
		cout << "Trying AttributeMap pf constructor"<<endl;
		SEISPP::AttributeMap am(amname);
		cout << "Trying copy constructor and = operator"<<endl;
		SEISPP::AttributeMap am2(am);
		am=am2;
		cout << "Trying pfget_mdlist function with tag mdlist" << endl;
		MetadataList mdl=pfget_mdlist(pf,"mdlist");
		cout << "Trying to copy mdlist" << endl;
		MetadataList mdl2;
		mdl2=mdl;
		cout << "Original and copy of mdlist read" << endl;
		MetadataList::iterator mdi,mdi2;
		for(mdi=mdl.begin(),mdi2=mdl2.begin();mdi!=mdl.end();++mdi,++mdi2)
		{
			cout << mdi->tag 
			<< "," 
			<< mdi2->tag 
			<< "," 
			<<mdi->mdt
			<< "," 
			<<mdi2->mdt<<endl;
		}
		cout << "Testing delete on MetadataList object " << endl;
		MetadataList *mdl3=new MetadataList(mdl);
		delete mdl3;
		for(mdi=mdl.begin();mdi!=mdl.end();++mdi)
		{
			cout << mdi->tag 
			<< "," 
			<<mdi->mdt<<endl;
		}

		cout << "Trying to build Metadata objects" << endl;
		cout << "Trying default constructor" << endl;
		Metadata mdplain;
		cout << mdplain;
		cout << "Trying string constructor with inline data" << endl;
                /* Note the nsamp field is here to test that the + operator replaces 
                   instead of duplicates */
		string smdtest("x 4.5\ny 8.0\ni 27\nMYPAR 2.5\ncpar xyz\nnsamp -999\n");
		Metadata mds(smdtest);
		cout << mds;
		cout <<"Trying db constructor in testdb"<<endl;
		Dbptr db;
		char *dbname=strdup("testdb");
		DatascopeHandle dbh(dbname,pfname,string("dbprocess_list"),
                        false);
		dbh.rewind();
		Metadata *mddb = new Metadata(dynamic_cast<DatabaseHandle&>(dbh),
					mdl,am2);
		cout << *mddb;
                cout << "Trying + operator.  Merging inline and db objects"<<endl;
                Metadata mdsum;
                mdsum=mds+(*mddb);
                cout << mdsum<<endl;
                cout << "Sum in reverse order - simple should override db"<<endl;
                mdsum=(*mddb)+mds;
                cout << mdsum<<endl;
                /* Now try out the PfStyleMetadata object.  We use the
                   same pf file as above */
                string pfsn("test_md");
                PfStyleMetadata pfsmd(pfsn);
                cout << "Reading and writing a couple of simple parameters"<<endl;
                cout << "simple_real_parameter="
                    <<pfsmd.get<double>("simple_real_parameter")<<endl
                  << "simple_int_parameter="
                    <<pfsmd.get<int>("simple_int_parameter")<<endl
                  << "simple_bool_parameter="<<pfsmd.get_bool("simple_bool_parameter")
                  <<endl;
                list<string> tsttbl;
                tsttbl=pfsmd.get_tbl("mdlist");
                cout << "mdlist Tbl extracted from pf"<<endl;
                list<string>::iterator iptr;
                for(iptr=tsttbl.begin();iptr!=tsttbl.end();++iptr)
                {
                  cout << *iptr<<endl;
                }
                cout << "Trying get_branch method - extracting test_nested_tag"
                  <<endl;
                PfStyleMetadata pfbr(pfsmd.get_branch("test_nested_tag"));
                cout << "Success"<<endl
                    <<"Contents"<<endl
                    << dynamic_cast<Metadata&>(pfbr)<<endl;
                cout << "test_double parameter in branch="<<pfbr.get_double("test_double")<<endl;

	}
	catch (MetadataError& mess)
	{
		mess.log_error();
		exit(-1);
	}
	catch (SeisppError& sess)
	{
		sess.log_error();
	}
	
}
