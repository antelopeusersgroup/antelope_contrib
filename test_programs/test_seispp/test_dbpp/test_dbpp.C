#include "dbpp.h"
#include "seispp.h"
using namespace SEISPP;
void log_Dbptr(Dbptr db)
{
	cout << "Dbptr contents:"<<endl
	<< "database="<<db.database
	<< "table="<<db.table
	<< "field="<<db.field
	<< "record="<<db.record<<endl;
}
bool SEISPP::SEISPP_verbose(true);
int main(int argc, char **argv)
{
	string dbname("testdb"), pfname("test_dbpp");
	string pftag("one_table_open");
	string pftag2("dbprocess_join_test");

	try{
	cout << "Testing simple open and close"<<endl;
	DatascopeHandle dbh1(dbname,true);
	log_Dbptr(dbh1.db);
	dbh1.close();
	}
	catch (SeisppError& sdberr)
	{
		sdberr.log_error();
		exit(-1);
	}

	cout << "Testing full constructor with pf and one table" << endl;
	try{
		DatascopeHandle dbh2(dbname,pfname,pftag,false);
		cout << dbname <<" view created by " << pftag << " has "
			<< dbh2.number_tuples() << " rows" <<endl;
		// assumes pf sets view to be assoc table
		cout << "Trying to join assoc->arrival"<<endl;
		dbh2.natural_join(string("arrival"));
		cout << "Join table size = "<<dbh2.number_tuples()<<endl;
		cout << "Trying to join event and origin with evid"
			<< endl;
		list<string>joinkeys;
		joinkeys.push_back(string("evid"));
		dbh2.join(string("event"),string("origin"),
			joinkeys,joinkeys);
		cout << "Join table size = "<<dbh2.number_tuples()<<endl;
		cout << "closing this database " << endl;
		dbh2.close();
	}
	catch (SeisppError& sdberr)
	{
		sdberr.log_error();
		exit(-1);
	}
	
	cout << "Testing get and put functions"<<endl;
	try {
		// input db
		DatascopeHandle dbh(dbname,true);
		//output db
		DatascopeHandle dbho(string("testout"),false);
		dbh.lookup("site");
		dbho.lookup("site");
		dbh.rewind();
		dbho.rewind();
		for(int i=0;i<dbh.number_tuples();
				++i,++dbh)
		{
			double lat,lon,elev,dnorth,deast;
			int ondate,offdate;
			string sta,staname,statype,refsta;
			dbho.append();
			// some simple get and puts
			lat = dbh.get_double("lat");
			lon = dbh.get_double("lon");
			elev = dbh.get_double("elev");
			dnorth = dbh.get_double("dnorth");
			deast = dbh.get_double("deast");
			statype=dbh.get_string("statype");
			dbho.put("lat",lat);
			dbho.put("lon",lon);
			dbho.put("elev",elev);
			dbho.put("dnorth",dnorth);
			dbho.put("deast",deast);
			dbho.put("statype",statype);
			// try this construct
			dbho.put(string("ondate"),dbh.get_int("ondate"));
			dbho.put("offdate",dbh.get_int("offdate"));
			dbho.put("sta",dbh.get_string("sta"));
			dbho.put("staname",dbh.get_string("staname"));
			refsta = dbh.get_string("refsta");
			dbho.put("refsta",refsta);
		}
		cout << "Copy of site completed" << endl;
		dbh.close();
		dbho.close();
	}
	catch (SeisppError& sdberr)
	{
		sdberr.log_error();
		exit(-1);
	}
	cout << "testing other relational db features"<<endl;
	try{
		// pftag2 should define an event->origin->assoc->arrival
		// view with orid==prefor
		DatascopeHandle dbh(dbname,pfname,pftag2,false);
		list<string> sortkeys;
		list<string> groupkeys;
		string sstr("sta=~/PFO/");
		sortkeys.push_back(string("evid"));
		sortkeys.push_back(string("arid"));
		groupkeys.push_back(string("evid"));
		cout << "Working view has "<<dbh.number_tuples()
			<< " rows and " << dbh.number_attributes()
			<< " columns" << endl;
		cout << "Testing assignment operator to save base view"<<endl;
		cout << "right hand side" << endl;
		log_Dbptr(dbh.db);
		DatascopeHandle dbhv=dbh;
		cout << "left hand side result" << endl;
		log_Dbptr(dbh.db);
		
		cout << "Trying sort "<<endl;
		dbh.sort(sortkeys);
		cout << "Sorted view has "<<dbh.number_tuples()
			<< " rows and " << dbh.number_attributes()
			<< " columns" << endl;

		cout << "Testing copy constructor"<<endl;
		cout << "parent Dbptr " << endl;
		log_Dbptr(dbh.db);
		DatascopeHandle dbhgrp(dbh);
		cout << "Result of copy constructor Dbptr " << endl;
		log_Dbptr(dbhgrp.db);
		cout << "Testing group function" << endl;
		dbhgrp.group(groupkeys);
		cout << "Grouped view has "<<dbhgrp.number_tuples()
			<< " group bundles "<<endl;
		cout << "Testing subset on original view" << endl;
		dbhv.subset(sstr);
		cout << "Subset view has "<<dbhv.number_tuples()
			<< " rows and " << dbhv.number_attributes()
			<< " columns" << endl;
		// Intentionally do not call close here
	}
	catch (SeisppError& sdberr)
	{
		sdberr.log_error();
		exit(-1);
	}
}
