#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
*******
Datascope_schema - builds pf file for MsPASS import/export
*******
.. topic:: Usage

    Datascope_schema [-pf pffile]  [-t t1 t2 ...]

.. topic::Description

    This is a command line tool to generate a pf file that is the 
    input file for the mspass `DatascopeDatabase` class.  The output 
    pf is used by the constructor of that class to parse the fixed field, 
    text files used by the Datascope database.   The pf file could be 
    distributed with mspass but this allows more flexibility in adding 
    additional tables not created by default.  
    
    This application requires a license for Antelope to run. Most 
    MsPASS Users can simply use the default parameter file 
    found in the MsPASS distribution.  That master was created with 
    this program. 

.. topic::Arguments

    -pf:
       Output to file defined by pffile instead of default 
       "DatascopeDatabase.pf".

    -t:
      List of tables to parse produce the output pf file. 
      Default is a list of more standard tables. 


@author: Gary Pavlis
"""
import sys, os, getopt
import argparse

sys.path.append(os.environ["ANTELOPE"] + "/data/python")
from antelope.datascope import *


def parse_table(db, table) -> list:
    """
    Returns a list of tuples in field index order defining the
    field properties of the specified table.   The tuples each
    contain:  (attribute name, field width, byte 0 of field, type_name, null_value).
    The type is converted using a constant table defined above from
    integers that are presumably a raw index of a C enum.

    :param db:  Datascope db pointer (four integers).
    :param table:  name of table to be parsed (string type)

    :return:  list of tuples in field order - see above for tuple content.
    """
    record_data = []
    dbh = db.lookup(table=table)
    nf = dbh.query(dbFIELD_COUNT)
    for i in range(nf):
        dbh.field = i
        name = dbh.query(dbFIELD_NAME)
        fsize = dbh.query(dbFIELD_SIZE)
        fi = dbh.query(dbFIELD_INDEX)
        form = dbh.query(dbFIELD_FORMAT)
        # this returns an integer.  The integer is a magic number that
        # defines type. We convert it to a string handled downstream
        ftype = dbh.query(dbFIELD_TYPE)
        # more robust way supplied by Kent - db cosntant names need checking
        ftype = dbh.query(dbFIELD_TYPE)
        if ftype == dbREAL or ftype == dbTIME:
            type_name = "float"
        elif ftype == dbINTEGER or ftype == dbYEARDAY:
            type_name = "int"
        elif ftype == dbSTRING:
            type_name = "string"
        elif ftype == dbBOOL:
            type_name = "bool"
        elif ftype == dbDBPTR:
            type_name = "dbptr"
        else:
            message = "parse_table:   dbFIELD_TYPE unrecognized value={}".format(ftype)
            raise RuntimeError(message)

        dbn = db.lookup(table=table, field=name, record="dbNULL")
        this_null = dbn.getv(name)
        # for some weird reason getv returns a tuple
        null_value = this_null[0]
        record_data.append([name, fsize, fi, type_name, null_value, form])
    return record_data


def get_primary_key(db, table) -> list:
    """
    Return a list of the pimary keys defined for a table.
    db is assumed a Datascope dbptr and table is a string defining
    a table name defined for the schema with which db is associated.

    """
    dbh = db.lookup(table=table)
    pkeys = dbh.query(dbPRIMARY_KEY)
    return pkeys


def load_table_data(
    db,
    tablelist=[
        "wfdisc",
        "site",
        "sitechan",
        "event",
        "origin",
        "assoc",
        "arrival",
        "origerr",
        "prearr",
        "moment",
        "mt",
    ],
) -> dict:
    """
    Loads what is more or less an in memory image of the data structure
    this program writes to the pf file.  It creates a python dictionary
    keyed by table names.  The contents of the values of each key
    are used to construct the pf "Arr" section for that table in the
    output pf.

    :param db:  Datascope dbptr of an instance of an Datascope
    handle.  Whatever schema that handle is associated with defines the
    attributes loaded for the requested table.
    :type db:  Datascope dbptr

    :param tablelist:  list of strings defining the tables to be
    parsed and returned as the output.

    :return:  python dictionary keyed by table names sent through tablelist.
    """
    result = dict()
    for table in tablelist:
        this_entry = dict()
        arr = parse_table(db, table)
        this_entry["attributes"] = arr
        pkeys = get_primary_key(db, table)
        this_entry["primary_keys"] = pkeys
        result[table] = this_entry
    return result


def write_pf(table_data, pffile="DatascopeDatabase.pf"):
    """
    Creates a file with name pffile and wriites the contentn of
    table_data to the file.  table_data is assumed to be the
    dictionary output by the function in the module called
    "load_table_data".  Note the file is openned with "w"
    which will silently overwrite any existing content.
    That seems the appropriate model for this application.

    Important:  note the attributes &Tbl is assumed to be in
    original file order by the DatascopeDatabase class that uses
    this pf.
    """
    with open(pffile, "w") as fp:
        for table in table_data:
            # note normal string format is problematic with
            # pf curly brackets so I use a string operator
            s = table + " &Arr{\n"
            fp.write(s)
            tbldata = table_data[table]
            pkeys = tbldata["primary_keys"]
            s = "primary_keys &Tbl{\n"
            fp.write(s)
            for k in pkeys:
                s = str(k)
                s += "\n"
                fp.write(s)
            fp.write("}\n")

            fp.write("attributes &Tbl{\n")
            att = tbldata["attributes"]
            for a in att:
                # att is a list of tuples - need to reformat to
                # match Tbl requirement of strings with white space
                s = ""
                for x in a:
                    s += str(x)
                    s += " "
                s += "\n"
                fp.write(s)
            fp.write("}\n}\n")


def print_fields(fields):
    """
    For testing only
    """
    print("name width offset type null_value")
    for x in fields:
        print(x[0], x[1], x[2], x[3], x[4])


def main(args=None):
    # As a script that would be run from the shell we let
    # any functions below that throw exception do so and assume they
    # will write a message that can help debug what went wrong
    if args is None:
        args = sys.argv[1:]
    parser = argparse.ArgumentParser(
        prog="Datascope_schema",
        usage="%(prog)s [-pf pffile]  [-t t1 t2 ...]",
        description="Create parameter file used to construct a DatascopeDatabase object",
    )
    parser.add_argument(
        "-pf",
        "--pffile",
        action="store",
        type=str,
        default="DatascopeDatabase.pf",
        help="Change name of output pf file",
    )
    parser.add_argument(
        "-t",
        "--tables",
        action="store",
        nargs="*",
        default=[
            "arrival",
            "assoc",
            "calibration",
            "detection",
            "digitizer",
            "emodel",
            "event",
            "fplane",
            "gap",
            "gps",
            "gregion",
            "hypocentroid",
            "instrument",
            "lastid",
            "moment",
            "mt",
            "netmag",
            "netmw",
            "network",
            "nominalresp",
            "origerr",
            "origin",
            "predarr",
            "predmech",
            "remark",
            "schanloc",
            "seismometer",
            "sensor",
            "sensorcal",
            "sensormodel",
            "site",
            "sitechan",
            "snetsta",
            "specdisc",
            "trigger",
            "wfdisc",
            "wfdisc_tshift",
            "wfedit",
            "wfmeas",
            "wfmgme",
            "wfoffset",
            "wfparam",
            "wfrms",
            "wfsrb",
            "wftag",
        ],
        help="list of table (relation) names to be defined in output - must be consistent with schema",
    )

    args = parser.parse_args(args)
    pffile = args.pffile
    tables = args.tables
    db = dbopen("temp", "r")
    tabledata = load_table_data(db, tables)
    write_pf(tabledata, pffile=pffile)


if __name__ == "__main__":
    main()
