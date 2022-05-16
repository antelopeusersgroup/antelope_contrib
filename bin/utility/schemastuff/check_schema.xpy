"""
check schema definitions for inconsitencies

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     May 9, 2022
@version     1.0
@license     MIT-style license

"""


def read_attributes(fp):
    attributes = {}
    relations = {}
    schemata = {}

    with open(fp) as f:
        try:
            lines = f.readlines()
        except Exception as __:
            elog.notify("encoding problem in file %s" % fp.decode())

    with open(fp, encoding="utf8", errors="ignore") as f:
        lines = f.readlines()
        attribute = False
        relation = False
        schema = False
        def_name = ""
        mydict = {}
        detail = None
        fields = None
        for line in lines:
            # line = line.strip() # no big difference, examine later
            num_match = re.match(
                r"\s*(\S+)\s*\(\s*(\d+)\s*\)", line
            )  # [space],Word,[space],(,[space],Number,[space],)
            fields_match = re.match(
                r"\s*Fields\s*\((.*)", line
            )  # only opening bracket on line, rest follows on further lines (until closing bracket)
            property_match = re.match(
                r"\s*(\S+)\s*\((.*)\)", line
            )  # any "property" with a "value" in brackets ()
            def_match = re.match(r"\s*(Attribute|Schema|Relation)\s+(.*)", line)
            def_end = re.match(r"\s*;", line)
            detail_match = re.match(r"\s*Detail\s*\{", line)
            detail_end = re.match(r"\s*\}", line)
            if detail_match:
                detail = []
            elif detail_end:
                ds = "".join(detail)
                mydict["detail"] = ds
                detail = None
            elif detail is not None:
                detail.append(line)
            elif fields is not None:
                if ")" in line:  # this is the end
                    line = line.replace(")", "")
                    fields = line.split()
                    if "fields" in mydict:
                        mydict["fields"] += fields
                    else:
                        mydict["fields"] = fields
                    fields = None  # close
                else:
                    fields += line.split()
            elif def_match:
                de_t = def_match.group(1)
                de_n = def_match.group(2).strip()
                mydict = {}
                mydict["file"] = fp
                if de_t == "Attribute":
                    attribute_name = de_n
                    attribute = True
                    mydict["attribute"] = de_n
                    # if de_n == "sswrodgf":
                    #    breakpoint()
                elif de_t == "Relation":
                    relation = True
                    mydict["relation"] = de_n
                elif de_t == "Schema":
                    schema = True
                    mydict["schema"] = de_n
                def_name = de_n
            elif num_match:
                t = num_match.group(1)
                v = num_match.group(2)
                mydict["len"] = int(v)
                if t == "Real":
                    mydict["type"] = "real"
                elif t == "Integer":
                    mydict["type"] = "int"
                elif t == "String":
                    mydict["type"] = "str"
                elif t == "Time":
                    mydict["type"] = "time"
                elif t == "YearDay":
                    mydict["type"] = "jearday"
                elif t == "Dbptr":
                    mydict["type"] = "dbptr"
            elif property_match:
                t = property_match.group(1)
                v = property_match.group(2)
                v = v.strip().strip('"').strip("'")  # get rid of surrounding quotes
                if t == "Format":
                    mydict["format"] = v
                elif t == "Null":
                    mydict["null"] = v
                elif t == "Range":
                    mydict["range"] = v
                elif t == "Units":
                    mydict["units"] = v
                elif t == "Units":
                    mydict["units"] = v
                elif t == "Units":
                    mydict["units"] = v
                elif t == "Fields":
                    mydict["fields"] = v.split()
                elif t == "Primary":
                    mydict["primary"] = v.split()
                elif t == "Alternate":
                    mydict["alternate"] = v.split()
                elif t == "Foreign":
                    mydict["foreign"] = v.split()
            elif fields_match:
                fields = []
                val = fields_match.group(1).strip()
                if val != "":
                    fields = val.split()
            elif def_end:
                if attribute:
                    attributes[def_name] = mydict
                elif relation:
                    relations[def_name] = mydict
                elif schema:
                    schemata[def_name] = mydict
                attribute = False
                relation = False
                schema = False
    return attributes, relations, schemata


def check_schema_files(schemaname):
    elog.notify("check schema files for schema  %s" % schemaname)
    fp = os.path.join(os.environ["ANTELOPE"] + "/data/schemas/" + schemaname)
    fp2 = os.path.join(os.environ["ANTELOPE"] + "/contrib/data/schemas/" + schemaname)
    attributes = {}
    relations = {}
    # print(os.path.join(directory, filename))
    if os.path.exists(fp) and os.path.exists(fp2):
        elog.notify("strange, schema found both on %s and %s" % (fp, fp2))
    if os.path.exists(fp):
        base_attributes, base_relations, base_schema = read_attributes(fp)
        for attribute in base_attributes:
            my_att = base_attributes[attribute]
            check_file_attribute(my_att, fp)
        # print(base_schema)
    elif os.path.exists(fp2):
        base_attributes, base_relations, base_schema = read_attributes(fp2)
        for attribute in base_attributes:
            my_att = base_attributes[attribute]
            check_file_attribute(my_att, fp2)
        # print(base_schema)
    extpath = "%s.ext" % schemaname
    basedir = os.path.join(os.environ["ANTELOPE"] + "/data/schemas/" + extpath)
    if os.path.exists(basedir):
        directory = os.fsencode(basedir)
        for myfile in os.listdir(directory):
            filename = os.fsdecode(myfile)
            fp = os.path.join(directory, myfile)
            filepath = fp.decode()
            if filename.startswith("."):
                elog.notify("Attention: hidden file %s" % filepath)
            att, rel, schema = read_attributes(fp)
            attributes[filename] = att
            relations[filename] = rel
            base_attributes.update(att)
            base_relations.update(rel)
            if schema != {}:
                elog.notify("file %s: second schema defined" % (filepath, schema))
            n_relations = len(rel.keys())
            if n_relations > 1:
                elog.notify(
                    "strange, found more than one relation (%d) in file %s"
                    % (n_relations, filepath)
                )
            for attribute in att:
                my_att = att[attribute]
                check_file_attribute(my_att, filepath)
            # for relation in rel:
            #    my_rel = rel[relation]
            #    check_relation(my_rel, att, filepath, base_attributes)

    basedir = os.path.join(os.environ["ANTELOPE"] + "/contrib/data/schemas/" + extpath)
    if os.path.exists(basedir):
        directory = os.fsencode(basedir)
        extension_attributes = {}
        for myfile in os.listdir(directory):
            filename = os.fsdecode(myfile)
            fp = os.path.join(directory, myfile)
            filepath = fp.decode()
            if filename.startswith("."):
                elog.notify("Attention: hidden file %s" % filepath)
            att, __, __ = read_attributes(fp)
            extension_attributes.update(att)
        for myfile in os.listdir(directory):
            filename = os.fsdecode(myfile)
            fp = os.path.join(directory, myfile)
            filepath = fp.decode()
            if filename.startswith("."):
                elog.notify("Attention: hidden file %s" % filepath)
            att, rel, schema = read_attributes(fp)
            attributes[filename] = att
            relations[filename] = rel
            if schema != {}:
                elog.notify("file %s: second schema defined" % (filepath, schema))
            n_relations = len(rel.keys())
            if n_relations > 1:
                elog.notify(
                    "strange, found more than one relation (%d) in file %s"
                    % (n_relations, filepath)
                )
            for attribute in att:
                my_att = att[attribute]
                check_file_attribute(my_att, filepath)
            for relation in rel:
                my_rel = rel[relation]

                other_attributes = copy.deepcopy(extension_attributes)
                # for attribute in att:
                #    other_attributes.pop(attribute)
                check_file_relation(
                    my_rel, att, filepath, base_attributes, other_attributes
                )


def check_file_relation(rel, att, filename, base_attributes, other_attributes):
    name = ""
    if "relation" in rel:
        name = rel["relation"]
    else:
        elog.complain("file %s: nameless relation %s" % (filename, rel.keys()))
        return
    if not "fields" in rel:
        elog.complain("file %s: no fields in relation %s" % (filename, rel))
        return
    for field in rel["fields"]:
        defined_here = False
        defined_there = False
        defined_in_extensions = False
        if field in att:
            defined_here = True
        if field in base_attributes:
            defined_there = True
        if field in other_attributes:
            defined_in_extensions = True
        if not (defined_here or defined_there):
            if not defined_in_extensions:
                breakpoint()
                elog.complain("file %s: attribute %s undefined" % (filename, field))
            else:
                dfile = ""
                if "file" in other_attributes[field]:
                    dfile = other_attributes[field]["file"].decode()
                else:
                    elog.complain(
                        "file %s: HELP! Unable to find filename for attribute %s"
                        % (filename, field)
                    )
                elog.complain(
                    "file %s: attribute %s defined in another extension (%s)"
                    % (filename, field, dfile)
                )


def check_file_attribute(att, filename):
    name = ""
    atype = ""
    length = -1
    if "attribute" in att:
        name = att["attribute"]
    else:
        elog.complaint("nameless attribute %s" % att.keys())
        return
    if "len" in att:
        length = att["len"]
    else:
        elog.complain(
            "%s: no lenght defined for attribute %s, giving up format check for this attribute"
            % (filename, name)
        )
        return
    if "type" in att:
        atype = att["type"]
    else:
        elog.complain(
            "file %s: no type defined for attribute %s, giving up format check for this attribute"
            % (filename, name)
        )
        return

    if "format" in att:
        ftype = ""
        formatstr = att["format"]
        if len(formatstr) < 2:
            elog.complain(
                "file %s: format specification too short %s for attribute %s"
                % (filename, formatstr, name)
            )
        if atype != "dbptr":
            fmtmatch = re.match(r"%(.*\d+)([a-z]+)", formatstr)
            if fmtmatch:
                fmt_nr = fmtmatch.group(1)
                l_ftype = fmtmatch.group(2)
                ftype = l_ftype[-1]
                if len(l_ftype) == 2 and l_ftype[0] != "l":
                    elog.complain(
                        "file %s: suspicious length specification %s in format for attribute %s"
                        % (filename, formatstr, name)
                    )
                len_from_format = abs(int(float(fmt_nr)))
                if len_from_format != length:
                    elog.complain(
                        "file %s: length mismatch %d != %s for attribute %s"
                        % (filename, length, formatstr, name)
                    )
            else:
                elog.complain(
                    "file %s: suspicious format %s for attribute %s"
                    % (filename, formatstr, name)
                )

        if not formatstr.startswith("%"):
            elog.complain(
                "file %s: format %s for attribute %s should start with '%%'"
                % (filename, formatstr, name)
            )
        if atype == "int" and ftype != "d":
            elog.complain(
                "file %s: format type mismatch for attribute %s (%s != %s)"
                % (filename, name, formatstr, atype)
            )
        if atype == "real" and ftype not in "efg":
            elog.complain(
                "file %s: format type mismatch for attribute %s (%s != %s)"
                % (filename, name, formatstr, atype)
            )
        if atype == "string" and ftype != "s":
            elog.complain(
                "file %s: format type mismatch for attribute %s (%s != %s)"
                % (filename, name, formatstr, atype)
            )
        if atype == "time" and ftype != "f":
            elog.complain(
                "file %s: format type mismatch for attribute %s (%s != %s)"
                % (filename, name, formatstr, atype)
            )
    else:
        elog.notify(
            "file %s: no format specification for attribute %s" % (filename, name)
        )
    if "null" in att:
        fnull = att["null"]
        if atype == "real":
            if not "." in fnull:
                elog.notify(
                    "file %s: NULL specification for attribute %s should be a float instead of '%s'"
                    % (filename, name, fnull)
                )

    if "range" in att:
        frange = att["range"]
        if name not in frange:
            elog.complain(
                "file %s: meaningless RANGE check for attribute %s. Attribute not mentioned in definition of RANGE '%s'"
                % (filename, name, frange)
            )


def check_keys(tablename, fields, keys, ktype):
    """ see if keys are defined
    """
    if len(keys) > 0:
        for key in keys:
            if "::" in key:
                for sub_key in key.split("::"):
                    if sub_key not in fields:
                        elog.notify(
                            "%s key %s (%s) not found in table %s"
                            % (ktype, sub_key, key, tablename)
                        )
            elif key not in fields:
                elog.notify("%s key %s not found in table %s" % (ktype, key, tablename))


def check_table_keys(table):
    tablename = table.query(ds.dbTABLE_NAME)
    fields = table.query(ds.dbTABLE_FIELDS)
    primary_keys = table.query(ds.dbPRIMARY_KEY)
    alternate_key = table.query(ds.dbALTERNATE_KEY)
    foreign_keys = table.query(ds.dbFOREIGN_KEYS)
    check_keys(tablename, fields, primary_keys, "Primary")
    check_keys(tablename, fields, alternate_key, "Alternate")
    check_keys(tablename, fields, foreign_keys, "Foreign")


def check_range(range, fieldname):
    pass


def check_attribute(db, field):
    """
    validate if:
    NULL key present (and makes sense)
    FORMAT defined and matches type definition
    UNITS defined
    and such...
    """
    fnull = None
    ftype = None

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        # this is needed to catch errors when looking up a nonexisting field-attribute
        # the usual try-catch does not help here
        tables = db.query(ds.dbSCHEMA_TABLES)
        not_found = True
        for table in tables:
            dbt = db.lookup(table=table)
            fields = dbt.query(ds.dbTABLE_FIELDS)
            if field in fields:
                not_found = False
                break  # we found the
        if not_found:
            elog.complain("field %s: found in no table" % field)
            return

        dbq = dbt.lookup(field=field, record="dbNULL")
        flen = dbq.query(ds.dbFIELD_SIZE)
        ftype = dbq.query(ds.dbFIELD_TYPE)
        try:
            fformat = dbq.query(ds.dbFIELD_FORMAT)
        except Exception as __:
            elog.notify("attribute %s: no FORMAT" % field)
        funits = dbq.query(ds.dbFIELD_UNITS)
        ftype = dbq.query(ds.dbFIELD_TYPE)
        # fnull = dbq.query(sd.dbNULL)
        try:
            [fnull] = dbq.getv(field)
        except Exception as __:
            elog.notify("attribute %s: no NULL" % field)
        if fnull == "":
            elog.complain("attribute %s empty NULL value" % field)
    if fformat and fformat != "" and ftype and ftype != "":
        pass
    if fnull and fnull != "" and fformat and fformat != "":
        if ftype != ds.dbDBPTR:
            try:
                testval = fformat % fnull
            except Exception as __:
                elog.complain(
                    "attribute %s: problem with format :%s: or null :%s: "
                    % (field, fformat, fnull)
                )
            str_null = str(fnull)
            if testval.strip() != str_null.strip():
                elog.complain("attribute %s: null: %s != %s" % (field, fnull, testval))


import getopt
import warnings
import re
import copy

# Import Antelope modules

import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog


def usage(progname):
    print(progname, "[-v] [-f|-s] schema [table]")


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init(progname)

    tablename = None
    verbose = False
    schema_checks = True
    file_checks = True
    opts = []
    args = []
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vfs", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        return 2

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-s":
            schema_checks = True
            file_checks = False
        elif o == "-f":
            file_checks = True
            schema_checks = False

    if len(args) > 2 or len(args) < 1:
        usage(progname)
        sys.exit(1)

    schemaname = args[0]

    if len(args) > 1:
        tablename = args[1]

    # 1st check on stuff read from files
    if file_checks:
        check_schema_files(schemaname)
    # 2nd check on the object in memory
    if schema_checks:
        with ds.destroying(ds.dbtmp(schemaname)) as db:
            if tablename:
                try:
                    dbt = db.lookup(table=tablename)
                except Exception as __:
                    elog.die("table %s not found in schema %s" % (tablename, schemaname))
                check_table_keys(dbt)
                for fieldname in sorted(dbt.query(ds.dbTABLE_FIELDS)):
                    check_attribute(dbt, fieldname)
            else:
                for tablename in sorted(db.query(ds.dbSCHEMA_TABLES)):
                    dbt = db.lookup(table=tablename)
                    check_table_keys(dbt)
                for fieldname in sorted(db.query(ds.dbSCHEMA_FIELDS)):
                    check_attribute(db, fieldname)

    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
