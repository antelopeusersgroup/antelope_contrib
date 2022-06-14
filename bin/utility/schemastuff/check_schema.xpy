"""
check schema definitions for inconsistencies
this is done in 2 steps, first on the contents of the files parsed and then on the resultin schema in memory.
The first step - files - is necessary to detect unused attributes, duplicate definitions and multiple definitions in general.


@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     May 9, 2022
@version     1.0
@license     MIT-style license

assumptions:
DESCRIPTION, DETAIL are optional
definitions from $ANTELOPE are taken for granted,
contributed definitions should follow the following rules 

"""
global pedantic
pedantic = False
global problem_seen
problem_seen = False


def log_info(infostring):
    global pedanti
    if pedantic:
        print(infostring)


def log_complaint(complaintstring):
    print(complaintstring)


def log_error(errorstring):
    print(errorstring)


def read_attributes(fp):
    attributes = {}
    relations = {}
    schemata = {}
    error_seen = False

    with open(fp) as f:
        try:
            lines = f.readlines()
        except Exception as __:
            log_info(
                "file %s: encoding problem. File is not purely UTF-8" % fp.decode()
            )
            # error_seen = True
            problem_seen = True

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
                if ")" in line:  # this is the end of a definition
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
                elif t == "Description":
                    mydict["description"] = v
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
    return error_seen, attributes, relations, schemata


def check_schema_files(schemaname):
    error_seen = False
    log_info("check schema files for schema %s" % schemaname)
    fp = os.path.join(os.environ["ANTELOPE"] + "/data/schemas/" + schemaname)
    fp2 = os.path.join(os.environ["ANTELOPE"] + "/contrib/data/schemas/" + schemaname)
    attributes = {}
    relations = {}
    all_attributes = {}
    all_relations = {}
    # print(os.path.join(directory, filename))
    if os.path.exists(fp) and os.path.exists(fp2):
        log_complaint("strange, schema found both in %s and %s" % (fp, fp2))
        error_seen = True
    if os.path.exists(fp):
        read_error = False
        read_error, base_attributes, base_relations, base_schema = read_attributes(fp)
        all_attributes[fp] = base_attributes
        all_relations[fp] = base_relations
        if read_error:
            error_seen = True
        for attribute in base_attributes:
            my_att = base_attributes[attribute]
            err = check_file_attribute(my_att, fp)
            if err:
                error_seen = True
        # print(base_schema)
    elif os.path.exists(fp2):
        read_error = False
        read_error, base_attributes, base_relations, base_schema = read_attributes(fp2)
        all_attributes[fp2] = base_attributes
        all_relations[fp2] = base_relations
        if read_error:
            error_seen = True
        for attribute in base_attributes:
            my_att = base_attributes[attribute]
            err = check_file_attribute(my_att, fp2)
            if err:
                error_seen = True
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
                log_info("Attention: hidden file %s" % filepath)

            err = False
            err, att, rel, schema = read_attributes(fp)
            if err:
                error_seen = True
            attributes[filename] = att
            relations[filename] = rel
            base_attributes.update(att)
            base_relations.update(rel)
            if schema != {}:
                log_error("file %s: second schema defined" % (filepath, schema))
                error_found = True
            n_relations = len(rel.keys())
            if n_relations > 1:
                log_info(
                    "file %s: strange, found more than one relation (%d)"
                    % (filepath, n_relations)
                )
                problem_seen = True
            for attribute in att:
                my_att = att[attribute]
                err = check_file_attribute(my_att, filepath)
                if err:
                    error_seen = True
            # for relation in rel:
            #    my_rel = rel[relation]
            #    check_relation(my_rel, att, filepath, base_attributes)

    basedir = os.path.join(os.environ["ANTELOPE"] + "/contrib/data/schemas/" + extpath)
    if os.path.exists(basedir):
        directory = os.fsencode(basedir)
        extension_attributes = {}
        # first read the list of extension attributes
        for myfile in os.listdir(directory):
            filename = os.fsdecode(myfile)
            fp = os.path.join(directory, myfile)
            filepath = fp.decode()
            if filename.startswith("."):
                log_info("Attention: hidden file %s" % filepath)
                problem_seen = True
            err = False
            err, att, __, __ = read_attributes(fp)
            if err:
                error_seen = True
            extension_attributes.update(att)
        # and then read in everything
        for myfile in os.listdir(directory):
            filename = os.fsdecode(myfile)
            fp = os.path.join(directory, myfile)
            filepath = fp.decode()
            if filename.startswith("."):
                log_info("Attention: hidden file %s" % filepath)
            err = False
            err, att, rel, schema = read_attributes(fp)
            if err:
                error_seen = True
            attributes[filename] = att
            all_attributes[fp] = att
            relations[filename] = rel
            all_relations[fp] = rel

            if schema != {}:
                log_info("file %s: second schema defined" % (filepath, schema))
                error_seen = True
            n_relations = len(rel.keys())
            if n_relations > 1:
                log_info(
                    "strange, found more than one relation (%d) in file %s"
                    % (n_relations, filepath)
                )
                error_seen = True
            for attribute in att:
                my_att = att[attribute]
                err = check_file_attribute(my_att, filepath)
                if err:
                    error_seen = True
            for relation in rel:
                my_rel = rel[relation]

                other_attributes = copy.deepcopy(extension_attributes)
                # for attribute in att:
                #    other_attributes.pop(attribute)
                err = check_file_relation(
                    my_rel, att, filepath, base_attributes, other_attributes
                )
                if err:
                    error_seen = True
    err = compare_all_attributes(all_attributes)
    if err:
        error_seen = True
    return error_seen


def compare_attribute(first, second):
    """compare arrtibute definitions
    type, length, format, units, range, null, description, detail
    """
    error_seen = False
    name = first["attribute"]
    f_file = first["file"].decode()
    s_file = second["file"].decode()
    f_type = first["type"]
    s_type = second["type"]
    f_len = first["len"]
    s_len = second["len"]
    f_format = None
    s_format = None
    if f_type != s_type:
        error_seen = True
        log_error(
            "attribute '%s': TYPE '%s' != '%s' (defined in files %s and %s)"
            % (name, f_type, s_type, f_file, s_file)
        )
    if f_len != s_len:
        error_seen = True
        log_error(
            "attribute '%s': LENGTH '%d' != '%d' (defined in files %s and %s)"
            % (name, f_len, s_len, f_file, s_file)
        )

    for thing in ["format", "units", "range", "null", "description", "detail"]:
        f_thing = None
        s_thing = None
        if thing in first:
            f_thing = first[thing]
        if thing in second:
            s_thing = second[thing]
        if f_thing is not None and s_thing is not None:
            if f_thing != s_thing:
                if thing == "detail":
                    f_desc = f_thing.strip()
                    s_desc = s_thing.strip()
                    f_unif = f_desc.replace(" ", "").replace("\t", "").replace("\n", "")
                    s_unif = s_desc.replace(" ", "").replace("\t", "").replace("\n", "")
                    if f_desc != s_desc:
                        if f_unif == s_unif:
                            log_info(
                                "attribute '%s': whitespace differs in DETAIL between files %s and %s"
                                % (name, f_file, f_file)
                            )
                        else:
                            log_error(
                                "attribute '%s': DETAIL definitions \n'%s'\nand \n'%s'\ndiffer in files %s and %s)"
                                % (name, f_desc, s_desc, f_file, s_file)
                            )
                else:
                    error_seen = True
                    log_error(
                        "attribute '%s': %s definitions '%s' and '%s' differ in files %s and %s)"
                        % (name, thing.upper(), f_thing, s_thing, f_file, s_file)
                    )
        elif f_thing is not None:
            error_seen = True
            log_error(
                "attribute '%s': %s '%s' defined in file %s but not in file %s)"
                % (name, thing.upper(), f_thing, f_file, s_file)
            )
        elif s_thing is not None:
            error_seen = True
            log_error(
                "attribute '%s': %s '%s' not defined in file %s but in file %s)"
                % (name, thing.upper(), s_thing, f_file, s_file)
            )
    return error_seen


def compare_all_attributes(all_attributes):
    error_seen = False
    a_checked = []
    files = all_attributes.keys()
    for file in files:
        f_atts = all_attributes[file]
        for a_name in f_atts.keys():
            if not a_name in a_checked:
                a_checked.append(a_name)
                this_attribute = f_atts[a_name]
                for other_file in files:
                    if other_file != file:
                        of_atts = all_attributes[other_file]
                        if a_name in of_atts:
                            of_attribute = of_atts[a_name]
                            err = compare_attribute(this_attribute, of_attribute)
                            if err:
                                error_seen = True
        # print("fatt", f_atts, f_atts.keys())
        # print("myfile: ", file)
    return error_seen


def check_file_relation(rel, att, filename, base_attributes, other_attributes):
    error_seen = False
    name = ""
    if "relation" in rel:
        name = rel["relation"]
    else:
        error_seen = True
        log_complaint("file %s: nameless relation %s" % (filename, rel.keys()))
        return
    if not "fields" in rel:
        error_seen = True
        log_complaint("file %s: no fields in relation %s" % (filename, name))
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
                error_seen = True
                log_complaint("file %s: attribute '%s' undefined" % (filename, field))
            else:
                dfile = ""
                if "file" in other_attributes[field]:
                    dfile = other_attributes[field]["file"].decode()
                else:
                    error_seen = True
                    log_complaint(
                        "file %s: HELP! Unable to find filename for attribute '%s'"
                        % (filename, field)
                    )
                error_seen = True
                log_complaint(
                    "file %s: attribute '%s' defined in another extension (%s)"
                    % (filename, field, dfile)
                )
    return error_seen


def check_file_attribute(att, filename):
    name = ""
    atype = ""
    length = -1
    error_seen = False
    if "attribute" in att:
        name = att["attribute"]
    else:
        log_complaintt("nameless attribute '%s'" % att.keys())
        return
    if "len" in att:
        length = att["len"]
    else:
        error_seen = True
        log_complaint(
            "%s: no lenght defined for attribute '%s', giving up format check for this attribute"
            % (filename, name)
        )
        return
    if "type" in att:
        atype = att["type"]
    else:
        error_seen = True
        log_complaint(
            "file %s: no type defined for attribute '%s', giving up format check for this attribute"
            % (filename, name)
        )
        return

    if "format" in att:
        ftype = ""
        formatstr = att["format"]
        if len(formatstr) < 2:
            error_seen = True
            log_complaint(
                "file %s: format specification '%s' too short for attribute '%s'"
                % (filename, formatstr, name)
            )
        if atype != "dbptr":
            fmtmatch = re.match(r"%(.*\d+)([a-z]+)", formatstr)
            if fmtmatch:
                fmt_nr = fmtmatch.group(1)
                l_ftype = fmtmatch.group(2)
                ftype = l_ftype[-1]
                if len(l_ftype) == 2 and l_ftype[0] != "l":
                    error_seen = True
                    log_complaint(
                        "file %s: suspicious length specification '%s' in format for attribute '%s'"
                        % (filename, formatstr, name)
                    )
                len_from_format = abs(int(float(fmt_nr)))
                if len_from_format != length:
                    error_seen = True
                    log_complaint(
                        "file %s: length mismatch %d != '%s' for attribute '%s'"
                        % (filename, length, formatstr, name)
                    )
            else:
                error_seen = True
                log_complaint(
                    "file %s: suspicious format '%s' for attribute '%s'"
                    % (filename, formatstr, name)
                )

        if not formatstr.startswith("%"):
            error_seen = True
            log_complaint(
                "file %s: format %s for attribute '%s' should start with '%%'"
                % (filename, formatstr, name)
            )
        if atype == "int" and ftype != "d":
            error_seen = True
            log_complaint(
                "file %s: format type mismatch for attribute '%s' ('%s' != %s)"
                % (filename, name, formatstr, atype)
            )
        if atype == "real" and ftype not in "efg":
            error_seen = True
            log_complaint(
                "file %s: format type mismatch for attribute '%s' ('%s' != %s)"
                % (filename, name, formatstr, atype)
            )
        if atype == "string" and ftype != "s":
            error_seen = True
            log_complaint(
                "file %s: format type mismatch for attribute '%s' ('%s' != %s)"
                % (filename, name, formatstr, atype)
            )
        if atype == "time" and ftype != "f":
            error_seen = True
            log_complaint(
                "file %s: format type mismatch for attribute '%s' ('%s' != %s)"
                % (filename, name, formatstr, atype)
            )
    else:
        error_seen = True
        log_complaint(
            "file %s: no format specification for attribute '%s'" % (filename, name)
        )

    if "null" in att:
        fnull = att["null"]
        if atype == "real":
            if not "." in fnull:
                error_seen = True
                log_complaint(
                    "file %s: NULL specification for attribute '%s' should be a float instead of '%s'"
                    % (filename, name, fnull)
                )
    else:
        problem_seen = True
        log_info("file %s: no NULL for attribute '%s'." % (filename, name))

    if "range" in att:
        frange = att["range"]
        if name not in frange:
            error_seen = True
            log_complaint(
                "file %s: meaningless RANGE check for attribute '%s'. Attribute not mentioned in definition of RANGE '%s'"
                % (filename, name, frange)
            )
    else:
        problem_seen = True
        log_info("file %s: no RANGE for attribute '%s'." % (filename, name))

    if not "detail" in att:
        error_seen = True
        log_info("file %s: no DETAIL for attribute '%s'." % (filename, name))
    if not "description" in att:
        error_seen = True
        log_complaint("file %s: no DESCRIPTION for attribute '%s'." % (filename, name))

    return error_seen


def check_keys(tablename, fields, keys, ktype):
    error_seen = False
    """see if keys are defined"""
    if len(keys) > 0:
        for key in keys:
            if "::" in key:
                for sub_key in key.split("::"):
                    if sub_key not in fields:
                        eror_seen = True
                        log_info(
                            "%s key '%s' (%s) not found in table %s"
                            % (ktype, sub_key, key, tablename)
                        )
            elif key not in fields:
                error_seen = True
                log_error("%s key '%s' not found in table %s" % (ktype, key, tablename))
        return error_seen
    else:
        return True  # I think it is an error if a table has no keys


def check_table_keys(table):
    error_seen = False
    tablename = table.query(ds.dbTABLE_NAME)
    fields = table.query(ds.dbTABLE_FIELDS)
    primary_keys = table.query(ds.dbPRIMARY_KEY)
    alternate_key = table.query(ds.dbALTERNATE_KEY)
    foreign_keys = table.query(ds.dbFOREIGN_KEYS)
    err = False
    err = check_keys(tablename, fields, primary_keys, "Primary")
    if err:
        error_seen = True
    err = check_keys(tablename, fields, alternate_key, "Alternate")
    if err:
        error_seen = True
    err = check_keys(tablename, fields, foreign_keys, "Foreign")
    if err:
        error_seen = True
    return error_seen


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

    error_seen = False
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
                break  # we found the field in a table
        if not_found:
            log_complaint("attribute '%s': defined but not used in any table" % field)
            error_seen = True
            return

        dbq = dbt.lookup(field=field, record="dbNULL")
        flen = dbq.query(ds.dbFIELD_SIZE)
        try:
            fformat = dbq.query(ds.dbFIELD_FORMAT)
        except Exception as __:
            log_info("attribute '%s': no FORMAT" % field)
            error_seen = True
        funits = dbq.query(ds.dbFIELD_UNITS)
        ftype = dbq.query(ds.dbFIELD_TYPE)
        # fnull = dbq.query(sd.dbNULL)
        try:
            [fnull] = dbq.getv(field)
        except Exception as __:
            log_info("attribute '%s': no NULL" % field)
            error_seen = True

        if fnull == "":
            log_complaint("attribute '%s': empty NULL value" % field)

        if not funits or funits == "":
            log_info("attribute '%s': no UNITS" % field)
            error_seen = True
    if fformat and fformat != "" and ftype and ftype != "":
        pass
    if fnull and fnull != "" and fformat and fformat != "":
        if ftype != ds.dbDBPTR:
            try:
                testval = fformat % fnull
            except Exception as __:
                error_seen = True
                log_complaint(
                    "attribute '%s': problem formatting NULL value '%s' or null '%s'"
                    % (field, fformat, fnull)
                )
            str_null = str(fnull).strip()
            testval = testval.strip()
            if len(str_null) > len(testval):
                error_seen = True
                log_complaint(
                    "attribute '%s': NULL value '%s' does not fit into field when unsing format specification '%s'"
                    % (field, fnull, fformat)
                )
            if testval != str_null:
                log_info(
                    "attribute '%s': NULL value does not match formatted NULL: '%s' != %s"
                    % (field, fnull, testval)
                )


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
    global pedantic

    tablename = None
    verbose = False
    schema_checks = True
    file_checks = True
    error_seen = False
    opts = []
    args = []
    try:
        opts, args = getopt.getopt(sys.argv[1:], "avfs", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        return 2

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-a":
            pedantic = True
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
        err = False
        err, check_schema_files(schemaname)
        if err:
            error_seen = True
    # 2nd check on the object in memory
    if schema_checks:
        # trange enough, try... does not work with dbtmp
        db = ds.dbtmp(schemaname)
        # but fortunately it retruns dbinvalid in case of a problem
        if db == ds.dbinvalid():
            # elog.flush(False, -1)
            elog.die(
                "problem with database schema '%s'. Maybe the schema does not exist?"
                % schemaname
            )
        try:
            name = db.query(ds.dbSCHEMA_NAME)
        except Exception as __:
            elog.flush(False, 0)
            elog.die("problen with database schema %s" % schemaname)
        with ds.destroying(ds.dbtmp(schemaname)) as db:
            if tablename:
                try:
                    dbt = db.lookup(table=tablename)
                except Exception as __:
                    elog.die(
                        "table %s not found in schema %s" % (tablename, schemaname)
                    )
                err = check_table_keys(dbt)
                if err:
                    error_seen = True
                for fieldname in sorted(dbt.query(ds.dbTABLE_FIELDS)):
                    check_attribute(dbt, fieldname)
            else:
                for tablename in sorted(db.query(ds.dbSCHEMA_TABLES)):
                    dbt = db.lookup(table=tablename)
                    err = check_table_keys(dbt)
                    if err:
                        error_seen = True
                for fieldname in sorted(db.query(ds.dbSCHEMA_FIELDS)):
                    check_attribute(db, fieldname)

    if error_seen:
        return -1
    else:
        log_info("not a single problem in schema '%s'" % schemaname)
        return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
