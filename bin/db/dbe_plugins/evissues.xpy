""" Event-Probleme

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     June 11, 2020
@version     2.1
@license     MIT-style license

"""
import getopt
import math
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
import antelope.sysinfo as sysinfo


def usage(progname):
    print(progname, " list issue(s): [-v] [-o] database[.table]")
    print(
        progname,
        " add entry:     [-v] -e evid   (-I issue|-E|-D|-C|-M) [-s source] [-c checktime] [-a auth] database[.table]",
    )
    print(
        progname,
        " fix:           [-v] -f fixtime -e evid  (-I issue|-E|-D|-C|-M) [-a auth] [-s source] database[.table]",
    )
    print(
        progname,
        " -E - etype| -D - depth | -C - lat/lon | -M magnitude"
    ) 


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init(progname)

    verbose = False
    action = ""
    recno = -1
    dboutname = ""
    show = True
    show_open_only = False
    pfname = progname
    issue = ""
    source = ""
    fixtime = checktime = -9999999999.99900
    auth = ""
    fix = False
    add = False
    evid = -1

    try:
        opts, args = getopt.getopt(sys.argv[1:], "vhoe:I:EDCMs:c:a:f:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")
        sys.exit(1)

    for option, argument in opts:
        if option == "-v":
            verbose = True
        elif option == "-h":
            usage(progname)
            sys.exit(0)
        elif option == "-o":
            show_open_only = True
            show = True
        elif option == "-e":
            evid = int(argument)
            show = False
        elif option == "-I":
            issue = argument
            show = False
            add = True
        elif option == "-a":
            auth = argument
        elif option == "-s":
            source = argument
        elif option == "-f":
            add = False
            fix = True
            fixtime_s = argument
            try:
                fixtime = stock.str2epoch(fixtime_s)
            except Exception as __:
                elog.die("problem with fix time %s" % fixtime_s)
        elif option == "-c":
            checktime_s = argument
            try:
                checktime = stock.str2epoch(checktime_s)
            except Exception as __:
                elog.die("problem with check time %s" % checktime_s)
        elif option == "-E":
            issue = "etype"
            show = False
            add = True
        elif option == "-D":
            issue = "depth"
            show = False
            add = True
        elif option == "-C":
            issue = "coordinates"
            show = False
            add = True
        elif option == "-M":
            issue = "magnitude"
            show = False
            add = True
        else:
            print("unknown option %s" % option)
            usage(progname)
            sys.exit(1)

    if len(args) < 1 or len(args) > 2:
        usage(progname)
        sys.exit(1)

    if len(args) > 1:
        recno = int(args[1])

    dbname = args[0]
    # need to open writable database BEFORE any other database.
    # That's a known bug in the Python interface
    db = ds.dbopen_database(dbname, "r+")
    dbt = db.lookup(table="evissues")
    if show:
        if show_open_only:
            dbs = dbt.subset("open !~/n/")
        else:
            dbs = dbt
        print("%6s %10s %15s %20s %16s %16s %s" % ("State", "evid", "issue", "source", "checked", "fixed", "auth"))
        for dbs.record in range(dbs.record_count):
            [i_evid, i_issue, i_source, i_checked, i_fixed, i_auth] = dbs.getv(
                "evid",
                "issue",
                "source_of_current_parameter",
                "time_checked",
                "time_fixed",
                "auth",
            )
            still_open = dbs.ex_eval(" open == NULL || open !~ /n/")
            if still_open:
                s_open = "Open"
                s_fixed = " - "
            else:
                s_open = "Fixed"
                s_fixed = stock.epoch2str(i_fixed, "%Y-%m-%d %H:%M")
            s_checked = stock.epoch2str(i_checked, "%Y-%m-%d %H:%M")
            print(
                "%6s: %10d %15s %20s %16s %16s %s"
                % (s_open, i_evid, i_issue[:15], i_source[:20], s_checked, s_fixed, i_auth)
            )
    elif fix:
        if evid < 0:
            elog.die("you must specify an event id")
        if issue == "":
            elog.die("you must specify an issue")
        if fixtime < 0:
            fixtime = stock.now()
        if source == "":
            source = "just so"
        if auth == "":
            auth = sysinfo.my_username()
        try:
            recno = dbt.find("evid == %d && issue =~/%s/" % (evid, issue))
        except Exception as e:
            elog.flush(False,0)
            elog.notify("here evid %d and issue %s not found" % (evid, issue))
            elog.die("%s: evid %d and issue %s not found" % (e, evid, issue))
            sys.exit(1)
        if recno < 0:
            elog.notify("evid %d and issue %s not found" % (evid, issue))
            return -1
        dbt.record = recno
        still_open = dbt.ex_eval(" open == NULL || open !~ /n/")
        [i_evid, i_issue, i_source, i_checked, i_fixed, i_auth] = dbt.getv(
            "evid",
            "issue",
            "source_of_current_parameter",
            "time_checked",
            "time_fixed",
            "auth",
        )
        s_fixed = stock.epoch2str(i_fixed, "%Y-%m-%d %H:%M")
        if still_open:
            dbt.putv(
                ("open", "n"),
                ("time_fixed", fixtime),
                ("source_of_current_parameter", source),
                ("auth", auth),
            )
        else:
            elog.notify("issue %s was fixed %s" % (issue, s_fixed))
    elif add:
        if evid < 0:
            elog.die("you must specify an event id")
        if issue == "":
            elog.die("you must specify an issue")
        if checktime < 0:
            checktime = stock.now()
        if source == "":
            source = "just so"
        if auth == "":
            __, auth = sysinfo.my_username()
        try:
            dbt.addv(
                ("evid", evid),
                ("issue", issue),
                ("open", "y"),
                ("time_checked", checktime),
                ("source_of_current_parameter", source),
                ("auth", auth),
            )
        except Exception as e:
            elog.die("problem adding issue %s" % e)

    else:
        elog.die("dunno what to do...")
        return -1

    return 0    

if __name__ == "__main__":
    status = main()
    sys.exit(status)
