"""
@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     Jun 14, 2022
@version     1.0
@license     MIT-style license

"""

from operator import itemgetter


# Import Antelope modules

import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
import getopt


def usage(progname):
    print(progname, "[-v] [-s expr] [-f x] [-b pfname] pfname dbmaster")


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init()

    verbose = False
    show_backups = False
    opts = []
    args = []
    mode = "q3302orb"
    fraction = 2
    sort_expression = "sta"  # alphabetical
    alternate_suffix = "2"
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vs:f:b:", "")
    except getopt.GetoptError:
        usage(progname)
        elog.die("Illegal option")

    for o, a in opts:
        if o == "-v":
            verbose = True
        elif o == "-s":
            sort_expression = a
        elif o == "-f":
            fraction = int(a)
        elif o == "-b":
            show_backups = True
            connections_pfname = a
        elif o == "-h":
            usage(progname)
            sys.exit(0)

    if len(args) > 2 or len(args) < 2:
        usage(progname)
        sys.exit(1)

    pfname = args[0]
    dbname = args[1]

    etime = stock.now()
    db = ds.dbopen(dbname, "r")
    dbsite = db.lookup(table="site")
    dbsite = dbsite.subset("offdate == NULL || offdate > %f" % etime)
    dbsite = dbsite.sort(sort_expression)
    stalist = []
    pfitems = []
    # pf = stock.pfin(pfname)
    if show_backups:
        bpf = stock.pfread(connections_pfname)
        stations_array = bpf["stations"]
    pf = stock.pfread(pfname)
    if pf.has_key("connections"):
        mode = "orb2orb"
        connections = pf["connections"]
        for connection in connections:
            if "name" in connection:
                sta = connection["name"]
                try:
                    order = dbsite.find("sta == '%s'" % sta, first=-1)
                except Exception as __:
                    order = -1
                pfitems.append(dict(sta=sta, order=order, pfstuff=connection))
            else:
                print("no name found")
                print(connection)

    elif pf.has_key("dataloggers"):
        mode = "q3302orb"
        dls = pf["dataloggers"]
        for dl in dls:
            dlname, net, sta, __ = dl.split(None, 3)
            try:
                order = dbsite.find("sta == '%s'" % sta, first=-1)
            except Exception as __:
                order = -1
            pfitems.append(dict(sta=sta, order=order, pfstuff=dl))

    elif pf.has_key("stations"):
        mode = "slink2orb"
        stalist = pf["stations"]
        for netsta in stalist.keys():
            net, sta = netsta.split("_", 1)
            try:
                order = dbsite.find("sta == '%s'" % sta, first=-1)
            except Exception as __:
                order = -1
            line = "%s %s" % (netsta, stalist[netsta])
            pfitems.append(
                dict(
                    sta=sta,
                    order=order,
                    pfstuff=line,
                    netsta=netsta,
                    selectors=stalist[netsta],
                )
            )

    elif pf.has_key("Processes"):
        mode = "rtexec"
        jobs = pf["Processes"]
        for job in jobs:
            name, cmdline = job.split(None, 1)
            if name.startswith("slink2orb"):
                sta = name.replace("slink2orb", "")
                try:
                    order = dbsite.find("sta == '%s'" % sta, first=-1)
                except Exception as __:
                    order = -1
                pfitems.append(dict(sta=sta, order=order, pfstuff=job))

    slist = sorted(pfitems, key=itemgetter("order"))
    # slist = sorted(pfitems, key=itemgetter('order'), reverse=True)

    for i in range(fraction):
        pfarr = []
        pfdict = {}
        pfarr_backup = []
        pfdict_backup = {}
        start = i
        index = 0
        for pf in slist:
            if ((start + index) % fraction) == 0:
                txt = pf["pfstuff"]
                if mode == "rtexec" or mode == "q3302orb":
                    pfarr.append(txt)
                elif mode == "slink2orb":
                    netsta = pf["netsta"]
                    selectors = pf["selectors"]
                    pfdict[netsta] = selectors
                elif mode == "orb2orb":
                    pfarr.append(pf["pfstuff"])
                if show_backups:
                    sta = pf["sta"]
                    if sta in stations_array:
                        sta_cons = stations_array[sta]
                        if "backup" in sta_cons:
                            ip = sta_cons["primary"]
                            ip2 = sta_cons["backup"]
                            if mode != "orb2orb":
                                ntxt = txt.replace(ip, ip2)
                                ntxt = ntxt.replace(sta, sta + alternate_suffix, 1)
                            if mode == "rtexec" or mode == "q3302orb":
                                pfarr_backup.append(ntxt)
                            elif mode == "orb2orb":
                                tmpdict = pf["pfstuff"]
                                newdict = {}
                                for key in tmpdict.keys():
                                    val = tmpdict[key]
                                    newval = val.replace(ip, ip2)
                                    newval = newval.replace(
                                        sta, sta + alternate_suffix, 1
                                    )
                                    newdict[key] = newval
                                pfarr_backup.append(newdict)

            index += 1
        if mode == "rtexec":
            newpf = stock.ParameterFile()
            newpf.update(dict(Processes=pfarr))
            pfname = "rtexec-%d.pf" % (i + 1)
            newpf.pfwrite(pfname)
            if verbose:
                print("wrote %s to new parameter file %s" % ("Processes", pfname))
            if show_backups:
                newpf = stock.ParameterFile()
                newpf.update(dict(Processes=pfarr_backup))
                pfname = "rtexec-%d-backup.pf" % (i + 1)
                newpf.pfwrite(pfname)
                if verbose:
                    print("wrote %s to new parameter file %s" % ("Processes", pfname))
        elif mode == "q3302orb":
            newpf = stock.ParameterFile()
            newpf.update(dict(dataloggers=pfarr))
            pfname = "q3302orb-%d.pf" % (i + 1)
            newpf.pfwrite(pfname)
            if verbose:
                print("wrote %s to new parameter file %s" % ("dataloggers", pfname))
            if show_backups:
                newpf = stock.ParameterFile()
                newpf.update(dict(dataloggers=pfarr_backup))
                pfname = "q3302orb-%d-backup.pf" % (i + 1)
                newpf.pfwrite(pfname)
                if verbose:
                    print("wrote %s to new parameter file %s" % ("dataloggers", pfname))
        elif mode == "slink2orb":
            newpf = stock.ParameterFile()
            newpf.update(dict(stations=pfdict))
            pfname = "slink2orb-%d.pf" % (i + 1)
            newpf.pfwrite(pfname)
            if verbose:
                print("wrote %s to new parameter file %s" % ("stations", pfname))
        elif mode == "orb2orb":
            newpf = stock.ParameterFile()
            newpf.update(dict(connections=pfarr))
            pfname = "orb2orb-%d.pf" % (i + 1)
            newpf.pfwrite(pfname)
            if verbose:
                print("wrote %s to new parameter file %s" % ("connections", pfname))
            if show_backups:
                newpf = stock.ParameterFile()
                newpf.update(dict(connections=pfarr_backup))
                pfname = "orb2orb-%d-backup.pf" % (i + 1)
                newpf.pfwrite(pfname)
                if verbose:
                    print("wrote %s to new parameter file %s" % ("connections", pfname))


if __name__ == "__main__":
    status = main()
    sys.exit(status)
