"""
#   Copyright (c) 2019,2021 Nikolaus Horn, ZAMG
#
#   Written by Nikolaus Horn
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.

unsorted helper functions
"""

import os
import math
import numpy as np
import antelope.stock as stock
import antelope.datascope as ds
import antelope.elog as elog


def init_statefile(filename):
    """initialize a simplistic statefile consisting of an id and a timestamp. returns lastid and lasttime"""
    mystruct = {}
    if os.path.isfile(filename):
        mypf = stock.pfin(filename)
        if mypf.has_key("lastid"):
            lastid = int(mypf["lastid"])
        else:
            lastid = -1
        if mypf.has_key("lasttime"):
            lasttime = stock.str2epoch(mypf["lasttime"])
        else:
            lasttime = 0.0
    else:
        lastid = -1
        lasttime = 0.0
    return lastid, lasttime


def update_statefile(filename, lastid, lasttime):
    """update time and id in statefile"""
    pfobj = stock.ParameterFile()
    pfobj.update(dict(lastid=lastid))
    pfobj.update(dict(lasttime=lasttime))
    with open(filename, "w", encoding="utf8") as mypffile:
        try:
            mypffile.write(pfobj.pf2string())
        except Exception as __:
            elog.notify("problem with statepf.pfout")


def utf8len(my_string):
    """length of utf8-encoded string in bytes"""
    try:
        """encoded length, works if there are no problems with the encodung"""
        return len(my_string.encode("utf-8", errors="replace"))
    except UnicodeDecodeError as __:
        elog.complain("problem encoding %s in utf8" % my_string)
        """if there are problems, return length of unencoded string"""
        return len(my_string)


def string_charsplit(my_string, bytelen):
    """
     split into lines of maximal bytelen bytes

     Args:
         my_string (string): input
         bytelen (int):      maximum length of encoded line in bytes
    Returns:
         array of lines - not encoded
    """
    offset = 0
    lines = []
    while utf8len(my_string[offset:]) > bytelen:
        slen = 0
        # if the encoded version with one char plus is still short enough...
        while utf8len(my_string[offset : offset + slen + 1]) < bytelen:
            slen += 1
        lines.append(my_string[offset : offset + slen])
        offset += slen
    if offset < len(my_string):
        lines.append(my_string[offset:])
    return lines


def string_maxbytes(my_string, bytelen):
    """chop encoded string into characters, not bytes"""
    chars = 1
    while utf8len(my_string[:chars]) <= bytelen and chars <= len(my_string):
        chars += 1
    return my_string[: chars - 1]


def create_dbdesc(dbpath, dbschema, tablename=None):
    """create database descriptor. Returns database pointer to a table"""
    kdb = ds.dbopen(dbpath, "r+")
    descname = kdb.query("dbDATABASE_FILENAME")
    if os.path.exists(descname):
        schemaname = kdb.query("dbSCHEMA_NAME")
        if schemaname != dbschema:
            elog.complain(
                "database %s already exists but has wrong schema %s instead of %s"
                % (dbpath, schemaname, dbschema)
            )
            return None
    else:
        kdb.close()
        ds.dbcreate(dbpath, dbschema)

        kdb = ds.dbopen(dbpath, "r+")
    if tablename != None:
        try:
            idmatch = kdb.lookup(table=tablename)
        except Exception as __:
            elog.complain(
                "table %s does not exist in schema for database %s" % (tablename, dbpath)
            )
            return None
        return idmatch
    else:
        return kdb


def add_remark(db, remark):
    """add remark to designated table and record"""
    if db.table < 0 or db.record < 0:
        elog.complain("cannot add remark %s to unspecified record" % remark)
    try:
        db_r = db.lookup(table="remark")
    except Exception as __:
        elog.notify("cannot lookup remark table for remark %s" % remark)
    try:
        commid = db.nextid("commid")
    except Exception as __:
        elog.notify("cannot get new commid for remark %s" % remark)
        return -1
    dbq = db_r.lookup(field="remark", record="dbNULL")
    remark_width = dbq.query(ds.dbFIELD_SIZE)
    remark_lines = string_charsplit(remark, remark_width)
    lineno = 0
    for line in remark_lines:
        try:
            db_r.addv(("commid", commid), ("lineno", lineno), ("remark", line))
        except Exception as __:
            elog.notify("cannot add remark %s" % remark)
            return -1
        lineno += 1

    try:
        db.putv(("commid", commid))
    except ds.DbputvError as __:
        elog.complain(
            "add_remark: problem updating commid in base table after adding remark"
        )
        return -1
    return commid


def set_remark(db, remark):
    """set remark to designated table and record. If it makes sense, entries are updated"""

    """ sanity checks """
    if db.table < 0 or db.record < 0:
        elog.complain("cannot add remark %s to unspecified record" % remark)
    try:
        db_r = db.lookup(table="remark")
    except Exception as __:
        elog.notify("cannot lookup remark table for remark %s" % remark)

    try:
        [commid] = db.getv("commid")
    except Exception as __:
        elog.notify("cannot retrieve commid")
        return -1
    if commid < 0:  # seems we have no entry yet
        commid = add_remark(db, remark)
        return commid
    else:
        db_s = db_r.sort(["commid", "lineno"])
        matcher = db.matches(db_s, kpattern="commid", tpattern="commid")
        previous_lines = matcher()
        npl = len(previous_lines)
        dbq = db_r.lookup(record="dbNULL")
        [remark_null, lddate_null, commid_null, lineno_null] = dbq.getv(
            "remark", "lddate", "commid", "lineno"
        )
        dbq = db_r.lookup(field="remark", record="dbNULL")
        remark_width = dbq.query(ds.dbFIELD_SIZE)
        remark_lines = string_charsplit(remark, remark_width)
        nrl = len(remark_lines)
        lineno = 0
        for lineno in range(nrl):
            rline = remark_lines[lineno]
            if lineno < npl:
                db_r.record = previous_lines[lineno]
                db_r.putv(("commid", commid), ("lineno", lineno), ("remark", rline))
            else:
                try:
                    db_r.addv(("commid", commid), ("lineno", lineno), ("remark", rline))
                except Exception as __:
                    elog.notify("new add: cannot add remark %s" % remark)
                    return -1
        if nrl < npl:
            for xl in range(nrl, npl):
                db_r.record = previous_lines[xl]
                db_r.putv(
                    ("lineno", lineno_null),
                    ("commid", commid_null),
                    ("remark", remark_null),
                    ("lddate", lddate_null),
                )


def get_remark(db):
    """get remark from designated table and record"""
    remark = ""
    if db.table < 0 or db.record < 0:
        elog.complain("cannot retrieve remark from unspecified record")
        return remark
    try:
        db_r = db.lookup(table="remark")
    except Exception as __:
        elog.notify("cannot lookup remark table")
        return remark
    try:
        [commid] = db.getv("commid")
    except Exception as __:
        elog.notify("cannot retrieve commid")
        return remark
    if commid < 0:
        elog.notify("commid useless")
        return remark
    db_r = db_r.sort(["commid", "lineno"])
    matcher = db.matches(db_r, kpattern="commid", tpattern="commid")
    records = matcher()
    if len(records) > 0:
        remarks = []
        for db_r.record in records:
            [line] = db_r.getv("remark")
            remarks.append(line)
        remark = "".join(remarks)
    return remark

def mark_remark(db):
    if db.table < 0 or db.record < 0:
        elog.complain("cannot clear remark from unspecified record")
    try:
        db_r = db.lookup(table="remark")
    except Exception as __:
        elog.notify("cannot lookup remark table")
    try:
        [commid] = db.getv("commid")
    except Exception as __:
        elog.notify("cannot retrieve commid")
    if commid >= 0:
        db_r = db_r.sort(["commid", "lineno"])
        matcher = db.matches(db_r, kpattern="commid", tpattern="commid")
        records = matcher()
        if len(records) > 0:
            for db_r.record in records:
                db_r.mark()

def rfc33392epoch(timestring):
    """convert internet timestamp in RFC3339 format. Returns normal antelope epoch time"""
    time_date = timestring.split("T")
    if len(time_date) != 2:
        time_date = timestring.split("t")
        if len(time_date) != 2:
            return None
    ymd = time_date[0].split("-")
    if len(ymd) != 3:
        return None
    year = int(ymd[0])
    month = int(ymd[1])
    day = int(ymd[2])
    offset = 0.0
    time_zone = time_date[1].split("-")
    if len(time_zone) == 2:
        time = time_zone[0]
        zone = time_zone[1].split(":")
        if zone != 2:
            return None
        offset = int(zone[0]) * 3600.0 + int(zone[1]) * 60.0
    else:
        time_zone = time_date[1].split("+")
        time = time_zone[0]
        if len(time_zone) > 2:
            return None
        if len(time_zone) == 2:
            if time_zone[1] != "00:00":
                [tz_hour, tz_min] = time_zone[1].split(":")
                offset = -1 * (int(tz_hour) * 3600 + int(tz_min) * 60)
        else:
            if time[-1:] == "Z":
                time = time[:-1]
            if time[-1:] == "z":
                time = time[:-1]
    hms = time.split(":")
    if len(hms) != 3:
        return None
    hour = int(hms[0])
    minute = int(hms[1])
    second = float(hms[2])
    epoch = stock.str2epoch(
        "%s/%s/%s %s:%s:%.6f" % (year, month, day, hour, minute, second)
    )
    epoch += offset
    return epoch


def epoch2rfc3339(*args):
    """epoch time as RFC3339 compatible string"""
    if len(args) == 1:
        return stock.epoch2str(args[0], "%Y-%M-%DT%H:%M:%S.%sZ")
    elif len(args) > 1:
        return stock.epoch2str(args[0], "%Y-%M-%DT%H:%M:%S.%sZ")


# def mydist2(lt
#    pvar=0.017453292519943295
#    dist[i]= 114.59148343 * np.arcsin(np.sqrt(0.5 - np.cos((stalat[i] - LA) * pvar)/2 + np.cos(LA * pvar) * np.cos(stalat[i] * pvar) * (1 - np.cos((stalon[i] - LO) * pvar)) / 2))
#


def spherical_distance(lat1, lon1, lat2, lon2, degrees=False):
    """great-arc distance on a sphere, either in degrees or km"""
    # lon1, lat1, lon2, lat2 = map(np.radians, [lon1, lat1, lon2, lat2])
    p_var = 0.017453292519943295

    dist = 114.59148343 * np.arcsin(
        np.sqrt(
            0.5
            - np.cos((lat1 - lat2) * p_var) / 2
            + np.cos(lat2 * p_var)
            * np.cos(lat1 * p_var)
            * (1 - np.cos((lon1 - lon2) * p_var))
            / 2
        )
    )

    if degrees:
        return dist
    else:
        return dist * 111.195


def haversine_distance(lat1, lon1, lat2, lon2, degrees=False):
    r = 6371.0  # WGS84
    phi1 = np.radians(lat1)
    phi2 = np.radians(lat2)
    delta_phi = np.radians(lat2 - lat1)
    delta_lambda = np.radians(lon2 - lon1)
    a = (
        np.sin(delta_phi / 2) ** 2
        + np.cos(phi1) * np.cos(phi2) * np.sin(delta_lambda / 2) ** 2
    )
    res = r * (2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a)))
    if degrees:
        return np.round(res / 111.195, 2)
    else:
        return np.round(res, 2)


def next_number(inp, unit=1, log=False):
    """Next number divisible by _unit_ or next power of _unit_"""
    q = int(inp / unit)
    nn = unit * (q + 1)
    return nn
