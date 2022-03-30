import zamg._missing as _missing


def dbget_calib(sta, chan, time, dbname):
    """lookup calib, calper, segtype from calibration database"""
    ret = _missing._dbget_calib(sta, chan, time, dbname)
    return ret

def TIME2SAMP(starttime, samprate, endtime):
    """number of samples from start- and endtime"""
    ret = _missing._TIME2SAMP(starttime, samprate, endtime)
    return ret

def SAMP2TIME(starttime, samprate, nsamp):
    """endtime from starttime, sampling rate and number of samples"""
    ret = _missing._SAMP2TIME(starttime, samprate, nsamp)
    return ret

def SAMPRATE(starttime, nsamp, endtime):
    """sampling rate from number of samples between start- and endtime"""
    ret = _missing._SAMPRATE(starttime, nsamp, endtime)
    return ret

def NSAMP(starttime, samprate, endtime):
    """number of samples from sampling rate, start- and endtime"""
    ret = _missing._NSAMP(starttime, samprate, endtime)
    return ret

def ENDTIME(starttime, samprate, nsamp):
    """endtime of samples from starttime, sampling rate and number of samples"""
    ret = _missing._ENDTIME(starttime, samprate, nsamp)
    return ret

def dbget_remark(db):
    """return remark for specified record and table"""
     # Ugly but clear: Kent says, for C, a database is only a list
    mydb = [db.database, db.table, db.field, db.record]
    remark = _missing._dbget_remark(mydb)
    return remark

def dbadd_remark(db, remark):
    """add remark for specified record and table. Returns the new commid"""
     # Ugly but clear: Kent says, for C, a database is only a list
    mydb = [db.database, db.table, db.field, db.record]
    commid = _missing._dbadd_remark(mydb, remark)
    return commid


