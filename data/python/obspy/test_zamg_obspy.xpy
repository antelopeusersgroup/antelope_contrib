
# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog

sys.path.append(os.environ['ANTELOPE'] + "/contrib/data/python")
import zamg.missing as zm
import zamg.db2obspy as zop # imports all required ObsPy modules

def main():
    dbname = "/opt/antelope/data/db/demo/demo"
    db = ds.dbopen(dbname, "r")
    wf = db.lookup(table="wfdisc")
    snet = db.lookup(table="snetsta")

    sta = "TRO"
    chan = "HHZ"
    timestr = "2016-01-12 03:29:00"
    etime = stock.str2epoch(timestr)
    stream = zop.stachan2stream(sta, chan, etime, etime + 120., db)

    mycat = zop.db2catalog(db,prefor_only = True)
    print(mycat)
    dbe = db.lookup(table="event")
    dbs=dbe.subset("evid == 1")
    mycat = zop.db2catalog(dbs,prefor_only = True)
    print(mycat)
    print(stream)
    stream[0].spectrogram(log=True)  
    stream[0].plot()
    """
    xmlfile="demo.xml"
    zop.ppsd(stream, sta, chan, xmlfile)
    """
    return 0

if __name__ == "__main__":
    status = main()
    sys.exit(status)
