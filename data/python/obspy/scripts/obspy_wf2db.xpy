import getopt
import datetime

try:
    from obspy import read as op_read
except Exception as __:
    print(
        "cannot load obspy module. Maybe you need to install obspy (pip install obspy)?"
    )

# Import Antelope modules
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")
import zamg.utilities as zu
import zamg.missing as zm
import zamg.obspy2db as zo


def usage(progname):
    print(progname, "[-vd] [-c dbcalib] [-s 1|2] wf [wf2 [wf3 ...]] db")


progname = sys.argv[0].split("/")[-1]
elog.init(progname)
pfname = progname

overwrite = False
encoding = "STEIM2"
calibdbname = ""
verbose = False
debug = False
opts = []
args = []
try:
    opts, args = getopt.getopt(sys.argv[1:], "c:de:f:op:v", "")
except getopt.GetoptError:
    usage(progname)
    elog.die("Illegal option")
    sys.exit(2)

for o, a in opts:
    if o == "-v":
        verbose = True
    elif o == "-c":
        calibdbname = a
    elif o == "-d":
        debug = True
    elif o == "-s":
        if a == "1":
            encoding = "STEIM1"
        elif a == "2":
            encoding = "STEIM2"
        else:
            elog.die("unknown steim level %s for -s" % a)
    elif o == "-o":
        overwrite = True
    elif o == "-p":
        pfname = a


if len(args) < 2:
    usage(progname)
    sys.exit(1)

pf = stock.pfread(pfname)
wf_template = pf["wf_template"]
sta_trans = pf["sta_trans"]
chan_trans = pf["chan_trans"]
default_segtype = pf["segtype"]
datatype = pf["datatype"]
params = {}


dbname = args[-1]
if calibdbname == "":
    calibdbname = dbname
dbwfdisc = zu.create_dbdesc(dbname, "css3.0", "wfdisc")
net = "OE"
loc = ""
for ndx in range(len(args) - 1):
    filename = args[ndx]
    try:
        stream = op_read(filename)
    except Exception as __:
        if verbose:
            print("problem reading waveforms from %s" % filename)
        continue
    else:
        if verbose:
            print("reading waveforms from %s" % filename)
    for stachan in stream:
        stats = stachan.stats
        sps = stats.sampling_rate
        sta = stats.station
        chan = stats.channel

        for st in sta_trans:
            if sta == st:
                if debug:
                    print("\t%s->%s" % (st, sta_trans[st]))
                sta = sta_trans[st]
        for st in chan_trans:
            if chan == st:
                if debug:
                    print("\t%s->%s" % (st, chan_trans[st]))
                chan = chan_trans[st]
        if chan.find(" ") > -1:
            print("channel name contains spaces, replaced by underscore '_'")
            chan = chan.replace(
                " ", "_"
            )  # maybe better be translated above, but who knows...
        if chan.find(",") > -1:
            print("channel name contains commas, replaced by underscore '_'")
            chan = chan.replace(
                ",", "_"
            )  # maybe better be translated above, but who knows...
        # stachan.stats.sta = sta  # force renaming to updated sta and chan
        stachan.stats.station = sta  # force renaming to updated sta and chan
        # stachan.stats.chan = chan
        stachan.stats.channel = chan
        if debug:
            print("\tstats ", stachan.stats)

        nsamp = stats.npts
        samprate = stats.sampling_rate
        t1 = stachan.stats.starttime
        e1 = zo.obspy_datetime2epoch(t1)

        jdate = stock.yearday(e1)

        t2 = stachan.stats.endtime
        e2 = zo.obspy_datetime2epoch(t2)

        [calib, calper, st] = zm.dbget_calib(sta, chan, e1, calibdbname)
        if calib != 0.0:
            segtype = st
        else:
            segtype = default_segtype
            calib = -1.0  # defaults
            calper = -1.0
        #
        df = wf_template.format(sta=sta, chan=chan, net=net, loc=loc)
        wfpath = stock.epoch2str(e1, df)
        if os.path.isfile(wfpath) and not overwrite:
            print("\twffile %s exists" % wfpath)
            continue
        wfdir, wfdfile = os.path.split(wfpath)
        wfdir = os.path.dirname(wfpath)
        os.makedirs(wfdir, exist_ok=True)
        try:
            stachan.write(
                wfpath, format="MSEED", encoding="STEIM2", reclen=4096, byteorder=">"
            )
        except Exception as __:
            print("\tproblem writing data to %s" % wfpath)
        else:
            if verbose or debug:
                print("\twrote data from %s to %s" % (filename, wfpath))
            try:
                record = dbwfdisc.addv(
                    ("sta", sta),
                    ("chan", chan),
                    ("time", e1),
                    ("jdate", jdate),
                    ("endtime", e2),
                    ("nsamp", nsamp),
                    ("samprate", samprate),
                    ("dir", wfdir),
                    ("dfile", wfdfile),
                    ("segtype", segtype),
                    ("datatype", datatype),
                )
            except Exception as ex:
                print("\tproblem adding: %s" % ex)
sys.exit(0)
