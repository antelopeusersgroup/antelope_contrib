""" Presseaussendung und Meldungstext auf Homepage

@author      Nikolaus Horn <nikolaus.horn@zamg.ac.at
@created     November 03, 2021
@version     1.0
@license     MIT-style license

"""
import getopt
import tkinter as tk
import tkinter.scrolledtext as st
import tkinter.ttk as ttk
from tkinter import messagebox as mb
import antelope.datascope as ds
import antelope.stock as stock
import antelope.elog as elog
import antelope.sysinfo as sysinfo


# Import Antelope modules
sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")
# import zamg.seisparams as sp
import zamg.utilities as zu


def usage(progname):
    print(progname, " [-p pfname] [-o dbout] database[.table] [recno]")


class eventIssue:
    """Editorfenster mit Details"""

    def __init__(self, where, dbname, dbin, dbout, action, verbose):

        self.where = where
        self.dbin = dbin
        self.dbname = dbname
        self.dbout = dbout
        self.root = where
        self.verbose = verbose
        self.dbout.record = ds.dbNULL
        [self.time_null] = self.dbout.getv("time_checked")
        self.dbout.record = ds.dbALL

        try:
            [evid] = self.dbin.getv("evid")
            self.v_evid = evid
        except Exception as __:
            elog.die("cannot get evid, leaving!")
            self.dbout.record = ds.dbALL
        try:
            recno = self.dbout.find("evid==%d" % evid)
        except Exception as __:

            i_issue = "issue"
            i_source = "unk."
            still_open = True
            __, i_auth = sysinfo.my_username()
            s_checked = stock.epoch2str(stock.now(), "%Y-%m-%d %H:%M")
            s_fixed = ""
            remark = ""
        else:
            self.dbout.record = recno
            [
                i_evid,
                i_issue,
                i_source,
                i_checked,
                i_fixed,
                i_auth,
                i_commid,
            ] = self.dbout.getv(
                "evid",
                "issue",
                "source_of_current_parameter",
                "time_checked",
                "time_fixed",
                "auth",
                "commid",
            )
            still_open = self.dbout.ex_eval(" open == NULL || open !~ /n/")
            s_checked = stock.epoch2str(i_checked, "%Y-%m-%d %H:%M")
            if still_open:
                s_fixed = stock.epoch2str(stock.now(), "%Y-%m-%d %H:%M")
            else:
                s_fixed = stock.epoch2str(i_fixed, "%Y-%m-%d %H:%M")
            if i_commid >= 0:
                remark = zu.get_remark(self.dbout)
            else:
                remark = ""

        # Main Window
        self.evid_label = ttk.Label(where, width=10, text=evid)
        self.evid_label.grid(row=0, column=0)
        self.evid_label.config(background="#ffff66")

        self.v_issue = tk.StringVar()
        self.v_issue.set(i_issue)
        self.issue_entry = ttk.Entry(where, width=10, textvariable=self.v_issue)
        self.issue_entry.grid(row=0, column=1)

        self.s_label = ttk.Label(where, width=15, text="Informationsquelle")
        self.s_label.grid(row=1, column=0)

        tframe = ttk.Frame(where, relief="sunken")
        tframe.grid(row=1, column=1)
        self.r_btn_etype = ttk.Radiobutton(
            tframe, text="etype", value="etype", variable=self.v_issue
        )
        self.r_btn_mag = ttk.Radiobutton(
            tframe, text="magnitude", value="magnitude", variable=self.v_issue
        )
        self.r_btn_time = ttk.Radiobutton(
            tframe, text="time", value="time", variable=self.v_issue
        )
        self.r_btn_int = ttk.Radiobutton(
            tframe, text="I°", value="intensity", variable=self.v_issue
        )
        self.r_btn_coord = ttk.Radiobutton(
            tframe, text="lat/lon", value="coordinates", variable=self.v_issue
        )
        self.r_btn_depth = ttk.Radiobutton(
            tframe, text="depth", value="depth", variable=self.v_issue
        )
        if self.dbout.record >= 0:
            self.issue_entry.config(state=tk.DISABLED)
            self.r_btn_etype.config(state=tk.DISABLED)
            self.r_btn_mag.config(state=tk.DISABLED)
            self.r_btn_time.config(state=tk.DISABLED)
            self.r_btn_int.config(state=tk.DISABLED)
            self.r_btn_coord.config(state=tk.DISABLED)
            self.r_btn_depth.config(state=tk.DISABLED)
        self.r_btn_etype.grid(row=0, column=0)
        self.r_btn_mag.grid(row=0, column=1)
        self.r_btn_time.grid(row=0, column=2)
        self.r_btn_int.grid(row=0, column=3)
        self.r_btn_coord.grid(row=0, column=4)
        self.r_btn_depth.grid(row=0, column=5)
        self.v_source = tk.StringVar()
        self.v_source.set(i_source)
        self.source_entry = ttk.Entry(where, width=50, textvariable=self.v_source)
        self.source_entry.grid(row=2, column=0)

        self.c_label = ttk.Label(where, width=15, text="wann geprüft?")
        self.c_label.grid(row=3, column=0)
        self.f_label = ttk.Label(where, width=15, text="wann geklärt?")
        self.f_label.grid(row=3, column=1)
        self.v_checked = tk.StringVar()
        self.v_checked.set(s_checked)
        self.checked = ttk.Entry(where, width=20, textvariable=self.v_checked)
        self.checked.grid(row=4, column=0)
        self.v_fixed = tk.StringVar()
        self.v_fixed.set(s_fixed)
        self.fixed = ttk.Entry(where, width=20, textvariable=self.v_fixed)
        self.fixed.grid(row=4, column=1)

        self.v_open = tk.BooleanVar()
        self.v_open.set(still_open)
        self.c_open = ttk.Checkbutton(where, text="Open", variable=self.v_open)
        self.c_open.grid(row=5, column=0)
        self.v_auth = tk.StringVar()
        self.v_auth.set(i_auth)
        self.auth_text = ttk.Entry(where, width=20, textvariable=self.v_auth)
        self.auth_text.grid(row=5, column=1)

        self.remark_editor = st.ScrolledText(where, width=80, height=5)
        self.remark_editor.grid(row=6, column=0, columnspan=2, sticky="EW")
        self.i_remark = remark
        if remark != "":
            self.remark_editor.insert(tk.END, remark)
        # if previous_report != "":
        #    self.remark_editor.insert(tk.END, "--------Bisherige Meldung -----------\n")
        # self.remark_editor.insert(tk.END, msgtxt)

        ttk.Button(where, text="Abbruch", command=lambda: where.destroy()).grid(
            row=7, column=0
        )
        bstyle= ttk.Style()
        bstyle.configure("R.TButton", background="red")
        ttk.Button(
            where, text="Eintragen", style="R.TButton", command=lambda: self.set_issue()
        ).grid(row=7, column=1)
        where.title("Eventprobleme %s" % dbname)
        where.deiconify()

    def set_issue(self):
        s_evid = self.v_evid
        s_issue = self.v_issue.get()
        if self.v_open.get():
            s_open = "y"
        else:
            s_open = "n"
        s_source = self.v_source.get()
        ts_checked = self.v_checked.get()
        if ts_checked != "":
            s_checked = stock.str2epoch(ts_checked)
        else:
            s_checked = self.time_null
        ts_fixed = self.v_fixed.get()
        if ts_fixed != "":
            s_fixed = stock.str2epoch(ts_fixed)
        else:
            s_fixed = self.time_null
        s_auth = self.v_auth.get()
        if self.dbout.record >= 0:
            try:
                self.dbout.putv(
                    ("open", s_open),
                    ("source_of_current_parameter", s_source),
                    ("time_checked", s_checked),
                    ("time_fixed", s_fixed),
                    ("auth", s_auth),
                )
            except Exception as e:
                mb.showerror(
                    "Problem setting issue",
                    "problem with evid %d and issue %s: %s" % (s_evid, s_issue, e),
                )
                self.where.destroy()
        else:
            try:
                recno = self.dbout.addv(
                    ("evid", s_evid),
                    ("issue", s_issue),
                    ("open", s_open),
                    ("source_of_current_parameter", s_source),
                    ("time_checked", s_checked),
                    ("time_fixed", s_fixed),
                    ("auth", s_auth),
                )
            except Exception as e:
                mb.showerror(
                    "Problem adding issue",
                    "problem with evid %d and issue %s: %s" % (s_evid, s_issue, e),
                )
                self.where.destroy()
            self.dbout.record = recno

        tmpstr = self.remark_editor.get("1.0", "end")
        if tmpstr != "" and tmpstr != self.i_remark:
            remark = tmpstr.strip()
            commid = zu.set_remark(self.dbout, remark)
            if commid != None and commid < 0:
                mb.showerror(
                    "Problem setting remark",
                    "problem adding remark to database\n%s" % remark,
                )
        mb.showinfo(title="Eingetragen",message="Info für event %d erfolgreich eingetragen" % s_evid)
        self.where.destroy()


def main():
    """Eingabeparameter und so. Alles weitere in
    EventReport"""

    progname = sys.argv[0].split("/")[-1]
    elog.init(progname)

    verbose = False
    action = ""
    recno = -1
    dboutname = ""
    pfname = progname

    try:
        opts, args = getopt.getopt(sys.argv[1:], "ho:p:vPWH", "")
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
            dboutname = argument
        elif option == "-p":
            pfname = argument
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
    if dboutname != "":
        db_out = ds.dbopen(dboutname, "r+")
        my_db = ds.dbopen_database(dbname, "r")
        my_db = my_db.lookup(table="origin")
    else:
        my_db = ds.dbopen_database(dbname, "r+")
        db_out = my_db.lookup(table="origin")
    if dboutname != "":
        db_out = db_out.lookup(table="evissues")
    else:
        db_out = my_db.lookup(table="evissues")

    if recno > -1:
        my_db.record = recno
    else:
        my_db.record = 0

    main_window = tk.Tk()  ## Root window

    main_window.iconify()
    # main_window.geometry("800x800")
    my_dbname = my_db.query(ds.dbDATABASE_NAME)
    eventIssue(main_window, my_dbname, my_db, db_out, action, verbose)
    main_window.mainloop()


if __name__ == "__main__":
    status = main()
    sys.exit(status)
