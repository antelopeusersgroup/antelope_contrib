"""
Indent antelope parameterfiles

@author      Nikolaus Horn
@created     Nov 1, 2025
@version     1.1

idea is to tidy up antelope parameterfiles

name value # remark ->
name           value                 #
0              -l defines this       -c defines the indention level of comments

"""

import getopt
import re

# Import Antelope modules
import antelope.stock as stock
import antelope.elog as elog


def usage(progname):
    print(progname, " [-vI] [-l x] [-c y] [-I z] filename")
    print()
    print(" indent Antelope parameterfiles")
    print(" -I indent lines with only remarks")
    print(" -l x -  indent values to at least x (30) chars")
    print(" -c y -  indent comments (remarks) - '#' - to at least y (70) chars")
    print(" -t z -  indent nested items by z (3) spaces")
    print("default values in brackets")


def decode(s, encodings=("utf8", "latin1")):
    for encoding in encodings:
        try:
            return s.decode(encoding)
        except UnicodeDecodeError:
            pass
    return s.decode("latin1", "ignore")


def main():
    progname = sys.argv[0].split("/")[-1]
    elog.init(progname)
    verbose = 0
    indent = 0
    delta = 0
    indent_step = 3
    indent_value = 20
    indent_remark = 40
    indent_all_remarks = False
    opts = []
    args = []
    try:
        opts, args = getopt.getopt(sys.argv[1:], "vt:l:c:I", "")
    except getopt.GetoptError:
        print("illegal option")
        usage(progname)
        sys.exit(2)

    now = stock.now()
    year = int(stock.epoch2str(now, "%Y"))
    for o, a in opts:
        if o == "-v":
            verbose = 1
        elif o == "-t":
            indent_step = int(a)
        elif o == "-I":
            indent_step = int(a)
            indent_all_remarks = True
        elif o == "-l":
            indent_value = int(a)
        elif o == "-c":
            indent_remark = int(a)

    if len(args) > 1 or len(args) < 1:
        usage(progname)
        sys.exit(1)

    if len(args) > 0:
        filename = args[0]

    # with open(filename, encoding="latin1", errors="ignore") as f:
    with open(filename) as f:
        lines = f.readlines()

    indent = 0
    recno = -1
    closing_char = []
    lastblank = True
    for line in lines:
        delta = 0
        remarkonly = False
        remark = None
        key = None
        val = None

        sline = line.strip()
        # print("sline:",sline)
        if sline == "":
            key = ""
        elif sline.startswith("#"):
            remarkonly = True
            remark = sline[1:]
        else:
            # if starts with " or ':
            # check if starts with quoted word
            # else name is first word
            # split line in "name", "value", "remark"
            # attention: try to catch escaped pound sign!!! \# does NOT start a comment...
            if "#" in line:
                # split on first non-escaped pound-sign
                try:
                    stuff, remark = re.split(r"[^\\]#", sline, maxsplit=1)
                except Exception as __:
                    key = sline
                    remark = None
                else:
                    sline = stuff.strip()
            if len(closing_char) > 0:
                cc = closing_char[-1]
            else:
                cc = None
            if cc is not None and sline.startswith(cc):
                if verbose:
                    print("    closed!!!")
                closing_char.pop()
                # do it immediate, not after outputting the stuff
                indent -= indent_step
                delta = 0
                key = sline
            elif re.match(
                r"^\S+$", sline
            ):  # a line containing only one string, no spaces
                key = sline
                # print("only a key (or value?):",key)
                # there might be "anonymous arrays or tables
                tm = re.search(r"&Tbl{", key)
                am = re.search(r"&Arr{", key)
                if tm:
                    closing_char.append("}")
                    delta = indent_step
                elif am:
                    closing_char.append("}")
                    delta = indent_step
            else:  # there are spaces
                # match for quoted key
                qm = re.match(r"([\'\"])(.*)\1", sline)
                # match for regular unquoted key
                nm = re.match(r"(\S+)", sline)
                if qm:
                    key = qm.group(0)
                    # remove first occurence of key in string
                    val = re.sub(key, "", sline, count=1).lstrip()
                elif nm:
                    key = nm.group(0)
                    __, val = sline.split(None, maxsplit=1)
                else:
                    val = sline
                cc = None
                tm = re.search(r"&Tbl{", val)
                am = re.search(r"&Arr{", val)
                lm = re.search(r"&Literal(.)", val)
                if tm:
                    closing_char.append("}")
                    delta = indent_step
                elif am:
                    closing_char.append("}")
                    delta = indent_step
                elif lm:
                    delta = indent_step
                    oc = lm.group(1)
                    if oc == r"{":
                        closing_char.append("}")
                    elif oc == r"[":
                        closing_char.append("]")
                    elif oc == r"<":
                        closing_char.append(">")
                    elif oc == r"`":
                        closing_char.append("'")
                    else:
                        print("problem: unknown opening character for Literal:", oc)
        outstr = ""
        if indent_all_remarks:  # force all remarks to be indented
            lastblank = False
        if key is not None:
            lastblank = False
            ic = " " * indent
            outstr = ic + key
            strlen = len(outstr)
            # print("ic:%s:%s:" % (ic,outstr))
        if val is not None:
            lastblank = False
            if strlen + 1 > indent_value:
                outstr += " " + val
            else:
                outstr += " " * (indent_value - strlen) + val
        if remarkonly and lastblank:
            outstr = "# " + remark.strip()
            lastblank = True
        elif remark is not None:
            remark = remark.strip()
            lastblank = False
            strlen = len(outstr)
            if strlen + 2 > indent_remark:
                outstr += " # " + remark
            else:
                outstr += " " * (indent_remark - strlen) + "# " + remark
        indent += delta  # change indent level AFTER output!
        if outstr == "":
            lastblank = True
        print(outstr)

    return 0


if __name__ == "__main__":
    status = main()
    sys.exit(status)
