import getopt
import smtplib
from email.message import EmailMessage

sys.path.append(os.environ["ANTELOPE"] + "/contrib/data/python")

import zamg.mailutils as zm


def usage(progname):
    print(progname, "[-vd] [-s subject] recipient")


progname = sys.argv[0].split("/")[-1]
subject = ""
input_is_html = False
attachment = None

try:
    opts, args = getopt.getopt(sys.argv[1:], "vs:a:", "")
except getopt.GetoptError:
    usage(progname)
    elog.die("Illegal option")
    sys.exit(2)

for o, a in opts:
    if o == "-v":
        verbose = True
    elif o == "-s":
        subject = a
    elif o == "-a":
        attachment = a
    elif o == "-h":
        input_is_html = True

if len(args) > 1 or len(args) < 1:
    usage(progname)
    sys.exit(1)

recipient = args[0]

text_input = sys.stdin.read()

if input_is_html:
    zm.send_multipartemail(recipient, subject, text_input, attachment=attachment)
else:
    zm.send_multipartemail(recipient, subject, None, text_input, attachment=attachment)
