"""
#   Copyright (c) 2021 Nikolaus Horn, ZAMG
#
#   Written by Nikolaus Horn
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.

mail utility functions
"""
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.header import Header
import smtplib
import re
import antelope.stock as stock
import antelope.elog as elog


def multipartemail(sender, email, subject, html, text=None):
    """ Email magic: use only standard libs coming with Antelope to produce multipart email """

    if text is None:
        text = html.replace("<br>", "\n")
        text = re.sub(
            r"<a href=([^>]+)>(.*)</a>", r"\2:\1", html
        )  # replace anchors with text and link
        result = ""
        paren = 0
        # very poor mans cleaner for html: remove everything within brackets
        for char in text:
            if char == "<":
                paren += 1
            elif char == ">":
                paren -= 1
            elif (
                not paren
            ):  # trick: every number but 0 is false, so "not 0" is always true
                result += char
        text = result
        """
        keep for the record: cleaner code than above, but needs another module and eventally also longer time
        import re
        text = re.sub("\<.*?\>","", html)# very poor mans html2text
        """
    message = MIMEMultipart()
    message["To"] = Header(
        email
    )  # Header(email, 'utf8') does NOT work when multiple Recipients are specified...
    message["From"] = Header(sender)
    message["Subject"] = Header(
        subject, "utf8"
    )  # hopefully get rid of "problems" with ugly umlauts

    msg_body = MIMEMultipart("alternative")

    plaintext_part = MIMEText(text, "plain")
    msg_body.attach(plaintext_part)

    html_part = MIMEText(html, "html")
    msg_body.attach(html_part)
    message.attach(msg_body)

    encoded_email = message.as_string()
    return encoded_email


def send_multipartemail(
    email, subject, html, text=None, myfrom=None, one_email=True, address_separator=","
):
    """ Send email as text and html """

    mypf = stock.pfread("site")

    if myfrom == None:
        try:
            import antelope.sysinfo as sysinfo
        except Exception as __:
            # this only fails on older Antelope installations
            sender = "seismo@zamg.ac.at"
        else:
            if mypf.has_key("mail_domain"):
                host = mypf["mail_domain"]
            else:
                (__, host) = sysinfo.my_hostname()
            (__, me) = sysinfo.my_username()
            sender = "%s@%s" % (me, host)
    else:
        sender = myfrom

    if mypf.has_key("mailhost"):
        mailhost = mypf["mailhost"]
    else:
        mailhost = "localhost"
    receivers = email.split(
        ","
    )  # list items are always separated by commas, regardless of what is used as address_separator later
    receivers = [x.strip() for x in receivers]  # trim whitespace

    retcode = True
    if one_email:
        destination = address_separator.join(receivers)

        #fout=open("/tmp/mailscheiss.log","w+")
        #fout.write("one_email: %s" % destination)
        #fout.close()
        #exit

        #print("to: ", destination)
        msgbody = multipartemail(sender, destination, subject, html, text)
        try:
            smtp = smtplib.SMTP(mailhost)
            smtp.sendmail(sender, destination, msgbody)
        except smtplib.SMTPException as __:
            elog.notify("problem sending email to %s" % destination)
            smtp.close()
            retcode = False
        else:
            smtp.close()
    else:  # one email per receiver
        for destination in receivers:
            #fout=open("/tmp/mailscheiss.log","w+")
            #fout.write("many emails: %s" % destination)
            #fout.close()
            #exit
            #print("jeweils to: ", destination)
            msgbody = multipartemail(sender, destination, subject, html, text)
            try:
                smtp = smtplib.SMTP(mailhost)
                smtp.sendmail(sender, destination, msgbody)
            except smtplib.SMTPException as __:
                elog.notify("problem sending email to %s" % destination)
                smtp.close()
            retcode = False
        else:
            smtp.close()
    return retcode
