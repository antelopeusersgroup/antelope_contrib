"""
#   Copyright (c) 2021 Nikolaus Horn, ZAMG
#
#   Written by Nikolaus Horn
#
#   This software may be used freely in any way as long as
#   the copyright statement above is not removed.

mail utility functions
"""

from os.path import basename
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.application import MIMEApplication
from email.header import Header
from email.message import EmailMessage
import smtplib
import re
import html
import antelope.stock as stock
import antelope.elog as elog


def html_to_text(htmlcode):
    text = html.unescape(htmlcode)
    text = text.replace("<br>", "\n")
    text = re.sub(
        r"<a href=([^>]+)>(.*)</a>", r"\2:\1", text
    )  # replace anchors with text and link
    text = re.sub(r"<.*?>", "", text)  # very poor mans html2text
    return text


def multipartemail(sender, email, subject, htmlcode, text=None, attachment=None):
    """Email magic: use only standard libs coming with Antelope to produce multipart email"""

    if text is None:
        text = html_to_text(htmlcode)
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
    if htmlcode is not None:
        html_part = MIMEText(htmlcode, "html")
        msg_body.attach(html_part)
    message.attach(msg_body)
    if attachment is not None:
        bn = basename(attachment)
        with open(attachment, "rb") as f:
            mattachment = MIMEApplication(f.read(), Name=bn)
            mattachment["Content-Disposition"] = 'attachment; filename="%s"' % bn
            message.attach(mattachment)
    encoded_email = message.as_string()
    return encoded_email


def send_multipartemail(
    email,
    subject,
    htmlcode,
    text=None,
    myfrom=None,
    one_email=True,
    address_separator=",",
    attachment=None,
):
    """Send email as text and html"""

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
    )  # make sure list items are always separated by commas, regardless of what is used as address_separator later
    receivers = [x.strip() for x in receivers]  # trim whitespace

    retcode = True
    if one_email:  # one email to many recipients
        destination = address_separator.join(receivers)
        msgbody = multipartemail(
            sender, destination, subject, htmlcode, text, attachment=attachment
        )
        try:
            smtp = smtplib.SMTP(mailhost)
            smtp.sendmail(sender, destination, msgbody)
        except smtplib.SMTPException as __:
            elog.notify("problem sending email to %s" % destination)
            smtp.close()
            retcode = False
        else:
            smtp.close()
    else:  # one email per recipient
        for destination in receivers:
            msgbody = multipartemail(
                sender, destination, subject, htmlcode, text, attachment=attachment
            )
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
