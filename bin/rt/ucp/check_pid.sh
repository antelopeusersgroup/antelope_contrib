#!/sbin/sh
#
#set -v

pid=`/usr/bin/ps -elo pid,args |
     /usr/bin/grep $1 | /usr/bin/grep -v grep | /usr/bin/grep -v check |
     /usr/bin/sed -e 's/^  *//' -e 's/ .*//'`
[ "$pid" != "" ] && echo $pid && exit 0
[ "$pid" = "" ] && echo none && exit 0
#

