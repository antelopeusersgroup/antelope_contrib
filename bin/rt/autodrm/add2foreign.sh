#!/bin/sh -x

if [ $# -ne 3 ] ; then
    echo "Usage: $0 database seed-net autodrm-net"
    exit 1
fi


foreign=`pfecho -q trdefaults foreignkeys_database`
foreign=$ANTELOPE/data/$foreign
dbadd $1.site $foreign.fnetsta "sta:=sta" "fsta:=sta" "snet:='$2'" "anet:='$3'"
dbadd $1.sitechan $foreign.fchanloc "sta:=sta" "fchan:=chan" "chan:=chan" "loc:=''" "aux:=''"
