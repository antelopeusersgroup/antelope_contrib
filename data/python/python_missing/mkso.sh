#!/bin/sh

MYOS=`uname -s`
pylib=$1
basepage=$2
if [ "$MYOS" == "Linux" ]; then
cat << __END__ > /tmp/myso
.so man3y/$basepage

__END__
else
cat << __END__ > /tmp/myso
.so man3/$basepage

__END__
fi

grep def $pylib | grep -v '#'| sed 's/def //g'| sed 's/(.*//g'| awk -F: '{printf("cp /tmp/myso %s.3y\n",$1)}'> /tmp/my_cmd
manpages=`grep def $pylib | grep -v '#'|sed 's/def //g'| sed 's/(.*//g'| awk -F: '{printf(" %s.3y",$1)}'`
echo "MAN3=$basepage $manpages"
bash /tmp/my_cmd
rm /tmp/myso
rm /tmp/my_cmd

