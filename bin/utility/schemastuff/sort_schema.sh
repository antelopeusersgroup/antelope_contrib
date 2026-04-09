#!/bin/sh

if [ "$#" != 2 ]; then
    echo "usage $0 schemafile newschema" 
    echo " "
    echo "little helper to sort a schema"
    exit 1
fi
schemafile=$1
newfile=$2
if [ -f $ANTELOPE/contrib/data/awk/splitschema.awk ]; then
    SPLITS=$ANTELOPE/contrib/data/awk/splitschema.awk
elif [ -f $ANTELOPE/data/awk/splitschema.awk ]; then
    SPLITS=$ANTELOPE/data/awk/splitschema.awk
else
    echo "helper splitschema.awk not found!"
    exit 1
fi
rm -f $newfile

egrep "^Attribute" $schemafile | awk '{print $2}' > Attributes
egrep "^Relation" $schemafile|awk '{print $2}' > Relations

for att in  `cat Attributes|sort -u`
do
    awk -f $SPLITS -v var=$att -v type=a $schemafile >> $newfile
    echo >> $newfile
done

for rel in `cat Relations|sort -u`
do
    awk -f $SPLITS -v var=$rel -v type=r $schemafile >> $newfile
done

rm -f Attributes
rm -f Relations
