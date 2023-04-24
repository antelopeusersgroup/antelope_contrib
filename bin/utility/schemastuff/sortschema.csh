if ( "$#" != 2 ) then
    echo "usage $0 schemafile newschema" 
    echo " "
    echo "little helper to sort a schema"
    exit 1
endif
set schemafile=$1
set newfile=$2
if ( -f $ANTELOPE/contrib/data/awk/splitschema.awk ) then
    set SPLITS=$ANTELOPE/contrib/data/awk/splitschema.awk
else if (-f $ANTELOPE/data/awk/splitschema.awk ) then
    set SPLITS=$ANTELOPE/data/awk/splitschema.awk
else
    echo "helper splitschema.awk not found!"
    exit 1
endif
rm -f $newfile
egrep "^Attribute" $schemafile | awk '{print $2}' > Attributes
egrep "^Relation" $schemafile|awk '{print $2}' > Relations
foreach att (`cat Attributes|sort -u`) 
    awk -f $SPLITS -v var=$att -v type=a $schemafile >> $newfile
    echo >> $newfile
end
foreach rel (`cat Relations|sort -u`) 
    awk -f $SPLITS -v var=$rel -v type=r $schemafile >> $newfile
end

rm -f Attributes
rm -f Relations
