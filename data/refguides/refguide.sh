
if [ $# -ne 1 ] ; then 
    echo "Usage: refguide name"
    exit 1 
fi

name=$1
output=$1.mif
input=$ANTELOPE/data/refguides/$name/$name.ref

txform ref2mif $input > $output

