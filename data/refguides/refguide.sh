#!/bin/sh -x

if [ $# -ne 1 ] ; then 
    echo "Usage: refguide name"
    exit 1 
fi

name=$1
output=$1.mif
case $name in
c|user|scripting) 
    input=$ANTELOPE/data/doc/refguides/$name/$name.ref 
    txform ref2mif $input > $output ;;

combined) 
    for input in user scripting c tables 
    do
	echo combined::$input
	txform ref2mif $ANTELOPE/data/doc/refguides/$input/${input}0.ref > ${input}0.mif 
    done ;;

*) echo "Usage: refguide [c|user|scripting|combined]"
    exit 1 ;;
esac


