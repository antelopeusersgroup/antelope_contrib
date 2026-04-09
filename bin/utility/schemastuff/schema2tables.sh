#!/bin/sh

if [ "$#" > 1 ]  || [ "$#" < 2 ]; then
    echo "usage $0 [baseschema] schemafile" 
    echo " "
    echo "little helper to split schema to extension tables"
    exit 1
fi
if [ "$#" > 1 ]; then
    set baseschema=$1
    set schemafile=$2
else
    set baseschema=''
    set schemafile=$1
fi
if [ -f $ANTELOPE/contrib/data/awk/splitschema.awk ]; then
    SPLITS=$ANTELOPE/contrib/data/awk/splitschema.awk
elif [-f $ANTELOPE/data/awk/splitschema.awk ]; then
    SPLITS=$ANTELOPE/data/awk/splitschema.awk
else
    echo "helper splitschema.awk not found!"
    exit 1
fi

#split extension schema to extension tables
egrep "^Attribute" $schemafile | awk '{print $2}' > Attributes
if [ $baseschema != '' ]; then
    egrep "^Attribute" $baseschema | awk '{print $2}' > base_Attributes
    comm -1 -3 base_Attributes Attributes > new_Attributes
    mv -f new_Attributes Attributes
fi
egrep "^Relation" $schemafile|awk '{print $2}' > Relations
for att in `cat Attributes` 
do
    awk -f $SPLITS -v var=$att -v type=a $schemafile > att_$att
done
for rel in `cat Relations`
do
    awk -f $SPLITS -v var=$rel -v type=r $schemafile > rel_$rel
    grep Fields rel_$rel | sed 's/.*[//g' | sed 's/].*//g' > fds
    #ufds- unique fields
    rm -f ufds
    for ff in `cat fds`
	do
        echo $ff >> ufds
	done
    #sfds - sorted fields
    cat ufds | sort  > sfds
    rm -f  $rel
    for ff in `cat sfds`
	do
        #echo "rel: $rel $ff"
        if [-e att_$ff]; then
            cat att_$ff >> $rel
        else    
            echo "Attribute $ff not found!"
		fi
	done    
    cat rel_$rel >> $rel
done
#cleanup
rm -f fds sfds ufds
for att in `cat Attributes`
do
    rm -f att_$att
done
rm -f Attributes
MF="Makefile_$schemafile"
echo "DATADIR=schemas/css3.0.ext" > $MF
echo "DATA= \\" >> $MF
for rel in `cat Relations`
do
    rm -f rel_$rel

    printf " %s"  $rel >> $MF
done
printf "\n" >> $MF
echo 'include $[ANTELOPEMAKE]' >> $MF
rm -f Relations Attributes base_Attributes

echo "New makefile $MF"
echo "run make -f $MF"
echo "to install the tables as extensions to css3.0"
