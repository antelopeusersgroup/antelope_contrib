if ( "$#" < 1 || "$#" >2 ) then
    echo "usage $0 [baseschema] schemafile" 
    echo " "
    echo "little helper to split schema to extension tables"
    exit 1
endif
if ( "$#" > 1 ) then
    set baseschema=$1
    set schemafile=$2
else
    set baseschema=''
    set schemafile=$1
endif
if ( -f $ANTELOPE/contrib/data/awk/splitschema.awk ) then
    set SPLITS=$ANTELOPE/contrib/data/awk/splitschema.awk
else if (-f $ANTELOPE/data/awk/splitschema.awk ) then
    set SPLITS=$ANTELOPE/data/awk/splitschema.awk
else
    echo "helper splitschema.awk not found!"
    exit 1
endif

#split extension schema to extension tables
egrep "^Attribute" $schemafile | awk '{print $2}' > Attributes
if ( $baseschema != '' ) then
    egrep "^Attribute" $baseschema | awk '{print $2}' > base_Attributes
    comm -1 -3 base_Attributes Attributes > new_Attributes
    mv -f new_Attributes Attributes
endif
egrep "^Relation" $schemafile|awk '{print $2}' > Relations
foreach att (`cat Attributes`) 
    awk -f $SPLITS -v var=$att -v type=a $schemafile > att_$att
end
foreach rel (`cat Relations`) 

    awk -f $SPLITS -v var=$rel -v type=r $schemafile > rel_$rel
    grep Fields rel_$rel | sed 's/.*(//g' | sed 's/).*//g' > fds
    #ufds- unique fields
    rm -f ufds
    foreach ff (`cat fds`) 
        echo $ff >> ufds
    end
    #sfds - sorted fields
    cat ufds | sort  > sfds
    rm -f  $rel
    foreach ff (`cat sfds`) 
        #echo "rel: $rel $ff"
        if (-e att_$ff) then
            cat att_$ff >> $rel
        else    
            echo "Attribute $ff not found!"
        endif
    end    
    cat rel_$rel >> $rel
end
#cleanup
rm -f fds sfds ufds
foreach att (`cat Attributes`) 
    rm -f att_$att
end
rm -f Attributes
set MF="Makefile_$schemafile"
echo "DATADIR=schemas/css3.0.ext" > $MF
echo "DATA= \" >> $MF
foreach rel (`cat Relations`) 
    rm -f rel_$rel

    printf " %s"  $rel >> $MF
end
printf "\n" >> $MF
echo 'include $(ANTELOPEMAKE)' >> $MF
rm -f Relations Attributes base_Attributes

echo "New makefile $MF"
echo "run make -f $MF"
echo "to install the tables as extensions to css3.0"
