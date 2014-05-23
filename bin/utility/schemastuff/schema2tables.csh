if ( "$#" != 1 ) then
    echo "usage $0 schemafile" 
    echo " "
    echo "little helper to split schema to extension tables"
    exit 1
endif
set schemafile=$1
#set base_schemafile=$ANTELOPE/data/schemas/css3.0
#set base_schemafile=$ANTELOPE/data/schemas/css3.0
if ( -f $ANTELOPE/local/data/awk/splitschema.awk ) then
    set SPLITS=$ANTELOPE/local/data/awk/splitschema.awk
else if (-f $ANTELOPE/data/awk/splitschem.awk ) then
    set SPLITS=$ANTELOPE/data/awk/splitschema.awk
else
    echo "helper splitschema.awk not found!"
    exit 1
endif
#split extension schema to extension tables
egrep "^Attribute" $schemafile | awk '{print $2}' > Attributes
#egrep "^Attribute" $base_schemafile | awk '{print $2}' > base_Attributes
egrep "^Relation" $schemafile|awk '{print $2}' > Relations
foreach att (`cat Attributes`) 
    awk -f $SPLITS -v var=$att -v type=a $schemafile > att_$att
end
#foreach att (`cat base_Attributes`) 
#    awk -f $SPLITS -v var=$att -v type=a $base_schemafile > base_$att
#end
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
        endif
    end    
    cat rel_$rel >> $rel
end
#cleanup
rm -f fds sfds ufds
foreach att (`cat Attributes`) 
    rm -f att_$att
end
#foreach att (`cat base_Attributes`) 
#    rm -f base_$att
#end
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
