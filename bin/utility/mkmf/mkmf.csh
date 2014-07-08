
set nonomatch

set mybins=""
set patterns="*.c *.cpp *.F *.f *.sh *.csh *.tcl *.xpl *.xppl *.xpls *.xtcl *.xwish *.xvwish *.xwish8 *.xpy"
foreach pat ($patterns)
    set ft=( $pat )
    if (-e $ft[1] ) then
        set mybins=`printf "%s %s" $mybins $ft:gr`
    endif
end
if ( "${mybins}--" != "--"} ) then
echo "BIN=$mybins"
endif

set pat="*.pf"
set ft = ( $pat )
if (-e $ft[1] ) then
    echo PF=$ft
endif

set myincludes=""
set patterns="*.h *.i"
foreach pat ($patterns)
    set ft=( $pat )
    if (-e $ft[1] ) then
        set myincludes=`printf "%s %s" $myincludes $ft`
    endif
end
if ( "${myincludes}--" != "--" ) then
echo "INCLUDE=$myincludes"
endif


foreach mansect ( 1 2 3 4 5 6 7 8) 
    set ft= ( *.$mansect* )
    if (-e $ft[1]) then
        echo "MAN$mansect=$ft"
    endif
end

echo 'include $(ANTELOPEMAKE)' 
echo 'DIRS='
echo 'SUBDIR=/local'
