#!/bin/sh

# Version 1.1 2023-03-13
# translated from csh to bash, since csh seems to disappear from our computers...

declare -a mybins=()
patterns=".c .cpp .F .f .sh .csh .tcl .xpl .xppl .xpls .ptk .xtcl .xwish .xvwish .xwish8 .oxwish .xpy .xbqpy .xbqpyn .xbqpyx .xpys"
for pat in ${patterns[@]}
do	
	for ff in *$pat; do
		if [ -f $ff ]; then
			fb=$(basename -s $pat $ff)
			mybins+=($(basename -s $pat $ff))
		fi
	done
done
if [[ ${#mybins[@]} -gt 0  ]];then
	mybins=$(printf " %s" "${mybins[@]}")
	echo "BIN=${mybins:1}" # get rid of initial space
fi

declare -a mypfs=()
pat=".pf"
for ff in *$pat; do
	if [ -f $ff ]; then
		mypfs+=($ff)
	fi
done
if [[ ${#mypfs[@]} -gt 0  ]];then
	mypfs=$(printf " %s" "${mypfs[@]}")
	echo "PF=${mypfs:1}"
fi

declare -a myincludes=()
patterns=".h .i"
for pat in ${patterns[@]}
do	
	for ff in *$pat; do
		if [ -f $ff ]; then
			myincludes+=($ff)
		fi
	done
done
if [[ ${#myincludes[@]} -gt 0  ]];then
	myincludes=$(printf " %s" "${myincludes[@]}")
	echo "INCLUDE=${myincludes:1}"
fi

declare -a mymanpages
for mansect in 1 2 3 4 5 6 7 8; do 
	mymanpages=()
	for ff in *.$mansect*; do
		if [ -f $ff ]; then
			mymanpages+=($ff)
		fi
		if [[ ${#mymanpages[@]} -gt 0  ]];then
			mp=$(printf " %s" "${mymanpages[@]}")
			echo "MAN$mansect=${mp:1}"
		fi
	done
done

echo 'include $(ANTELOPEMAKE)' 
echo 'DIRS='
echo 'SUBDIR=/contrib'
