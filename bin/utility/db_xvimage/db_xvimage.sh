FILES=`dbselect $1 'extfile()' | sort -u`
for i in $FILES
	do
	xv $i &
done
