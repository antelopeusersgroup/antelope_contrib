FILES=`dbselect $1 'extfile()' | sort -u`
for i in $FILES
	do
	/usr/local/bin/xv $i &
done
