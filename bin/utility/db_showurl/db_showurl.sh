FILES=`dbselect $1 url | sort -u`
for i in $FILES
	do
	netscape -remote "openURL($i, new-window)" &
done
