#!/bin/sh

#little helper to copy antelope parameterfiles
# Niko, 2003

	#sh does not allow &&
	if [ "$#" -ne 2 ];then
		if [ "$#" -ne 3 ]; then
			echo "usage: $0 [-d] name dest";exit
		fi
	fi
	if [ "$1"  = "-d" ]; then
		default=1;
		shift;
	else	
		default=0;
	fi

	if [ $default -eq 1 ]; then
		# get 1st line
		pf=`pfecho -w $1 | sed -n '1p'`
	else
		# get last line
		pf=`pfecho -w $1 | sed -n '$p'`
	fi

	if [ -z $pf ]; then
		echo "parapeterfile $1 not found!";
		exit;
	fi
	
	if [ ! -r $pf ]; then
		echo "parameterfile $pf not readable!"
		exit;
	fi

	if [ -d $2 ]; then
		dest=$2/`basename $pf`
	else	
		# ksh not always present on linux
		#dest=${2%.pf}.pf
		dest=`echo $2 | sed 's/\(.*\)\.pf/\1/`.pf
	fi

	cp -i $pf $dest	
