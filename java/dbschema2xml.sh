#!/bin/sh
if [ $# != 1 ]; then
  echo Usage: $0 [schema file]
  exit
fi
if [ ! -e $1 ]; then
  echo $0: error: "$1" does not exist. 
  echo You have to specify the complete filename to the schema definition file.
  exit
fi
exec java -cp $ANTELOPE/data/java/antelope.jar com.brtt.antelope.DatabaseSchema $1 -toXML
