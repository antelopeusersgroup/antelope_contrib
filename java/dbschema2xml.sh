#!/bin/sh
exec java -cp $ANTELOPE/data/java/antelope.jar com.brtt.antelope.DatabaseSchema $1 -toXML
