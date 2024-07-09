#!/bin/sh

# show database holding the arrival table

dbname=$1
record=$2


#show "real" database name, not the name of the Trial database

# The trick here is to find the name of the database that holds the arrival table

echo "database $dbname record $record"

db=`dbquery $dbname dbDATABASE_NAME`
arrival_table=`dbquery $db.arrival dbTABLE_FILENAME`
basedir=`dbquery $arrival_table dbTABLE_DIRNAME`
base_dbname=`basename $arrival_table .arrival`
dbname="$basedir/$base_dbname"

echo $dbname


