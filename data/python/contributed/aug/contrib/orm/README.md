## aug.contrib.orm
### Object-Relational Mapping for Datascope
Copyright by Mark Williams 2012.013
This software is distributed under the Lesser GNU Public License.
See files COPYING and COPYING.lesser for details.

Contains basic functions to interact with (read) data from Antelope Datascope database tables into python using the Antelope Python API.

### OVERVIEW:

* Dbrecord - basically a dictionary/object which holds all the data from one record of a table. Field access as key or attribute.

* DbrecordList - A list of Dbrecord's. ALL data are local, in namespace and memory.

* DbrecordPtr - acts the same as a Dbrecord, but the data are not local, they are read from/(AND WRITTEN TO) to the database.

* DbrecordPtrList - A list of DbrecordPtr's. Suitable for most occasions. Because DbrecordLists can take up memory for a lot of records.

* AttribDbptr - An 'attribute pointer' which consists of just one db pointer in memory, no matter how many records are contained. Based on DbrecordPtrList. builds DbrecordPtr's on the fly. Basically,a simple ORM for Datascope.

