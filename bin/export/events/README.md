Event Export
=======
Utilities to export event information from Datascope into different formats

There are several different tools that will convert the CSS3.0 data in Datascope
into different formats (i.e. JSON, GeoJSON, QuakeML, etc.) written by many authors
over the years. It's not trivial to identify the ones that we need and maintain the
old code that might not work anymore. Since most of the recent development in
Antelope's contrib repo is now done in Python and it's easy to create a modular
framework for data conversion then we can consolidate all of the tools into one
folder in the repo. Inside this folder we can keep all modules and libraries that
we produce and then have simple wrappers that will modify the process as needed and
apply the correct syntax for the output information. Instead of having one exporter
with multiple modes of operation we can have one executable for each intended application
without too much code overhead. Since all the data extraction and parsing is already
defined in the libraries then we just add a new file for each new format that we want
and one dedicated Python wrapper.

Applications
------------
Each wrapper is consider an individual entity for the user but developers can share
all of the redundant code within different exporters and converters. Names for the
executables should be specific to their application and each should have a dedicated
manpage. One Makefile will be responsible for the compilation and the addition of new
modules should not interfere with preexisting code.

Classes
-------
Each module should be as generic as possible. Instead of having one Class for every
table that you are handling try to make a generic version that will handle all tables.
If we document them consistently then we can share them between applications.

    css2qml.py              Convert an Event() object into QuakeML() object.
    db_collection.py        Abstract objects to store information from Datascope tables.
    event.py                Lead the information from 1 event from a CSS3.0 database.
    functions.py            Several functions to test the status of databases and convert values.
    logging_helper.py       Generic logging tool mainly replacing elog functionality.
    xmltodict.py            External code that will convert a Python dict into XML.


QuakeML
------
Juan Reyes
reyes@ucsd.edu

QuakeML is an XML representation of seismological data. Intended to standardize seismological
data exchange. A basic outline of the general concept of QuakeML can be found in
Schorlemmer et al. (2004). Other online references:
    https://quake.ethz.ch/quakeml
    https://earthquake.usgs.gov/earthquakes/feed/v1.0/quakeml.php

We decided to develop this version to exchange parametric data between ANSS networks,
IRIS and other international partners. The specifics on ANSS Quakeml Standards can be
found on https://github.com/usgs/Quakeml/wiki/ANSS-Quakeml-Standards-Working-Group-Project-Brief

There are two working versions of code intended to export event data from Datascope into
QuakeML format. One is from the Nevada Seismological Lab's and it's available on their
github repo https://github.com/NVSeismoLab/qmlutil that has public access. The second was
developed by Matt Gardine at the Alaska Earthquake Center. Both accomplished the main task
of exporting the data but I couldn't get them to fit in this repo format easily. You might
see some good ideas from both versions in our approach but the code is all new.

This code attempts to convert 1 seismic event and all other associated information from
an Antelope Datascope database into QuakeML format. We start with an EVIDs and all ORIDs
associated to that EVID. For this we *most* have an event table present.

The RelaxNG files describing the schema are included in the repo and referenced in the
parameter file. There is a validator that will try to verify the output.

The code will send all XML output to STDOUT or to a file if you are using the -o FILENAME
flag at runtime. Run with an EVID to export that single event. If no event is found with
that EVID then the tool will produce a QuakeML object to "delete" that event and will ouptut
the result.

There is an external library that we included not developed by us that will convert a Python
dict into the final XML format.
    XMLTODICT.PY = Parse the given XML input and convert it into a dictionary.
    #Copyright (C) 2012 Martin Blech and individual contributors.

