# libslink

A C library for SeedLink clients.

This project is a general purpose library for communicating with a SeedLink
server.  The library supports [SeedLink v4](https://docs.fdsn.org/projects/seedlink/),
standardized by the [International FDSN](https://www.fdsn.org/), and earlier
versions of the protocol.

SeedLink is an open transport protocol commonly use for continuous
seismological data streams.

Documentation is available at https://earthscope.github.io/libslink

Releases are available at https://github.com/EarthScope/libslink/releases

## Building and Installing

In most Unix/Linux environments a simple 'make' will build the program.

The `CC` and `CFLAGS` environment variables can be used to configure
the build parameters.

The library should work on Linux, macOS, and MS Windows.

## Licensing

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Copyright (C) 2024 Chad Trabant, EarthScope Data Services

## Acknowlegements

Numerous improvements have been incorporated based on feedback and
patches submitted by others.  Individual acknowlegements are included
in the ChangeLog.  Thank you!

Initial development at the ORFEUS Data Center/EC MEREDIAN Project
Continued maintained at the IRIS Data Management Center
Current maintenance from EarthScope Data Services
