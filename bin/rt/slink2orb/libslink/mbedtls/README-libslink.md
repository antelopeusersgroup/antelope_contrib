# Mbed-TLS in libslink

The [Mbed-TLS](https://github.com/Mbed-TLS/mbedtls) library is used in
libslink for TLS support.

## Updating the Mbed-TLS version

The Mbed-TLS build system is not used in libslink.

Only a minimal number of the Mbed-TLS release files are needed by ringserver.

Steps to update Mbed-TLS to a newer release:

1) Download a release bundle from: https://github.com/Mbed-TLS/mbedtls/releases
2) Extract the contents in a temporary directory
3) Copy the following files and directories to this directory:
   - `LICENSE`
   - `README.md`
   - `include/`
   - `library/`

Make sure any new files are added and committed.
