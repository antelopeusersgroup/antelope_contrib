/* @(#)syserr.c	2.3 01/28/97 */
/*======================================================================
 *
 *  Generate error message strings from errno, in case strerror() is
 *  not available.
 *
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Copyright (c) 1997 Regents of the University of California.
 * All rights reserved.
 *====================================================================*/
#include <stdio.h>
#include <errno.h>
#include "util.h"

char *syserrmsg(code)
int code;
{
static char *mesg;
static char other[] = "error 0xNNNNNNNN";

    if (1 == 2) {
#ifdef E2BIG
    } else if (code == E2BIG) {
        mesg = "Arg list too long";
#endif
#ifdef EACCES
    } else if (code == EACCES) {
        mesg = "Permission denied";
#endif
#ifdef EADDRINUSE
    } else if (code == EADDRINUSE) {
        mesg = "Address already in use";
#endif
#ifdef EADDRNOTAVAIL
    } else if (code == EADDRNOTAVAIL) {
        mesg = "Can't assign requested address";
#endif
#ifdef EADV
    } else if (code == EADV) {
        mesg = "Advertise error";
#endif
#ifdef EAFNOSUPPORT
    } else if (code == EAFNOSUPPORT) {
        mesg = "Address family not supported by protocol family";
#endif
#ifdef EAGAIN
    } else if (code == EAGAIN) {
        mesg = "No more processes";
#endif
#ifdef EALREADY
    } else if (code == EALREADY) {
        mesg = "Operation already in progress";
#endif
#ifdef EBADE
    } else if (code == EBADE) {
        mesg = "Bad exchange descriptor";
#endif
#ifdef EBADF
    } else if (code == EBADF) {
        mesg = "Bad file number";
#endif
#ifdef EBADFD
    } else if (code == EBADFD) {
        mesg = "Bad file descriptor";
#endif
#ifdef EBADMSG
    } else if (code == EBADMSG) {
        mesg = "Not a data message";
#endif
#ifdef EBADR
    } else if (code == EBADR) {
        mesg = "Bad request descriptor";
#endif
#ifdef EBADRQC
    } else if (code == EBADRQC) {
        mesg = "Bad request code";
#endif
#ifdef EBADSLT
    } else if (code == EBADSLT) {
        mesg = "Invalid slot";
#endif
#ifdef EBFONT
    } else if (code == EBFONT) {
        mesg = "Bad font file format";
#endif
#ifdef EBUSY
    } else if (code == EBUSY) {
        mesg = "Device busy";
#endif
#ifdef ECANCELED
    } else if (code == ECANCELED) {
        mesg = "Operation canceled";
#endif
#ifdef ECHILD
    } else if (code == ECHILD) {
        mesg = "No children";
#endif
#ifdef ECHRNG
    } else if (code == ECHRNG) {
        mesg = "Channel number out of range";
#endif
#ifdef ECOMM
    } else if (code == ECOMM) {
        mesg = "Communication error on send";
#endif
#ifdef ECONNABORTED
    } else if (code == ECONNABORTED) {
        mesg = "Software caused connection abort";
#endif
#ifdef ECONNREFUSED
    } else if (code == ECONNREFUSED) {
        mesg = "Connection refused";
#endif
#ifdef ECONNRESET
    } else if (code == ECONNRESET) {
        mesg = "Connection reset by peer";
#endif
#ifdef EDEADLK
    } else if (code == EDEADLK) {
        mesg = "Deadlock condition.";
#endif
#ifdef EDEADLOCK
    } else if (code == EDEADLOCK) {
        mesg = "File locking deadlock";
#endif
#ifdef EDESTADDRREQ
    } else if (code == EDESTADDRREQ) {
        mesg = "Destination address required";
#endif
#ifdef EDOM
    } else if (code == EDOM) {
        mesg = "Argument out of domain";
#endif
#ifdef EDOTDOT
    } else if (code == EDOTDOT) {
        mesg = "Cross mount point (not an error)";
#endif
#ifdef EDQUOT
    } else if (code == EDQUOT) {
        mesg = "Disc quota exceeded";
#endif
#ifdef EEXIST
    } else if (code == EEXIST) {
        mesg = "File exists";
#endif
#ifdef EFAULT
    } else if (code == EFAULT) {
        mesg = "Bad address";
#endif
#ifdef EFBIG
    } else if (code == EFBIG) {
        mesg = "File too large";
#endif
#ifdef EHOSTDOWN
    } else if (code == EHOSTDOWN) {
        mesg = "Host is down";
#endif
#ifdef EHOSTUNREACH
    } else if (code == EHOSTUNREACH) {
        mesg = "No route to host";
#endif
#ifdef EIDRM
    } else if (code == EIDRM) {
        mesg = "Identifier removed";
#endif
#ifdef EILSEQ
    } else if (code == EILSEQ) {
        mesg = "Illegal byte sequence.";
#endif
#ifdef EINPROGRESS
    } else if (code == EINPROGRESS) {
        mesg = "Operation now in progress";
#endif
#ifdef EINTR
    } else if (code == EINTR) {
        mesg = "Interrupted system call";
#endif
#ifdef EINVAL
    } else if (code == EINVAL) {
        mesg = "Invalid argument";
#endif
#ifdef EIO
    } else if (code == EIO) {
        mesg = "I/O error";
#endif
#ifdef EISCONN
    } else if (code == EISCONN) {
        mesg = "Socket is already connected";
#endif
#ifdef EISDIR
    } else if (code == EISDIR) {
        mesg = "Is a directory";
#endif
#ifdef EL2HLT
    } else if (code == EL2HLT) {
        mesg = "Level 2 halted";
#endif
#ifdef EL2NSYNC
    } else if (code == EL2NSYNC) {
        mesg = "Level 2 not synchronized";
#endif
#ifdef EL3HLT
    } else if (code == EL3HLT) {
        mesg = "Level 3 halted";
#endif
#ifdef EL3RST
    } else if (code == EL3RST) {
        mesg = "Level 3 reset";
#endif
#ifdef ELBIN
    } else if (code == ELBIN) {
        mesg = "Inode is remote (not really error)";
#endif
#ifdef ELIBACC
    } else if (code == ELIBACC) {
        mesg = "Can not access a needed shared library";
#endif
#ifdef ELIBBAD
    } else if (code == ELIBBAD) {
        mesg = "Accessing a corrupted shared library";
#endif
#ifdef ELIBEXEC
    } else if (code == ELIBEXEC) {
        mesg = "Attempting to exec a shared library.";
#endif
#ifdef ELIBMAX
    } else if (code == ELIBMAX) {
        mesg = "Attempting to link in too many libs.";
#endif
#ifdef ELIBSCN
    } else if (code == ELIBSCN) {
        mesg = ".lib section in a.out corrupted.";
#endif
#ifdef ELNRNG
    } else if (code == ELNRNG) {
        mesg = "Link number out of range";
#endif
#ifdef ELOOP
    } else if (code == ELOOP) {
        mesg = "Symbolic link loop";
#endif
#ifdef EMFILE
    } else if (code == EMFILE) {
        mesg = "Too many open files";
#endif
#ifdef EMLINK
    } else if (code == EMLINK) {
        mesg = "Too many links";
#endif
#ifdef EMSGSIZE
    } else if (code == EMSGSIZE) {
        mesg = "Message too long";
#endif
#ifdef EMULTIHOP
    } else if (code == EMULTIHOP) {
        mesg = "Multihop attempted";
#endif
#ifdef ENAMETOOLONG
    } else if (code == ENAMETOOLONG) {
        mesg = "Path name is too long";
#endif
#ifdef ENETDOWN
    } else if (code == ENETDOWN) {
        mesg = "Network is down";
#endif
#ifdef ENETRESET
    } else if (code == ENETRESET) {
        mesg = "Network dropped connection on reset";
#endif
#ifdef ENETUNREACH
    } else if (code == ENETUNREACH) {
        mesg = "Network is unreachable";
#endif
#ifdef ENFILE
    } else if (code == ENFILE) {
        mesg = "File table overflow";
#endif
#ifdef ENOANO
    } else if (code == ENOANO) {
        mesg = "No anode";
#endif
#ifdef ENOBUFS
    } else if (code == ENOBUFS) {
        mesg = "No buffer space available";
#endif
#ifdef ENOCSI
    } else if (code == ENOCSI) {
        mesg = "No CSI structure available";
#endif
#ifdef ENODATA
    } else if (code == ENODATA) {
        mesg = "No data available";
#endif
#ifdef ENODEV
    } else if (code == ENODEV) {
        mesg = "No such device";
#endif
#ifdef ENOENT
    } else if (code == ENOENT) {
        mesg = "No such file or directory";
#endif
#ifdef ENOEXEC
    } else if (code == ENOEXEC) {
        mesg = "Exec format error";
#endif
#ifdef ENOLCK
    } else if (code == ENOLCK) {
        mesg = "No record locks available.";
#endif
#ifdef ENOLINK
    } else if (code == ENOLINK) {
        mesg = "Link has been severed";
#endif
#ifdef ENOMEM
    } else if (code == ENOMEM) {
        mesg = "Not enough core";
#endif
#ifdef ENOMSG
    } else if (code == ENOMSG) {
        mesg = "No message of desired type";
#endif
#ifdef ENONET
    } else if (code == ENONET) {
        mesg = "Machine is not on the network";
#endif
#ifdef ENOPKG
    } else if (code == ENOPKG) {
        mesg = "Package not installed";
#endif
#ifdef ENOPROTOOPT
    } else if (code == ENOPROTOOPT) {
        mesg = "Protocol not available";
#endif
#ifdef ENOSPC
    } else if (code == ENOSPC) {
        mesg = "No space left on device";
#endif
#ifdef ENOSR
    } else if (code == ENOSR) {
        mesg = "Out of streams resources";
#endif
#ifdef ENOSTR
    } else if (code == ENOSTR) {
        mesg = "Not a stream device";
#endif
#ifdef ENOSYS
    } else if (code == ENOSYS) {
        mesg = "Function not implemented";
#endif
#ifdef ENOTBLK
    } else if (code == ENOTBLK) {
        mesg = "Block device required";
#endif
#ifdef ENOTCONN
    } else if (code == ENOTCONN) {
        mesg = "Socket is not connected";
#endif
#ifdef ENOTDIR
    } else if (code == ENOTDIR) {
        mesg = "Not a directory";
#endif
#ifdef ENOTEMPTY
    } else if (code == ENOTEMPTY) {
        mesg = "Directory not empty";
#endif
#ifdef ENOTSOCK
    } else if (code == ENOTSOCK) {
        mesg = "Socket operation on non-socket";
#endif
#ifdef ENOTSUP
    } else if (code == ENOTSUP) {
        mesg = "Operation not supported";
#endif
#ifdef ENOTTY
    } else if (code == ENOTTY) {
        mesg = "Inappropriate ioctl for device";
#endif
#ifdef ENOTUNIQ
    } else if (code == ENOTUNIQ) {
        mesg = "Name not unique on network";
#endif
#ifdef ENXIO
    } else if (code == ENXIO) {
        mesg = "No such device or address";
#endif
#ifdef EOPNOTSUPP
    } else if (code == EOPNOTSUPP) {
        mesg = "Operation not supported on socket";
#endif
#ifdef EOVERFLOW
    } else if (code == EOVERFLOW) {
        mesg = "Value too large to be stored in data type";
#endif
#ifdef EPERM
    } else if (code == EPERM) {
        mesg = "Not owner";
#endif
#ifdef EPFNOSUPPORT
    } else if (code == EPFNOSUPPORT) {
        mesg = "Protocol family not supported";
#endif
#ifdef EPIPE
    } else if (code == EPIPE) {
        mesg = "Broken pipe";
#endif
#ifdef EPROCLIM
    } else if (code == EPROCLIM) {
        mesg = "Too many processes";
#endif
#ifdef EPROTO
    } else if (code == EPROTO) {
        mesg = "Protocol error";
#endif
#ifdef EPROTONOSUPPORT
    } else if (code == EPROTONOSUPPORT) {
        mesg = "Protocol not supported";
#endif
#ifdef EPROTOTYPE
    } else if (code == EPROTOTYPE) {
        mesg = "Protocol wrong type for socket";
#endif
#ifdef ERANGE
    } else if (code == ERANGE) {
        mesg = "Result too large";
#endif
#ifdef EREMCHG
    } else if (code == EREMCHG) {
        mesg = "Remote address changed";
#endif
#ifdef EREMOTE
    } else if (code == EREMOTE) {
        mesg = "Object is remote";
#endif
#ifdef ERESTART
    } else if (code == ERESTART) {
        mesg = "Restartable system call";
#endif
#ifdef EROFS
    } else if (code == EROFS) {
        mesg = "Read only file system";
#endif
#ifdef ERREMOTE
    } else if (code == ERREMOTE) {
        mesg = "Object is remote";
#endif
#ifdef ESHUTDOWN
    } else if (code == ESHUTDOWN) {
        mesg = "Can't send after socket shutdown";
#endif
#ifdef ESOCKTNOSUPPORT
    } else if (code == ESOCKTNOSUPPORT) {
        mesg = "Socket type not supported";
#endif
#ifdef ESPIPE
    } else if (code == ESPIPE) {
        mesg = "Illegal seek";
#endif
#ifdef ESRCH
    } else if (code == ESRCH) {
        mesg = "No such process";
#endif
#ifdef ESRMNT
    } else if (code == ESRMNT) {
        mesg = "srmount error";
#endif
#ifdef ESTALE
    } else if (code == ESTALE) {
        mesg = "Stale NFS file handle";
#endif
#ifdef ESTRPIPE
    } else if (code == ESTRPIPE) {
        mesg = "If pipe/FIFO, don't sleep in stream head";
#endif
#ifdef ETIME
    } else if (code == ETIME) {
        mesg = "Timer expired";
#endif
#ifdef ETIMEDOUT
    } else if (code == ETIMEDOUT) {
        mesg = "Connection timed out";
#endif
#ifdef ETOOMANYREFS
    } else if (code == ETOOMANYREFS) {
        mesg = "Too many references: can't splice";
#endif
#ifdef ETXTBSY
    } else if (code == ETXTBSY) {
        mesg = "Text file busy";
#endif
#ifdef EUNATCH
    } else if (code == EUNATCH) {
        mesg = "Protocol driver not attached";
#endif
#ifdef EUSERS
    } else if (code == EUSERS) {
        mesg = "Too many users";
#endif
#ifdef EWOULDBLOCK
    } else if (code == EWOULDBLOCK) {
        mesg = "Operation would block";
#endif
#ifdef EXDEV
    } else if (code == EXDEV) {
        mesg = "Cross-device link";
#endif
#ifdef EXFULL
    } else if (code == EXFULL) {
        mesg = "Message tables full";
#endif
    } else {
        sprintf(other, "error 0x%x", code);
        mesg = other;
    }

    return mesg;
}
