*deck r1mach
      real function r1mach(i)
c***begin prologue  r1mach
c***revision date  811015   (yymmdd)
c***category no.  q
c***keywords  machine constants
c***date written  1979
c***author  fox p.a.,.hall a.d., schryer n.l. (bell labs)
c***purpose
c returns single precision machine dependent constants
c***description
c
c     r1mach can be used to obtain machine-dependent parameters
c     for the local machine environment.  it is a function
c     subroutine with one (input) argument, and can be called
c     as follows, for example
c
c          a = r1mach(i)
c
c     where i=1,...,5.  the (output) value of a above is
c     determined by the (input) value of i.  the results for
c     various values of i are discussed below.
c
c  single-precision machine constants
c  r1mach(1) = b**(emin-1), the smallest positive magnitude.
c  r1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
c  r1mach(3) = b**(-t), the smallest relative spacing.
c  r1mach(4) = b**(1-t), the largest relative spacing.
c  r1mach(5) = log10(b)
c
c  to alter this function for a particular environment,
c  the desired set of data statements should be activated by
c  removing the c from column 1.
c
c  where possible, octal or hexadecimal constants have been used
c  to specify the constants exactly which has in some cases
c  required the use of equivalent integer arrays.
c
c***references
c  fox p.a., hall a.d., schryer n.l.,*framework for a portable library*,
c  acm transaction on mathematical software, vol. 4, no. 2,
c  june 1978, pp. 177-188.
c***routines called  xerror
c***end prologue  r1mach
c
c     integer small(2)
c     integer large(2)
c     integer right(2)
c     integer diver(2)
c     integer log10(2)
c
      real rmach(5)
c
c     equivalence (rmach(1),small(1))
c     equivalence (rmach(2),large(1))
c     equivalence (rmach(3),right(1))
c     equivalence (rmach(4),diver(1))
c     equivalence (rmach(5),log10(1))
c
c    for alliant 
c      dimension rmach(5),imach(5)
c      equivalence (rmach,imach)
c
c    constants for alliant computed using
c    b=2
c    t=24
c    emin=-125
c    emax=128
c
c     machine constants for the burroughs 1700 system.
c
c     data rmach(1) / z400800000 /
c     data rmach(2) / z5ffffffff /
c     data rmach(3) / z4e9800000 /
c     data rmach(4) / z4ea800000 /
c     data rmach(5) / z500e730e8 /
c
c     machine constants for the burroughs 5700/6700/7700 systems.
c
c     data rmach(1) / o1771000000000000 /
c     data rmach(2) / o0777777777777777 /
c     data rmach(3) / o1311000000000000 /
c     data rmach(4) / o1301000000000000 /
c     data rmach(5) / o1157163034761675 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data rmach(1) / 00014000000000000000b /
c     data rmach(2) / 37767777777777777777b /
c     data rmach(3) / 16404000000000000000b /
c     data rmach(4) / 16414000000000000000b /
c     data rmach(5) / 17164642023241175720b /
c
c     machine constants for the cray 1
c
c     data rmach(1) / 200004000000000000000b /
c     data rmach(2) / 577777777777777777777b /
c     data rmach(3) / 377214000000000000000b /
c     data rmach(4) / 377224000000000000000b /
c     data rmach(5) / 377774642023241175720b /
c
c     machine constants for the data general eclipse s/200
c
c     note - it may be appropriate to include the following card -
c     static rmach(5)
c
c     data small/20k,0/,large/77777k,177777k/
c     data right/35420k,0/,diver/36020k,0/
c     data log10/40423k,42023k/
c
c     machine constants for the harris 220
c
c     data small(1),small(2) /  20000000,  00000201 /
c     data large(1),large(2) /  37777777,  00000177 /
c     data right(1),right(2) /  20000000,  00000352 /
c     data diver(1),diver(2) /  20000000,  00000353 /
c     data log10(1),log10(2) /  23210115,  00000377 /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data rmach(1) / o402400000000 /
c     data rmach(2) / o376777777777 /
c     data rmach(3) / o714400000000 /
c     data rmach(4) / o716400000000 /
c     data rmach(5) / o776464202324 /
c
c     machine constants for the hp 2100
c
c     3 word double precision with ftn4
c
c     data small(1), small(2) / 40000b,       1 /
c     data large(1), large(2) / 77777b, 177776b /
c     data right(1), right(2) / 40000b,    325b /
c     data diver(1), diver(2) / 40000b,    327b /
c     data log10(1), log10(2) / 46420b,  46777b /
c
c     machine constants for the hp 2100
c     4 word double precision with ftn4
c
c     data small(1), small(2) / 40000b,       1 /
c     data large91), large(2) / 77777b, 177776b /
c     data right(1), right(2) / 40000b,    325b /
c     data diver(1), diver(2) / 40000b,    327b /
c     data log10(1), log10(2) / 46420b,  46777b /
c
c     machine constants for the ibm 360/370 series,
c     the xerox sigma 5/7/9, the sel systems 85/86  and
c     the perkin elmer (interdata) 7/32.
c
c     data rmach(1) / z00100000 /
c     data rmach(2) / z7fffffff /
c     data rmach(3) / z3b100000 /
c     data rmach(4) / z3c100000 /
c     data rmach(5) / z41134413 /
c
c     machine constants for the pdp-10 (ka or ki processor).
c
c     data rmach(1) / "000400000000 /
c     data rmach(2) / "377777777777 /
c     data rmach(3) / "146400000000 /
c     data rmach(4) / "147400000000 /
c     data rmach(5) / "177464202324 /
c
c     machine constants for pdp-11 fortran s supporting
c     32-bit integers (expressed in integer and octal).
c
c     data small(1) /    8388608 /
c     data large(1) / 2147483647 /
c     data right(1) /  880803840 /
c     data diver(1) /  889192448 /
c     data log10(1) / 1067065499 /
c
c     data rmach(1) / o00040000000 /
c     data rmach(2) / o17777777777 /
c     data rmach(3) / o06440000000 /
c     data rmach(4) / o06500000000 /
c     data rmach(5) / o07746420233 /
c
c     machine constants for pdp-11 fortran s supporting
c     16-bit integers  (expressed in integer and octal).
c
c     data small(1),small(2) /   128,     0 /
c     data large(1),large(2) / 32767,    -1 /
c     data right(1),right(2) / 13440,     0 /
c     data diver(1),diver(2) / 13568,     0 /
c     data log10(1),log10(2) / 16282,  8347 /
c
c     data small(1),small(2) / o000200, o000000 /
c     data large(1),large(2) / o077777, o177777 /
c     data right(1),right(2) / o032200, o000000 /
c     data diver(1),diver(2) / o032400, o000000 /
c     data log10(1),log10(2) / o037632, o020233 /
c
c     machine constants for the univac 1100 series.
c
c     data rmach(1) / o000400000000 /
c     data rmach(2) / o377777777777 /
c     data rmach(3) / o146400000000 /
c     data rmach(4) / o147400000000 /
c     data rmach(5) / o177464202324 /
c
c     machine constants for the vax 11/780 using vms
c
c      data small(1) / z00000080 /
c      data large(1) / zffff7fff /
c      data right(1) / z00003480 /
c      data diver(1) / z00003500 /
c      data log10(1) / z20983f9a /
c
c     machine constants for the vax 11/780 using unix
c
c     data small(1) / z'00000080' /
c     data large(1) / z'ffff7fff' /
c     data right(1) / z'00003480' /
c     data diver(1) / z'00003500' /
c     data log10(1) / z'20983f9a' /
c
c     machine constants for apollo domain system
c
c      data rmach(1) /2.9387358e-39/
c      data rmach(2) /1.7014116e38/
c      data rmach(3) /1.1920928e-7/
c      data rmach(4) /2.3841857e-7/
c      data rmach(5) /0.301030/
c
c    constants for alliant computed using
c
c      data (imach(j),j=1,5)/
c     $     8388608,2139095039,864026624,872415232,
c     $     1050288283/
c
c
c
c  code below is only for suns
c  single-precision machine constants
c  r1mach(1) = b**(emin-1), the smallest positive magnitude.
c  r1mach(2) = b**emax*(1 - b**(-t)), the largest magnitude.
c  r1mach(3) = b**(-t), the smallest relative spacing.
c  r1mach(4) = b**(1-t), the largest relative spacing.
c  r1mach(5) = log10(b)
c  constants for sun workstations in single precision
c       b=2 ( binary machine)
c       t=23 (number of bits in the fractional part or mantissa)
c       emin=-127  (smallest exponent)
c       emax=128  (largest exponent)
c
c       integer t,emin,emax
c       data b,t,emin,emax/2.,23,-127,128/
c       if (i.eq.1) then
c               r1mach=b**(emin-1)
c       else if (i.eq.2) then
c               r1mach=b**emax*(1-b**(-t))
c       else if (i.eq.3) then
c               r1mach=b**(-t)
c       else if (i.eq.4) then
c               r1mach=b**(1-t)
c       else if (i.eq.5) then
c               r1mach=log10(b)
c       endif
c
       data rmach(1) /2.9387358e-39/
       data rmach(2) /1.7014116e38/
       data rmach(3) /1.1920928e-7/
       data rmach(4) /2.3841857e-7/
       data rmach(5) /0.301030/
c      data rmach(1) /5.8774716e-39/
c      data rmach(2) /1.7014116e38/
c      data rmach(3) /2.3841857e-7/
c      data rmach(4) /4.7683714e-7/
c      data rmach(5) /0.301030/
c
c***first executable statement  r1mach
c
c      if (i .lt. 1  .or.  i .gt. 5)
c     1   call xerror (25hr1mach -- i out of bounds,25,1,2)
      r1mach = rmach(i)
      return
c
      end

c $Id$ 
