*deck d1mach
      double precision function d1mach(i)
c***begin prologue  d1mach
c***revision date  811015   (yymmdd)
c***category no.  q
c***keywords  machine constants,double precision
c***date written  1975
c***author  fox p.a., hall a.d., schryer n.l. (bell labs)
c***purpose
c returns double precision machine dependent constants
c***description
c
c     d1mach can be used to obtain machine-dependent parameters
c     for the local machine environment.  it is a function
c     subroutine with one (input) argument, and can be called
c     as follows, for example
c
c          d = d1mach(i)
c
c     where i=1,...,5.  the (output) value of d above is
c     determined by the (input) value of i.  the results for
c     various values of i are discussed below.
c
c  double-precision machine constants
c  d1mach( 1) = b**(emin-1), the smallest positive magnitude.
c  d1mach( 2) = b**emax*(1 - b**(-t)), the largest magnitude.
c  d1mach( 3) = b**(-t), the smallest relative spacing.
c  d1mach( 4) = b**(1-t), the largest relative spacing.
c  d1mach( 5) = log10(b)
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
c***end prologue  d1mach
c
c     integer small(4)
c     integer large(4)
c     integer right(4)
c     integer diver(4)
c     integer log10(4)
c
c     double precision dmach(5)
c
c      equivalence (dmach(1),small(1))
c      equivalence (dmach(2),large(1))
c      equivalence (dmach(3),right(1))
c      equivalence (dmach(4),diver(1))
c      equivalence (dmach(5),log10(1))
c
c  for alliant 
c      double precision dmach(5)
c      integer small(2),big(2),smrel(2),lgrel(2),log2(2)
c      equivalence (dmach(1),small(1)),(dmach(2),big(1)),
c     $            (dmach(3),smrel(1)),(dmach(4),lgrel(1)),
c     $            (dmach(5),log2(1))
c
c
c     machine constants for the burroughs 1700 system.
c
c     data small(1) / zc00800000 /
c     data small(2) / z000000000 /
c
c     data large(1) / zdffffffff /
c     data large(2) / zfffffffff /
c
c     data right(1) / zcc5800000 /
c     data right(2) / z000000000 /
c
c     data diver(1) / zcc6800000 /
c     data diver(2) / z000000000 /
c
c     data log10(1) / zd00e730e7 /
c     data log10(2) / zc77800dc0 /
c
c     machine constants for the burroughs 5700 system.
c
c     data small(1) / o1771000000000000 /
c     data small(2) / o0000000000000000 /
c
c     data large(1) / o0777777777777777 /
c     data large(2) / o0007777777777777 /
c
c     data right(1) / o1461000000000000 /
c     data right(2) / o0000000000000000 /
c
c     data diver(1) / o1451000000000000 /
c     data diver(2) / o0000000000000000 /
c
c     data log10(1) / o1157163034761674 /
c     data log10(2) / o0006677466732724 /
c
c     machine constants for the burroughs 6700/7700 systems.
c
c     data small(1) / o1771000000000000 /
c     data small(2) / o7770000000000000 /
c
c     data large(1) / o0777777777777777 /
c     data large(2) / o7777777777777777 /
c
c     data right(1) / o1461000000000000 /
c     data right(2) / o0000000000000000 /
c
c     data diver(1) / o1451000000000000 /
c     data diver(2) / o0000000000000000 /
c
c     data log10(1) / o1157163034761674 /
c     data log10(2) / o0006677466732724 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data small(1) / 00604000000000000000b /
c     data small(2) / 00000000000000000000b /
c
c     data large(1) / 37767777777777777777b /
c     data large(2) / 37167777777777777777b /
c
c     data right(1) / 15604000000000000000b /
c     data right(2) / 15000000000000000000b /
c
c     data diver(1) / 15614000000000000000b /
c     data diver(2) / 15010000000000000000b /
c
c     data log10(1) / 17164642023241175717b /
c     data log10(2) / 16367571421742254654b /
c
c     machine constants for the cray 1
c
c     data small(1) / 200004000000000000000b /
c     data small(2) / 00000000000000000000b /
c
c     data large(1) / 577777777777777777777b /
c     data large(2) / 000007777777777777777b /
c
c     data right(1) / 377214000000000000000b /
c     data right(2) / 000000000000000000000b /
c
c     data diver(1) / 377224000000000000000b /
c     data diver(2) / 000000000000000000000b /
c
c     data log10(1) / 377774642023241175717b /
c     data log10(2) / 000007571421742254654b /
c
c     machine constants for the data general eclipse s/200
c
c     note - it may be appropriate to include the following card -
c     static dmach(5)
c
c     data small/20k,3*0/,large/77777k,3*177777k/
c     data right/31420k,3*0/,diver/32020k,3*0/
c     data log10/40423k,42023k,50237k,74776k/
c
c     machine constants for the harris 220
c
c     data small(1),small(2) /  20000000,  00000201 /
c     data large(1),large(2) /  37777777,  37777577 /
c     data right(1),right(2) /  20000000,  00000333 /
c     data diver(1),diver(2) /  20000000,  00000334 /
c     data log10(1),log10(2) /  23210115,  10237777 /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data small(1),small(2) / o402400000000, o000000000000 /
c     data large(1),large(2) / o376777777777, o777777777777 /
c     data right(1),right(2) / o604400000000, o000000000000 /
c     data diver(1),diver(2) / o606400000000, o000000000000 /
c     data log10(1),log10(2) / o776464202324, o117571775714 /
c
c      machine constants for the hp 2100
c      three word double precision option with ftn4
c
c      data small(1), small(2), small(3) / 40000b,       0,       1 /
c      data large(1), large(2), large(3) / 77777b, 177777b, 177776b /
c      data right(1), right(2), right(3) / 40000b,       0,    265b /
c      data diver(1), diver(2), diver(3) / 40000b,       0,    276b /
c      data log10(1), log10(2), log10(3) / 46420b,  46502b,  77777b /
c
c
c      machine constants for the hp 2100
c      four word double precision option with ftn4
c
c      data small(1), small(2) /  40000b,       0 /
c      data small(3), small(4) /       0,       1 /
c      data large(1), large(2) /  77777b, 177777b /
c      data large(3), large(4) / 177777b, 177776b /
c      data right(1), right(2) /  40000b,       0 /
c      data right(3), right(4) /       0,    225b /
c      data diver(1), diver(2) /  40000b,       0 /
c      data diver(3), diver(4) /       0,    227b /
c      data log10(1), log10(2) /  46420b,  46502b /
c      data log10(3), log10(4) /  76747b, 176377b /
c
c
c     machine constants for the ibm 360/370 series,
c     the xerox sigma 5/7/9, the sel systems 85/86, and
c     the perkin elmer (interdata) 7/32.
c
c     data small(1),small(2) / z00100000, z00000000 /
c     data large(1),large(2) / z7fffffff, zffffffff /
c     data right(1),right(2) / z33100000, z00000000 /
c     data diver(1),diver(2) / z34100000, z00000000 /
c     data log10(1),log10(2) / z41134413, z509f79ff /
c
c     machine constants for the pdp-10 (ka processor).
c
c     data small(1),small(2) / "033400000000, "000000000000 /
c     data large(1),large(2) / "377777777777, "344777777777 /
c     data right(1),right(2) / "113400000000, "000000000000 /
c     data diver(1),diver(2) / "114400000000, "000000000000 /
c     data log10(1),log10(2) / "177464202324, "144117571776 /
c
c     machine constants for the pdp-10 (ki processor).
c
c     data small(1),small(2) / "000400000000, "000000000000 /
c     data large(1),large(2) / "377777777777, "377777777777 /
c     data right(1),right(2) / "103400000000, "000000000000 /
c     data diver(1),diver(2) / "104400000000, "000000000000 /
c     data log10(1),log10(2) / "177464202324, "476747767461 /
c
c     machine constants for pdp-11 fortran s supporting
c     32-bit integers (expressed in integer and octal).
c
c     data small(1),small(2) /    8388608,           0 /
c     data large(1),large(2) / 2147483647,          -1 /
c     data right(1),right(2) /  612368384,           0 /
c     data diver(1),diver(2) /  620756992,           0 /
c     data log10(1),log10(2) / 1067065498, -2063872008 /
c
c     data small(1),small(2) / o00040000000, o00000000000 /
c     data large(1),large(2) / o17777777777, o37777777777 /
c     data right(1),right(2) / o04440000000, o00000000000 /
c     data diver(1),diver(2) / o04500000000, o00000000000 /
c     data log10(1),log10(2) / o07746420232, o20476747770 /
c
c     machine constants for pdp-11 fortran s supporting
c     16-bit integers (expressed in integer and octal).
c
c     data small(1),small(2) /    128,      0 /
c     data small(3),small(4) /      0,      0 /
c
c     data large(1),large(2) /  32767,     -1 /
c     data large(3),large(4) /     -1,     -1 /
c
c     data right(1),right(2) /   9344,      0 /
c     data right(3),right(4) /      0,      0 /
c
c     data diver(1),diver(2) /   9472,      0 /
c     data diver(3),diver(4) /      0,      0 /
c
c     data log10(1),log10(2) /  16282,   8346 /
c     data log10(3),log10(4) / -31493, -12296 /
c
c     data small(1),small(2) / o000200, o000000 /
c     data small(3),small(4) / o000000, o000000 /
c
c     data large(1),large(2) / o077777, o177777 /
c     data large(3),large(4) / o177777, o177777 /
c
c     data right(1),right(2) / o022200, o000000 /
c     data right(3),right(4) / o000000, o000000 /
c
c     data diver(1),diver(2) / o022400, o000000 /
c     data diver(3),diver(4) / o000000, o000000 /
c
c     data log10(1),log10(2) / o037632, o020232 /
c     data log10(3),log10(4) / o102373, o147770 /
c
c     machine constants for the univac 1100 series. ftn compiler
c
c     data small(1),small(2) / o000040000000, o000000000000 /
c     data large(1),large(2) / o377777777777, o777777777777 /
c     data right(1),right(2) / o170540000000, o000000000000 /
c     data diver(1),diver(2) / o170640000000, o000000000000 /
c     data log10(1),log10(2) / o177746420232, o411757177572 /
c
c     machine constants for the univac 1100 series. for compiler
c
c     data small(1), small(2) / o000040000000, o000000000000 /
c     data large(1), large(2) / o377777777777, o777777777777 /
c     data right(1), right(2) / o170540000000, o000000000000 /
c     data diver(1), diver(2) / o170640000000, o000000000000 /
c     data log10(1), log10(2) / o177746420232, o411757177572/
c
c
c     machine constants for vax 11/780
c     (expressed in integer and hexadecimal)
c
c     data small(1), small(2) /        128,           0 /
c     data large(1), large(2) /     -32769,          -1 /
c     data right(1), right(2) /       9344,           0 /
c     data diver(1), diver(2) /       9472,           0 /
c     data log10(1), log10(2) /  546979738,  -805665541 /
 
c      data small(1), small(2) / z00000080, z00000000 /
c      data large(1), large(2) / zffff7fff, zffffffff /
c      data right(1), right(2) / z00002480, z00000000 /
c      data diver(1), diver(2) / z00002500, z00000000 /
c      data log10(1), log10(2) / z209a3f9a, zcffa84fb /
c
c
c     machine constants for apollo domain system
c     (expressed in integer)
c
c      data dmach(1) /0.5562684646268003d-308/
c      data dmach(2) /0.8988465674311577d+308/
c      data dmach(3) /0.2220446049250313d-15/
c      data dmach(4) /0.4440892098500626d-15/
c      data dmach(5) /0.3010299956639811/
c
c  constants for alliant computed using
c    b=2
c    t=53
c    emin=-1021
c    emax=1024
c
c      data (small(j),j=1,2)/1048576,124/
c      data (big(j),j=1,2)/2147483647,-1/
c      data (smrel(j),j=1,2)/1017118720,3/
c      data (lgrel(j),j=1,2)/1018167296,11/
c      data (log2(j),j=1,2)/1070810131,1352628735/
c
c***first executable statement  d1mach
c
c     if (i .lt. 1  .or.  i .gt. 5) then
c       write(*,*) ' i out of bounds in d1mach'
c       stop
c     end if
c
c     d1mach = dmach(i)
c
c  code below is only for suns
c
c  constants for sun workstation in double precision
c       b=2.
c       t=52
c       emin=-1023
c       emax=1024
c        
        double precision b
        integer t,emax,emin
        data b,t,emin,emax/2.0d0,52,-1023,1024/
        if (i.eq.1) then
                d1mach=b**(emin-1)
        else if (i.eq.2) then
                d1mach=b**(emax)*(1-b**(-t))
        else if (i.eq.3) then
                d1mach=b**(-t)
        else if (i.eq.4) then
                d1mach=b**(1-t)
        else if (i.eq.5) then
                d1mach=dlog10(b)
        endif
      return
c
      end

c $Id$ 
