!*****************************************************
!
! Changed by G. Bokelmann, Dec. 1989
!
!   routine AUTOCORR calculates the autocorrelation for ss,
!   using an FFT. Output is auto, and ss is unchanged.
!   Normalized so that autocorr(unit delta) = unit delta
!
      subroutine autocorr(ss, ns, auto)
      parameter (ndmax = 65536)
      real ss(*)
!
      real auto(ndmax), st(ndmax)
      nss = 2 * ns
      if (nss .gt. ndmax) then
          write(unit=6, fmt=*)' Error in AUTOCORR - maximum dimension ',
     &'exceeded - abandon '
          stop
!
      end if
      call tsreverse(nss, ss, st)
      call tszero(st, ns)
!
      call convolvnp(ss, st, nss, auto)
      call tsreverse(nss, auto, st)
!
      call tscopy(st, auto, nss)
      return 
      end

!*****************************************************
!
!   routine CONVOLV forms the convolution of s1 & s2 using an FFT.
!   s1,s2 are unchanged, & output is ss.
!   Normalization is such that unit delta * unit delta = unit delta
!   Time series is padded to double length before 
!   padding to power of two, to avoid wrap around.
!
      subroutine convolv(s1, s2, ns, ss)
      parameter (ndmax = 65536)
      real s1(*), s2(*)
!
      real ss(ndmax), st1(ndmax), st2(ndmax)
      nss = 2 * ns
      if (nss .gt. ndmax) then
          write(unit=6, fmt=*) ' Error in CONVOLV - maximum dimension', 
     &' exceeded - abandon '
          stop
!
      end if
      call tszero(st1, nss)
      call tszero(st2, nss)
      call tscopy(s1, st1, ns)
!
      call tscopy(s2, st2, ns)
!
      lens = nss
      call two(st1, lens)
      call fftl(st1, lens, 1, ierr)
!
      call fftl(st2, lens, 1, ierr)
      len2 = lens / 2
      do i = 1, len2 + 1
      ir = (2 * i) - 1
      ii = ir + 1
      sr = st1(ir)
      si = st1(ii)
      tr = st2(ir)
!       write (6,*) 'i,ir,ii,sr,si,tr,ti =',i,ir,ii,sr,si,tr,ti
      ti = st2(ii)
      st1(ir) = (0.5 * ((sr * tr) - (si * ti))) * float(lens)
      st1(ii) = (0.5 * ((sr * ti) + (si * tr))) * float(lens)
!
      end do
      call fftl(st1, lens, 2, ierr)
!
      call tscopy(st1, ss, nss)
      return 
      end
!*****************************************************
!
!   routine CONVOLVNP forms the convolution of s1 & s2 using an FFT.
!   s1,s2 are unchanged, & output is ss.
!   all arrays should be dimensioned ns<65536
!   Normalization is such that unit delta * unit delta = unit delta
!
      subroutine convolvnp(s1, s2, ns, ss)
      parameter (ndmax = 65538)
      real s1(*), s2(*), ss(*)
!
      real st(ndmax)
      if ((ns + 2) .gt. ndmax) then
          write(unit=6, fmt=*) ' Error 1 in CONVOLVNP - maximum '
     &, 'dimension exceeded - abandon '
          stop
!
      end if
      do i = 1, ns
      ss(i) = s1(i)
      st(i) = s2(i)
      end do
!
      lens = ns
      call two(ss, lens)
      if ((lens + 2) .gt. ndmax) then
          write(unit=6, fmt=*) ' Error 2 in CONVOLVNP - maximum '
     &, 'dimension exceeded - abandon '
          stop
!
      end if
      call fftl(ss, lens, 1, ierr)
!
      call fftl(st, lens, 1, ierr)
      len2 = lens / 2
      do i = 1, len2 + 1
          ir = (2 * i) - 1
          ii = ir + 1
          sr = ss(ir)
          si = ss(ii)
          tr = st(ir)
!       write (6,*) 'i,ir,ii,sr,si,tr,ti =',i,ir,ii,sr,si,tr,ti
          ti = st(ii)
          ss(ir) = (0.5 * ((sr * tr) - (si * ti))) * float(lens)
          ss(ii) = (0.5 * ((sr * ti) + (si * tr))) * float(lens)
!
      end do
!
      call fftl(ss, lens, 2, ierr)
      return 
!
      end

!*****************************************************
!
!  cosine taper with low frequency truncation
!

      subroutine cosfilt(a, lenf, df, f1, f2, f3, f4)
      complex a(1)
      if1 = (f1 / df) + 1.5
      if2 = (f2 / df) + 1.5
      if3 = (f3 / df) + 1.5
      if4 = (f4 / df) + 1.5
      call ftaper(a(if1), (if2 - if1) + 1, -1)
      call ftaper(a(if3), (if4 - if3) + 1, 1)
      call zero(a, 1, if1 - 1)
!         write(6,*)'cosine taper'
      call zero(a, if4 + 1, lenf)
      return 
      end

!*****************************************************
!
!   APPLIES COSINE TAPER (10% OR 50%)
!   IWINDO=3  50% (CORRESPONDS TO HANNING WINDOW)
!   IWINDO=4  5%
!
      subroutine costap(x, npts, iwindo)
      dimension x(1)
      pi = 3.141592654
      percnt = float(iwindo - 3) * 5.
      if (percnt .le. 1.0) percnt = 50.0
      icos1 = npts * (percnt / 100.0)
      icos2 = (npts - icos1) + 1
      ia = 1
      ib = icos1
      iend = 1
      j = 1
      f = pi / float(icos1 - 1)

110   do i = ia, ib
          fi = float(i - 1)
          if (iend .eq. 1) then
              x(j) = (x(j) * (1. - cos(fi * f))) * 0.5
              j = j + 1
          else if (iend .eq. 2) then
!             X(J)=X(J)*(1.+COS(FI*F))*0.5
              x(j) = (x(j) * (1. - cos(fi * f))) * 0.5
              j = j - 1
!             X(J)=X(J)*(1.-COS(FI*F))*0.5 + SIGN(0.5,X(J))
          end if
      end do
      if (iend .eq. 2) return 
      iend = 2
      j = npts
      goto 110
      end

!*****************************************************
!
!   routine CROSSCORR forms the cross-correlation of two time
!   series s1,s2 of length ns, and places the result in ss.
!
      subroutine crosscorr(s1, s2, ns, ss)
      parameter (ndmax = 65536)
      real s1(*), s2(*)
!
!   pad series to double length
      real ss(ndmax), st(ndmax), su(ndmax)
      nss = 2 * ns
      if (nss .gt. ndmax) then
          write(unit=6, fmt=*) ' Error in CROSSCORR - maximum ', 
     &'dimension exceeded - abandon'
          stop
!
!    copy double length series S1 in reverse order to ST
      end if
!   make sure first NS entries of ST are zero
      call tsreverse(nss, s1, st)
!
      call tszero(st, ns)
!   copy S2 to first NS entries of SU
      call tszero(su, nss)
!
      call tscopy(s2, su, ns)
!
      call convolvnp(st, su, nss, ss)
      call tsreverse(nss, ss, st)
!
      call tscopy(st, ss, nss)
      return 
      end

!*****************************************************
!
!   routine CROSSPEC calculates the cross-spectrum (sum of AB*)for s1,s2
!   (elements n1 to n2), and returns it as CSP (complex).
!
      subroutine crosspec(s1, s2, n1, n2, csp)

      complex s1(*), s2(*), csp, z
      csp = cmplx(0.0,0.0)
      do i = n1, n2
          z = s1(i) * conjg(s2(i))
          csp = csp + z
      end do
      return 
      end

!*****************************************************
      subroutine dcm(a, adec, n, icnt, xlen, idec)
      common /ppt/ ppimax
      dimension a(1), adec(1)
      ppi = float(n) / xlen
      idec = ((ppi - 1) / ppimax) + 1.

      if (idec .eq. 1) then
          do 10 i = 1, n
              adec(i) = a(i)
10        continue
          icnt = n
          return 
      end if

      id2 = idec * 2
      icnt = 0
      do 20 i = 1, n, id2
          j = min(id2,(n - i) + 1)
          call scdcm(a(i), j, a1, a2)
          icnt = icnt + 1
          adec(icnt) = a1
          icnt = icnt + 1
          adec(icnt) = a2
20    continue
      return 
      end

!*****************************************************
      subroutine dtr(a, n, ifl)
      dimension a(1)
      if (ifl .eq. 1) then
          call demean(a, n)
      else if (ifl .eq. 2) then
          call demean(a, n)
          call dtrend(a, n)
      else
      write(unit=*, fmt='('' INVALID VALUE OF IFL IN DTR.PGM ABORTED'')'
     &) 
      stop
      end if
      return 
      end

!*****************************************************
      subroutine demean(a, n)
      dimension a(1)
      ave = 0.
      do 10 i = 1, n
   10     ave = ave + a(i)

      ave = ave / float(n)
      do 20 i = 1, n
   20     a(i) = a(i) - ave

      return 
      end

!*****************************************************
      subroutine dtrend(a, n)
      dimension a(1)
      xx = 0.
      xa = 0.
      cent = float(n + 1) / 2.

      do 10 i = 1, n
          x = float(i) - cent
          xx = xx + (x ** 2)
   10     xa = xa + (x * a(i))

      slope = xa / xx

      do 20 i = 1, n
          x = float(i) - cent
   20     a(i) = a(i) - (slope * x)

      return 
      end

!*****************************************************
!
! $$$$$ CALLS FFT AND REALTR $$$$$
!
!   IF IABS(NDIR).EQ.1 FFTL FOURIER TRANSFORMS THE N POINT REAL TIME
!   SERIES IN ARRAY X.  THE RESULT OVERWRITES X STORED AS
!   (N+2)/2 COMPLEX NUMBERS (NON-NEGATIVE FREQUENCIES ONLY).  IF
!   IABS(NDIR).EQ.2 FFTL FOURIER TRANSFORMS THE (N+2)/2 COMPLEX FOURIER
!   COEFFICIENTS (NON-NEGATIVE FREQUENCIES ONLY) IN ARRAY X
!   (ASSUMING THE SERIES IS HERMITIAN).  THE RESULTING N POINT REAL
!   TIME SERIES OVERWRITES X.  IF NDIR.GT.0 THE FOREWARD TRANSFORM USES
!   THE SIGN CONVENTION EXP(I*W*T).  IF NDIR.LT.0 THE FOREWARD TRANSFORM
!   USES THE SIGN CONVENTION EXP(-I*W*T).  THE FOREWARD TRANSFORM IS
!   NORMALIZED SUCH THAT A SINE WAVE OF UNIT AMPLITUDE IS TRANSFORMED
!   INTO DELTA FUNCTIONS OF UNIT AMPLITUDE.  THE BACKWARDS TRANSFORM IS
!   NORMALIZED SUCH THAT TRANSFORMING FOREWARD AND THEN BACK RECOVERS
!   THE ORIGINAL SERIES.  IERR IS NORMALLY ZERO.  IF IERR.EQ.1 THEN FFT
!   HAS NOT BEEN ABLE TO FACTOR THE SERIES.  HOWEVER, X HAS BEEN
!   SCRAMBLED BY REALTR.  NOTE THAT IF N IS ODD THE LAST POINT WILL NOT
!   BE USED IN THE TRANSFORM.
!
!                                                     -RPB
      subroutine fftl(x, n, ndir, ierr)
      integer*4 n, n2, n1, i
      dimension x(1)
      n2 = n / 2
      idir = iabs(ndir)
!   DO FOREWARD TRANSFORM (IE. TIME TO FREQUENCY).
      goto (1, 2), idir
    1 call fft(x, x(2), n2, n2, n2, 2, ierr)
      call realtr(x, x(2), n2, 2)
      n1 = (2 * n2) + 2
      scale = 1. / n

      if (ndir .gt. 0) goto 3

      do 5 i = 4, n, 2
    5     x(i) = - x(i)
!   DO BACKWARD TRANSFORM (IE. FREQUENCY TO TIME).
      goto 3

    2 if (ndir .gt. 0) goto 6

      do 7 i = 4, n, 2
    7     x(i) = - x(i)

    6 x(2) = 0.
      x((2 * n2) + 2) = 0.
      call realtr(x, x(2), n2, -2)
      call fft(x, x(2), n2, n2, n2, -2, ierr)
      n1 = 2 * n2
!   NORMALIZE THE TRANSFORM.
      scale = .5

    3 do 4 i = 1, n1
    4     x(i) = scale * x(i)

      return 
      end

!*****************************************************
!  MULTIVARIATE COMPLEX FOURIER TRANSFORM, COMPUTED IN PLACE
!    USING MIXED-RADIX FAST FOURIER TRANSFORM ALGORITHM.
!  BY R. C. SINGLETON, STANFORD RESEARCH INSTITUTE, SEPT. 1968
!  ARRAYS A AND B ORIGINALLY HOLD THE REAL AND IMAGINARY
!    COMPONENTS OF THE DATA, AND RETURN THE REAL AND
!    IMAGINARY COMPONENTS OF THE RESULTING FOURIER COEFFICIENTS.
!  MULTIVARIATE DATA IS INDEXED ACCORDING TO THE FORTRAN
!    ARRAY ELEMENT SUCCESSOR FUNCTION, WITHOUT LIMIT
!    ON THE NUMBER OF IMPLIED MULTIPLE SUBSCRIPTS.
!    THE SUBROUTINE IS CALLED ONCE FOR EACH VARIATE.
!    THE CALLS FOR A MULTIVARIATE TRANSFORM MAY BE IN ANY ORDER.
!  NTOT IS THE TOTAL NUMBER OF COMPLEX DATA VALUES.
!  N IS THE DIMENSION OF THE CURRENT VARIABLE.
!  NSPAN/N IS THE SPACING OF CONSECUTIVE DATA VALUES
!    WHILE INDEXING THE CURRENT VARIABLE.
!  THE INTEGER IERR IS AN ERROR RETURN INDICATOR. IT IS NORMALLY ZERO, B
!UT IS
!  SET TO 1 IF THE NUMBER OF TERMS CANNOT BE FACTORED IN THE SPACE AVAIL
!ABLE. IF
!  IT IS PERMISSIBLE THE APPROPRIATE ACTION AT THIS STAGE IS TO  ENTER F
!FT
!  AGAIN AFTER HAVING REDUCED THE LENGTH OF THE SERIES BY ONE TERM
!  THE SIGN OF ISN DETERMINES THE SIGN OF THE COMPLEX
!    EXPONENTIAL, AND THE MAGNITUDE OF ISN IS NORMALLY ONE.
!  A TRI-VARIATE TRANSFORM WITH A(N1,N2,N3), B(N1,N2,N3)
!    IS COMPUTED BY
!      CALL FFT(A,B,N1*N2*N3,N1,N1,1)
!      CALL FFT(A,B,N1*N2*N3,N2,N1*N2,1)
!      CALL FFT(A,B,N1*N2*N3,N3,N1*N2*N3,1)
!  FOR A SINGLE-VARIATE TRANSFORM,
!    NTOT = N = NSPAN = (NUMBER OF COMPLEX DATA VALUES), E.G.
!      CALL FFT(A,B,N,N,N,1)
!  WITH MOST FORTRAN COMPILERS THE DATA CAN ALTERNATIVELY BE
!    STORED IN A SINGLE COMPLEX ARRAY A, THEN THE MAGNITUDE OF ISN
!    CHANGED TO TWO TO GIVE THE CORRECT INDEXING INCREMENT AND A(2)
!    USED TO PASS THE INITIAL ADDRESS FOR THE SEQUENCE OF IMAGINARY
!    VALUES, E.G.
!      CALL FFT(A,A(2),NTOT,N,NSPAN,2)
!  ARRAYS AT(MAXF), CK(MAXF), BT(MAXF), SK(MAXF), AND NP(MAXP)
!    ARE USED FOR TEMPORARY STORAGE.  IF THE AVAILABLE STORAGE
!    IS INSUFFICIENT, THE PROGRAM IS TERMINATED BY THE ERROR RETURN OPTI
!ON
!    MAXF MUST BE .GE. THE MAXIMUM PRIME FACTOR OF N.
!    MAXP MUST BE .GT. THE NUMBER OF PRIME FACTORS OF N.
!    IN ADDITION, IF THE SQUARE-FREE PORTION K OF N HAS TWO OR
!    MORE PRIME FACTORS, THEN MAXP MUST BE .GE. K-1.
      subroutine fft(a, b, ntot, n, nspan, isn, ierr)
!  ARRAY STORAGE IN NFAC FOR A MAXIMUM OF 15 PRIME FACTORS OF N.
!  IF N HAS MORE THAN ONE SQUARE-FREE FACTOR, THE PRODUCT OF THE
!    SQUARE-FREE FACTORS MUST BE .LE. 210
      dimension a(1), b(1)
!  ARRAY STORAGE FOR MAXIMUM PRIME FACTOR OF 23
      dimension nfac(11), np(209)
      dimension at(23), ck(23), bt(23), sk(23)
!  THE FOLLOWING TWO CONSTANTS SHOULD AGREE WITH THE ARRAY DIMENSIONS.
      equivalence (ii, i)
      maxf = 23
      maxp = 209
      ierr = 0
      if (n .lt. 2) return 
      inc = isn
      c72 = 0.30901699437494742
      s72 = 0.95105651629515357
      s120 = 0.86602540378443865
      rad = 6.2831853071796
      if (isn .ge. 0) goto 10
      s72 = - s72
      s120 = - s120
      rad = - rad
      inc = - inc
   10 nt = inc * ntot
      ks = inc * nspan
      kspan = ks
      nn = nt - inc
      jc = ks / n
      radf = (rad * float(jc)) * 0.5
      i = 0
!  DETERMINE THE FACTORS OF N
      jf = 0
      m = 0
      k = n
      goto 20
   15 m = m + 1
      nfac(m) = 4
      k = k / 16
   20 if ((k - ((k / 16) * 16)) .eq. 0) goto 15
      j = 3
      jj = 9
      goto 30
   25 m = m + 1
      nfac(m) = j
      k = k / jj
   30 if (mod(k,jj) .eq. 0) goto 25
      j = j + 2
      jj = j ** 2
      if (jj .le. k) goto 30
      if (k .gt. 4) goto 40
      kt = m
      nfac(m + 1) = k
      if (k .ne. 1) m = m + 1
      goto 80
   40 if ((k - ((k / 4) * 4)) .ne. 0) goto 50
      m = m + 1
      nfac(m) = 2
      k = k / 4
   50 kt = m
      j = 2
   60 if (mod(k,j) .ne. 0) goto 70
      m = m + 1
      nfac(m) = j
      k = k / j
   70 j = (((j + 1) / 2) * 2) + 1
      if (j .le. k) goto 60
   80 if (kt .eq. 0) goto 100
      j = kt
   90 m = m + 1
      nfac(m) = nfac(j)
      j = j - 1
!  COMPUTE FOURIER TRANSFORM
      if (j .ne. 0) goto 90
  100 sd = radf / float(kspan)
      cd = 2.0 * (sin(sd) ** 2)
      sd = sin(sd + sd)
      kk = 1
      i = i + 1
!  TRANSFORM FOR FACTOR OF 2 (INCLUDING ROTATION FACTOR)
      if (nfac(i) .ne. 2) goto 400
      kspan = kspan / 2
      k1 = kspan + 2
  210 k2 = kk + kspan
      ak = a(k2)
      bk = b(k2)
      a(k2) = a(kk) - ak
      b(k2) = b(kk) - bk
      a(kk) = a(kk) + ak
      b(kk) = b(kk) + bk
      kk = k2 + kspan
      if (kk .le. nn) goto 210
      kk = kk - nn
      if (kk .le. jc) goto 210
      if (kk .gt. kspan) goto 800
  220 c1 = 1.0 - cd
      s1 = sd
  230 k2 = kk + kspan
      ak = a(kk) - a(k2)
      bk = b(kk) - b(k2)
      a(kk) = a(kk) + a(k2)
      b(kk) = b(kk) + b(k2)
      a(k2) = (c1 * ak) - (s1 * bk)
      b(k2) = (s1 * ak) + (c1 * bk)
      kk = k2 + kspan
      if (kk .lt. nt) goto 230
      k2 = kk - nt
      c1 = - c1
      kk = k1 - k2
      if (kk .gt. k2) goto 230
      ak = (cd * c1) + (sd * s1)
      s1 = ((sd * c1) - (cd * s1)) + s1
      c1 = c1 - ak
      kk = kk + jc
      if (kk .lt. k2) goto 230
      k1 = (k1 + inc) + inc
      kk = ((k1 - kspan) / 2) + jc
      if (kk .le. (jc + jc)) goto 220
!  TRANSFORM FOR FACTOR OF 3 (OPTIONAL CODE)
      goto 100
  320 k1 = kk + kspan
      k2 = k1 + kspan
      ak = a(kk)
      bk = b(kk)
      aj = a(k1) + a(k2)
      bj = b(k1) + b(k2)
      a(kk) = ak + aj
      b(kk) = bk + bj
      ak = (- (0.5 * aj)) + ak
      bk = (- (0.5 * bj)) + bk
      aj = (a(k1) - a(k2)) * s120
      bj = (b(k1) - b(k2)) * s120
      a(k1) = ak - bj
      b(k1) = bk + aj
      a(k2) = ak + bj
      b(k2) = bk - aj
      kk = k2 + kspan
      if (kk .lt. nn) goto 320
      kk = kk - nn
      if (kk .le. kspan) goto 320
!  TRANSFORM FOR FACTOR OF 4
      goto 700
  400 if (nfac(i) .ne. 4) goto 600
      kspnn = kspan
      kspan = kspan / 4
  410 c1 = 1.0
      s1 = 0
  420 k1 = kk + kspan
      k2 = k1 + kspan
      k3 = k2 + kspan
      akp = a(kk) + a(k2)
      akm = a(kk) - a(k2)
      ajp = a(k1) + a(k3)
      ajm = a(k1) - a(k3)
      a(kk) = akp + ajp
      ajp = akp - ajp
      bkp = b(kk) + b(k2)
      bkm = b(kk) - b(k2)
      bjp = b(k1) + b(k3)
      bjm = b(k1) - b(k3)
      b(kk) = bkp + bjp
      bjp = bkp - bjp
      if (isn .lt. 0) goto 450
      akp = akm - bjm
      akm = akm + bjm
      bkp = bkm + ajm
      bkm = bkm - ajm
      if (s1 .eq. 0) goto 460
  430 a(k1) = (akp * c1) - (bkp * s1)
      b(k1) = (akp * s1) + (bkp * c1)
      a(k2) = (ajp * c2) - (bjp * s2)
      b(k2) = (ajp * s2) + (bjp * c2)
      a(k3) = (akm * c3) - (bkm * s3)
      b(k3) = (akm * s3) + (bkm * c3)
      kk = k3 + kspan
      if (kk .le. nt) goto 420
  440 c2 = (cd * c1) + (sd * s1)
      s1 = ((sd * c1) - (cd * s1)) + s1
      c1 = c1 - c2
      c2 = (c1 ** 2) - (s1 ** 2)
      s2 = (2.0 * c1) * s1
      c3 = (c2 * c1) - (s2 * s1)
      s3 = (c2 * s1) + (s2 * c1)
      kk = (kk - nt) + jc
      if (kk .le. kspan) goto 420
      kk = (kk - kspan) + inc
      if (kk .le. jc) goto 410
      if (kspan .eq. jc) goto 800
      goto 100
  450 akp = akm + bjm
      akm = akm - bjm
      bkp = bkm - ajm
      bkm = bkm + ajm
      if (s1 .ne. 0) goto 430
  460 a(k1) = akp
      b(k1) = bkp
      a(k2) = ajp
      b(k2) = bjp
      a(k3) = akm
      b(k3) = bkm
      kk = k3 + kspan
      if (kk .le. nt) goto 420
!  TRANSFORM FOR FACTOR OF 5 (OPTIONAL CODE)
      goto 440
  510 c2 = (c72 ** 2) - (s72 ** 2)
      s2 = (2.0 * c72) * s72
  520 k1 = kk + kspan
      k2 = k1 + kspan
      k3 = k2 + kspan
      k4 = k3 + kspan
      akp = a(k1) + a(k4)
      akm = a(k1) - a(k4)
      bkp = b(k1) + b(k4)
      bkm = b(k1) - b(k4)
      ajp = a(k2) + a(k3)
      ajm = a(k2) - a(k3)
      bjp = b(k2) + b(k3)
      bjm = b(k2) - b(k3)
      aa = a(kk)
      bb = b(kk)
      a(kk) = (aa + akp) + ajp
      b(kk) = (bb + bkp) + bjp
      ak = ((akp * c72) + (ajp * c2)) + aa
      bk = ((bkp * c72) + (bjp * c2)) + bb
      aj = (akm * s72) + (ajm * s2)
      bj = (bkm * s72) + (bjm * s2)
      a(k1) = ak - bj
      a(k4) = ak + bj
      b(k1) = bk + aj
      b(k4) = bk - aj
      ak = ((akp * c2) + (ajp * c72)) + aa
      bk = ((bkp * c2) + (bjp * c72)) + bb
      aj = (akm * s2) - (ajm * s72)
      bj = (bkm * s2) - (bjm * s72)
      a(k2) = ak - bj
      a(k3) = ak + bj
      b(k2) = bk + aj
      b(k3) = bk - aj
      kk = k4 + kspan
      if (kk .lt. nn) goto 520
      kk = kk - nn
      if (kk .le. kspan) goto 520
!  TRANSFORM FOR ODD FACTORS
      goto 700
  600 k = nfac(i)
      kspnn = kspan
      kspan = kspan / k
      if (k .eq. 3) goto 320
      if (k .eq. 5) goto 510
      if (k .eq. jf) goto 640
      jf = k
      s1 = rad / float(k)
      c1 = cos(s1)
      s1 = sin(s1)
      if (jf .gt. maxf) goto 998
      ck(jf) = 1.0
      sk(jf) = 0.0
      j = 1
  630 ck(j) = (ck(k) * c1) + (sk(k) * s1)
      sk(j) = (ck(k) * s1) - (sk(k) * c1)
      k = k - 1
      ck(k) = ck(j)
      sk(k) = - sk(j)
      j = j + 1
      if (j .lt. k) goto 630
  640 k1 = kk
      k2 = kk + kspnn
      aa = a(kk)
      bb = b(kk)
      ak = aa
      bk = bb
      j = 1
      k1 = k1 + kspan
  650 k2 = k2 - kspan
      j = j + 1
      at(j) = a(k1) + a(k2)
      ak = at(j) + ak
      bt(j) = b(k1) + b(k2)
      bk = bt(j) + bk
      j = j + 1
      at(j) = a(k1) - a(k2)
      bt(j) = b(k1) - b(k2)
      k1 = k1 + kspan
      if (k1 .lt. k2) goto 650
      a(kk) = ak
      b(kk) = bk
      k1 = kk
      k2 = kk + kspnn
      j = 1
  660 k1 = k1 + kspan
      k2 = k2 - kspan
      jj = j
      ak = aa
      bk = bb
      aj = 0.0
      bj = 0.0
      k = 1
  670 k = k + 1
      ak = (at(k) * ck(jj)) + ak
      bk = (bt(k) * ck(jj)) + bk
      k = k + 1
      aj = (at(k) * sk(jj)) + aj
      bj = (bt(k) * sk(jj)) + bj
      jj = jj + j
      if (jj .gt. jf) jj = jj - jf
      if (k .lt. jf) goto 670
      k = jf - j
      a(k1) = ak - bj
      b(k1) = bk + aj
      a(k2) = ak + bj
      b(k2) = bk - aj
      j = j + 1
      if (j .lt. k) goto 660
      kk = kk + kspnn
      if (kk .le. nn) goto 640
      kk = kk - nn
!  MULTIPLY BY ROTATION FACTOR (EXCEPT FOR FACTORS OF 2 AND 4)
      if (kk .le. kspan) goto 640
  700 if (i .eq. m) goto 800
      kk = jc + 1
  710 c2 = 1.0 - cd
      s1 = sd
  720 c1 = c2
      s2 = s1
      kk = kk + kspan
  730 ak = a(kk)
      a(kk) = (c2 * ak) - (s2 * b(kk))
      b(kk) = (s2 * ak) + (c2 * b(kk))
      kk = kk + kspnn
      if (kk .le. nt) goto 730
      ak = s1 * s2
      s2 = (s1 * c2) + (c1 * s2)
      c2 = (c1 * c2) - ak
      kk = (kk - nt) + kspan
      if (kk .le. kspnn) goto 730
      c2 = c1 - ((cd * c1) + (sd * s1))
      s1 = s1 + ((sd * c1) - (cd * s1))
      kk = (kk - kspnn) + jc
      if (kk .le. kspan) goto 720
      kk = ((kk - kspan) + jc) + inc
      if (kk .le. (jc + jc)) goto 710
!  PERMUTE THE RESULTS TO NORMAL ORDER---DONE IN TWO STAGES
!  PERMUTATION FOR SQUARE FACTORS OF N
      goto 100
  800 np(1) = ks
      if (kt .eq. 0) goto 890
      k = (kt + kt) + 1
      if (m .lt. k) k = k - 1
      j = 1
      np(k + 1) = jc
  810 np(j + 1) = np(j) / nfac(j)
      np(k) = np(k + 1) * nfac(j)
      j = j + 1
      k = k - 1
      if (j .lt. k) goto 810
      k3 = np(k + 1)
      kspan = np(2)
      kk = jc + 1
      k2 = kspan + 1
      j = 1
!  PERMUTATION FOR SINGLE-VARIATE TRANSFORM (OPTIONAL CODE)
      if (n .ne. ntot) goto 850
  820 ak = a(kk)
      a(kk) = a(k2)
      a(k2) = ak
      bk = b(kk)
      b(kk) = b(k2)
      b(k2) = bk
      kk = kk + inc
      k2 = kspan + k2
      if (k2 .lt. ks) goto 820
  830 k2 = k2 - np(j)
      j = j + 1
      k2 = np(j + 1) + k2
      if (k2 .gt. np(j)) goto 830
      j = 1
  840 if (kk .lt. k2) goto 820
      kk = kk + inc
      k2 = kspan + k2
      if (k2 .lt. ks) goto 840
      if (kk .lt. ks) goto 830
      jc = k3
!  PERMUTATION FOR MULTIVARIATE TRANSFORM
      goto 890
  850 k = kk + jc
  860 ak = a(kk)
      a(kk) = a(k2)
      a(k2) = ak
      bk = b(kk)
      b(kk) = b(k2)
      b(k2) = bk
      kk = kk + inc
      k2 = k2 + inc
      if (kk .lt. k) goto 860
      kk = (kk + ks) - jc
      k2 = (k2 + ks) - jc
      if (kk .lt. nt) goto 850
      k2 = (k2 - nt) + kspan
      kk = (kk - nt) + jc
      if (k2 .lt. ks) goto 850
  870 k2 = k2 - np(j)
      j = j + 1
      k2 = np(j + 1) + k2
      if (k2 .gt. np(j)) goto 870
      j = 1
  880 if (kk .lt. k2) goto 850
      kk = kk + jc
      k2 = kspan + k2
      if (k2 .lt. ks) goto 880
      if (kk .lt. ks) goto 870
      jc = k3
  890 if (((2 * kt) + 1) .ge. m) return 
!  PERMUTATION FOR SQUARE-FREE FACTORS OF N
      kspnn = np(kt + 1)
      j = m - kt
      nfac(j + 1) = 1
  900 nfac(j) = nfac(j) * nfac(j + 1)
      j = j - 1
      if (j .ne. kt) goto 900
      kt = kt + 1
      nn = nfac(kt) - 1
      if (nn .gt. maxp) goto 998
      jj = 0
      j = 0
      goto 906
  902 jj = jj - k2
      k2 = kk
      k = k + 1
      kk = nfac(k)
  904 jj = kk + jj
      if (jj .ge. k2) goto 902
      np(j) = jj
  906 k2 = nfac(kt)
      k = kt + 1
      kk = nfac(k)
      j = j + 1
!  DETERMINE THE PERMUTATION CYCLES OF LENGTH GREATER THAN 1
      if (j .le. nn) goto 904
      j = 0
      goto 914
  910 k = kk
      kk = np(k)
      np(k) = - kk
      if (kk .ne. j) goto 910
      k3 = kk
  914 j = j + 1
      kk = np(j)
      if (kk .lt. 0) goto 914
      if (kk .ne. j) goto 910
      np(j) = - j
      if (j .ne. nn) goto 914
!  REORDER A AND B, FOLLOWING THE PERMUTATION CYCLES
      maxf = inc * maxf
      goto 950
  924 j = j - 1
      if (np(j) .lt. 0) goto 924
      jj = jc
  926 kspan = jj
      if (jj .gt. maxf) kspan = maxf
      jj = jj - kspan
      k = np(j)
      kk = ((jc * k) + ii) + jj
      k1 = kk + kspan
      k2 = 0
  928 k2 = k2 + 1
      at(k2) = a(k1)
      bt(k2) = b(k1)
      k1 = k1 - inc
      if (k1 .ne. kk) goto 928
  932 k1 = kk + kspan
      k2 = k1 - (jc * (k + np(k)))
      k = - np(k)
  936 a(k1) = a(k2)
      b(k1) = b(k2)
      k1 = k1 - inc
      k2 = k2 - inc
      if (k1 .ne. kk) goto 936
      kk = k2
      if (k .ne. j) goto 932
      k1 = kk + kspan
      k2 = 0
  940 k2 = k2 + 1
      a(k1) = at(k2)
      b(k1) = bt(k2)
      k1 = k1 - inc
      if (k1 .ne. kk) goto 940
      if (jj .ne. 0) goto 926
      if (j .ne. 1) goto 924
  950 j = k3 + 1
      nt = nt - kspnn
      ii = (nt - inc) + 1
      if (nt .ge. 0) goto 924
!  ERROR FINISH, INSUFFICIENT ARRAY STORAGE
      return 
  998 ierr = 1
  999 end

!*****************************************************
!
!
! TITLE - REALTR = REAL TRANSFORM
!     FOURIER TRANSFORM OF REAL SERIES FROM OUTPUT OF FFT
!
!              IF ISN=1, THIS SUBROUTINE COMPLETES THE FOURIER TRANS-
!              FORM OF 2*N REAL DATA VALUES, WHERE THE ORIGINAL DATA
!              VALUES ARE STORED ALTERNATELY IN ARRAYS A AND B, AND ARE
!              FIRST TRANSFORMED BY A COMPLEX FOURIER TRANSFORM OF
!              DIMENSION N.
!              THE COSINE COEFFICIENTS ARE IN A(1),A(2),...A(N+1) AND
!              THE SINE COEFFICIENTS ARE IN B(1),B(2),...B(N+1).
!              A TYPICAL CALLING SEQUENCE IS
!              CALL FFT (A,B,N,-1)
!              CALL REALTR (A,B,N,1)
!              THE RESULTS SHOULD BE MULTIPLIED BY 0.5/N TO GIVE THE
!              USUAL SCALING OF COEFFICIENTS.
!              IF ISN=1, THE INVERSE TRANSFORMATION IS DONE, THE
!              FIRST STEP IN EVALUATING A REAL FOURIER SERIES.
!              A TYPICAL CALLING SEQUENCE IS
!              CALL REALTR (A,B,N,-1)
!              CALL FFT (A,B,N,-1)
!              THE RESULTS SHOULD BE MULTIPLIED BY 0.5 TO GIVE THE
!              USUAL SCALING, AND THE TIME DOMAIN RESULTS ALTERNATE
!              IN ARRAYS A AND B, I.E. A(1),B(1), A(2),B(2),
!              ...A(N),B(N).
!              THE DATA MAY ALTERNATIVELY BE STORED IN A SINGLE
!              COMPLEX ARRAY A, THEN THE MAGNITUDE OF ISN CHANGED TO
!              TWO TO GIVE THE CORRECT INDEXING INCREMENT AND A(2)
!              USED TO PASS THE INITIAL ADDRESS FOR THE SEQUENCE OF
!              IMAGINARY VALUES,E.G.
!              CALL FFT(A,A(2),N,2)
!              CALL REALTR(A,A(2),N,2)
!              IN THIS CASE, THE COSINE AND SINE COEFFICIENTS
!              ALTERNATE IN A.
!
! INPUTS
!
!     A(I)            I=1, IABS(ISN)*N, IABS(ISN) CONTAINS THE REAL
!              PART OF THE FOURIER TRANSFORM OF A REAL DATA SERIES
!              TREATED AS COMPLEX.
!     B(I)            CONTAINS THE IMAGINARY PART CORRESPONDING TO A
!     N               THE NUMBER OF COMPLEX NUMBERS REPRESENTED IN
!              A AND B
!     ISN             IS NEGATIVE FOR THE INVERSE TRANSFORM AND
!              POSITIVE FOR THE TRANSFORM IABS(ISN) IS THE ADVANCE
!              FOR INDEXING THROUGH A OR B.
!
!
! OUTPUTS
!
!     A(1)     THE REAL PART OF THE FOURIER TRANSFORM OF A REAL DATA
!              SERIES
!     B(1)     THE IMAGINARY PART OF THE FOURIER TRANSFORM OF A
!              REAL DATA SERIES
!     F        = 0 IF NO ERRORS
!              = 1 IF N .LE. 0
!
!
! EXAMPLES
!
!  SUPPOSE  X  IS A REAL ARRAY WITH  N  ELEMENTS (N=2**L), AND
!  WE WISH THE DIGITAL FOURIER COSINE AND SINE TRANSFORMS OF IT;
!  THEN THESE CALLS ARE REQUIRED (USING PDP-11 ROUTINES)
!      CALL FFTTWO(X,N/2)
!      CALL REALTR(X,X(2),N/2,2)
!  NOW  X  CONTAINS THE COS AND SIN TRANSFORMS (UNNORMALIZED) ,
!  STARTING AT ZERO FREQUENCY AND ALTERNATING COS,SIN,COS,SIN,....
!  NOTE THAT  N  HAS BEEN HALVED IN THE CALLS TO FFTTWO  AND  REALTR
!
! ***** WARNING - THE TRANSFORMS OF THE NYQUIST FREQUENCY 
!      APPEAR IN THE POSITIONS  X(N+1),X(N+2)  SO THAT
!      THE ARRAY  X  MUST BE DIMENSIONED AT LEAST  X(N+2) IN
!      THE CALLING  PROGRAM   ***************
!
!
      subroutine realtr(a, b, n, isn)
      dimension a(1), b(1)
      real im
      if (n .le. 1) return 
      inc = iabs(isn)
      nk = (n * inc) + 2
      nh = nk / 2
      sd = (2. * atan(1.)) / float(n)
      cd = 2. * (sin(sd) ** 2)
      sd = sin(sd + sd)
      sn = 0.

      if (isn .gt. 0) goto 10
      cn = -1.
      sd = - sd
      goto 20

   10 cn = 1.
      a(nk - 1) = a(1)
      b(nk - 1) = b(1)

   20 do 30 j = 1, nh, inc
          k = nk - j
          aa = a(j) + a(k)
          ab = a(j) - a(k)
          ba = b(j) + b(k)
          bb = b(j) - b(k)
          re = (cn * ba) + (sn * ab)
          im = (sn * ba) - (cn * ab)
          b(k) = im - bb
          b(j) = im + bb
          a(k) = aa - re
          a(j) = aa + re
          aa = cn - ((cd * cn) + (sd * sn))
          sn = ((sd * cn) - (cd * sn)) + sn
   30     cn = aa

      return 

      end

!*****************************************************
      subroutine filt(a, lenf, df, f0, f1, f2, f3, f4, itype)
      complex a(1)
      if (itype .eq. 0) then
          write(unit=*, fmt=*) 'option not active, pgm aborted'
!
!       subroutine filt(a,lenf,df,f0,f1,f2,f3,f4,itype)
!
!
!       ************   BOXCAR FILTER   ********************
!
! fmin and fmax are the min and max frequencies within the boxcar
! filter limits.  Data (a(i)) are zeroed outside of those limits.
! 
!
!
!         CALL ZERO(A,1,INT((FMIN-F0)/DF+1.5))
!         CALL ZERO(A,INT((FMAX-F0)/DF+1.5),LENF)
!         WRITE(6,*)'BOX CAR FILTER'
      stop
!***    COSINE TAPER WITH LOW FREQUENCY TRUNCATION
      else if (itype .eq. 1) then
          if1 = ((f1 - f0) / df) + 1.5
          if2 = ((f2 - f0) / df) + 1.5
          if3 = ((f3 - f0) / df) + 1.5
          if4 = ((f4 - f0) / df) + 1.5
          call ftaper(a(if1), (if2 - if1) + 1, -1)
          call ftaper(a(if3), (if4 - if3) + 1, 1)
          call zero(a, 1, if1 - 1)
!         WRITE(6,*)'COSINE TAPER'
          call zero(a, if4 + 1, lenf)
!***    TRAPAZOID FILTER
      else if (itype .eq. 2) then
          call trpfil(a, lenf, df, f0, f1, f2, f3, f4)
          write(unit=6, fmt=*) 'TRAPAZOID FILTER'
      else
          write(unit=*, fmt='('' INVALID VALUE OF ITYPE,PGM ABORTED'')') 
          stop
      end if
      return 
      end

!*****************************************************
      subroutine zero(a, len1, len)
      complex a(1)
      do 10 i = len1, len
   10     a(i) = (0.,0.)
      return 
      end

!*****************************************************
      subroutine trpfil(a, lenf, df, f0, f1, f2, f3, f4)
      complex a(1)
      i1 = ((f1 - f0) / df) + 1.5
      i2 = ((f2 - f0) / df) + 1.5
      i3 = ((f3 - f0) / df) + 1.5
      i4 = ((f4 - f0) / df) + 1.5
      write(unit=6, fmt=100) i1, i2, i3, i4
  100 format(4i10)
      f21 = float(i2 - i1)
      f43 = float(i4 - i3)
      if (f21 .gt. 0.) then
      slope1 = 1. / f21
      do 10 i = i1, i2
      fil = float(i - i1) / f21
   10 a(i) = a(i) * fil
      end if
      if (f43 .gt. 0.) then
          slope2 = 1. / f43
          do 20 i = i3, i4
              fil = 1. - (float(i - i3) / f43)
   20         a(i) = a(i) * fil
      end if
      call zero(a, 1, i1)
      call zero(a, i4, lenf)
      return 
      end

!*****************************************************
!
!   routine FTAPER applies a cosine taper to the frequency domain values
! A.
!   The taper decays or grows as ISW is +1 or -1.
!
      subroutine ftaper(a, len, isw)
      complex a(1)
      data pi / 3.141592654 /
      if (len .eq. 1) return 
      arg = pi / float(len - 1)
      if (isw .eq. 1) then
          do 10 i = 1, len
              a(i) = (.5 * (1. + cos(arg * float(i - 1)))) * a(i)
   10     continue
      else if (isw .eq. (-1)) then
          do 20 i = 1, len
              a(i) = (.5 * (1. - cos(arg * float(i - 1)))) * a(i)
   20     continue
      else
          write(unit=*, 
     &          fmt='('' INCORRECT ISW IN FTAPER:PGM ABORTED'')') 
          stop
      end if
      return 
      end


!*****************************************************
!     This subroutine computes the source time function at intervals of
!     dt_fcn, from Fourier coefficients, stored in common source_com.
!     Coeficients are assumed to be stored cos sin cos sin with the
!     zeroth order coef in the last slot.  there are 2*ncoef+1
!     coeficients m is the number of points in the resulting time series
!     (tslen/dt_fcn.  Relevent numbers are stored in common block
!     source_com.  parameters are the integrated amplitude am0, the
!     centroid with respect to the beginning of the interval ((check
!     this)) am1, and the variance am2.  These numbers are calculated
!     using the fourier coeficients.  isyr,isday... gives the absolute
!     start time of the seismogram used to determine the source time
!     function.

      subroutine get_fcn(dt_fcn, d, m)
!       update obshead has been changed so that it only explicitly
!       considers the first three blocks.  The rest must be
!       accessed using the other commons such as gdsn.cmn etc.
!       previously, this common explicitly included the information
!       for the broad-band data.
!       hedtyp (='DATA' or 'HEAD') added to DATACMN JFS 2 Nov 1986
!
!       has been modified by pgs 5 1 88 to accept mulitblock headers
!       has been modified to include the real*4 variable 'orient'
!       that gives the true azimuth in degrees of the north-south
!       component. replaces dspace in datacm. Thus, if the n-s component
!       is really at an azimuth of -5 degrees, orient is -5.
        dimension       head1(10),head2(20),head3(20),head4(20),
     1                  head5(20),head6(20),head7(18)

        character*4     com
        common /titlcm/ com(10)

        integer*4       iyr,iday,ihr,imin,npts,isynth
        character*4     dnet,stn,chan,file,form,hedtyp
        common /datacm/ iyr,iday,ihr,imin,sec,dt,npts,dnet,stn,chan,
     1                  file(3),form,scale,timscl,
     1                  isynth,offset,hedtyp,orient

        integer*4       ieyr,ieday,iehr,iemin,numstn
        common /evntcm/ ieyr,ieday,iehr,iemin,esec,elat,elon,edep,
     1                  emb,ems,numstn,
     1                  edist,eaz,ebaz,foc(6)

        equivalence     (head1,com),
     2                  (head2,iyr),
     3                  (head3,ieyr)
        common/netblk/head4,head5,head6,head7

c       the rest is for filling up the second header block
c       modified pgs 5 2 88
        dimension head8(128)
        common/netblk2/head8
      common /source_com/ ssstn, isyr, isday, ishr, ismin, ssec, pshift
     &, am0, am1, am2, tslen, ncoef, xcoef(500)
      common /source_file/ source_name, isunit
      character ssstn*4, source_name*80
      common /source_pick/ lpickfile, pickfile, pickphase, pickshift
      character pickphase*4
!       data lpickfile/.false./
!       data pickshift/0.0/
!       data isunit/17/,ipunit/20/
!***    assumed that n+1 frequencies are used, including
!***    zero frequency.  in all 2n+1 parameters are
!***    used. (zero freq coef is in 2n+1st position).   
      character pickfile*80
      common /flnam/ istr, jstr
!
!   next 3 lines are to suppress written output if required
      character istr*80, jstr*80
      common /cwrite/ quiet
      logical quiet
      dimension d(*)
      logical first_time
      save first_time
! !displacement pulse, 1 would be velocity pulse
!       get coeficients from file or array
!         coefs supplied from program, skip io.
      data pi / 3.141592654 /
      data first_time / .true. /
      data iunit / 10 /
      data iprime / 0 /
      if ((source_name(1:3) .eq. 'pgm') .or. (source_name(1:3) .eq. 
     &'PGM')) then
          write(unit=*, fmt=*) 'coeficients supplied from program'
          goto 2
      end if
      if (first_time .eq. .true.) then
          if (source_name(1:1) .eq. ' ') then
              write(unit=*, fmt=*) 'For source fcn file,'
              ! sfiles does not exist on our machines, and I don't know
              ! what library it is a part of.  Commenting out the call
              ! so we can compile, but this will break get_fcn.
              !call sfiles(1, isunit)
              source_name = istr
              first_time = .false.
          else
              istr = source_name
              !call sfiles(-1, isunit)
              first_time = .false.
          end if
      else
          rewind(unit=isunit) 
!       search file for station and name of event
!       read record
      end if

    1 read(unit=isunit, fmt=100, end=99) ssstn, isyr, isday, ishr, ismin
     &, ssec, pshift, am0, am1, am2, tslen, ncoef, (xcoef(j),j = 1, (2
     & * ncoef) + 1)
      if (((ssstn .eq. stn) .and. (isyr .eq. iyr)) .and. (isday .eq. 
     &iday)) then
          if (.not. quiet) then
              write(unit=*, fmt=*) 'event and station found', ssstn,
     &        isyr, isday
          end if
      else
          goto 1
      end if

    2 continue
      if (.not. quiet) then
          write(unit=*, fmt=110) ssstn, isyr, isday, ishr, ismin, ssec, 
     &pshift, am0, am1, am2, tslen, ncoef, (xcoef(j),j = 1, (2 * ncoef)
     & + 1)
      end if

      ncoef2 = 2 * ncoef
      arg = (2. * pi) / tslen
      m = (tslen / dt_fcn) + 1.

!     zero time is in the center of the interval
      t = - (tslen / 2.)
      do i = 1, m
          d(i) = 0.
          arg1 = arg * t
          iarg = 0
          do j = 1, ncoef2, 2
              iarg = iarg + 1
              arg2 = arg1 * float(iarg)
              if (iprime .eq. 0) then
                  dtemp = (xcoef(j) * cos(arg2)) + (xcoef(j + 1) * 
     &                    sin(arg2))
              else if (iprime .eq. 1) then
                  dtemp = (arg2 / t) * ((xcoef(j + 1) * cos(arg2)) - 
     &                    (xcoef(j) * sin(arg2)))
              else
                  write(unit=6, fmt=*) 'invalid iprime.pgm aborted'
                  stop
              end if
              d(i) = d(i) + dtemp
          end do
          if (iprime .eq. 0) d(i) = d(i) + xcoef(ncoef2 + 1)
          t = t + dt_fcn
      end do

      if (am0 .lt. 0.) then
          call scalar(d, m, -1.)
      end if
      if (lpickfile) then
          if (pickfile(1:1) .eq. ' ') then
              write(unit=*, fmt=*) ' Name of pick file [hit return for',
     &                             ' none] :'
              read(unit=5, fmt='(a)') pickfile
              nch=lenstr(pickfile)
              if (nch .eq. 0) then
                  pickshift = 0.0
                  return 
              end if
          end if
          call get_pickshift
      end if

      return 
   99 write(unit=*, fmt=*) 
     &'unable to find event and station, pgm aborted'
      stop
  100 format(1x,a4,4i4,f6.2,f8.2/9x,3e13.3/6x,e13.3/6x,i4//2e13.3)
      ncoef2 = 2 * ncoef
  110 format(1x,a4,4i4,f6.2,f8.2/9h moments:,3e13.3/6h tlen:,e13.3/
     &6h nmax:,i4/28h    cos           sin       /(2e13.3))
      end
!

!*****************************************************
      subroutine get_pickshift()
! the following has been changed
!       include 'source_com.cmn'
!   (see below)
!       update obshead has been changed so that it only explicitly
!       considers the first three blocks.  The rest must be
!       accessed using the other commons such as gdsn.cmn etc.
!       previously, this common explicitly included the information
!       for the broad-band data.
!       hedtyp (='DATA' or 'HEAD') added to DATACMN JFS 2 Nov 1986
!
!       has been modified by pgs 5 1 88 to accept mulitblock headers
!       has been modified to include the real*4 variable 'orient'
!       that gives the true azimuth in degrees of the north-south
!       component. replaces dspace in datacm. Thus, if the n-s component
!       is really at an azimuth of -5 degrees, orient is -5.
        dimension       head1(10),head2(20),head3(20),head4(20),
     1                  head5(20),head6(20),head7(18)

        character*4     com
        common /titlcm/ com(10)

        integer*4       iyr,iday,ihr,imin,npts,isynth
        character*4     dnet,stn,chan,file,form,hedtyp
        common /datacm/ iyr,iday,ihr,imin,sec,dt,npts,dnet,stn,chan,
     1                  file(3),form,scale,timscl,
     1                  isynth,offset,hedtyp,orient

        integer*4       ieyr,ieday,iehr,iemin,numstn
        common /evntcm/ ieyr,ieday,iehr,iemin,esec,elat,elon,edep,
     1                  emb,ems,numstn,
     1                  edist,eaz,ebaz,foc(6)

        equivalence     (head1,com),
     2                  (head2,iyr),
     3                  (head3,ieyr)
        common/netblk/head4,head5,head6,head7

c       the rest is for filling up the second header block
c       modified pgs 5 2 88
        dimension head8(128)
        common/netblk2/head8
      common /source_com/ ssstn, isyr, isday, ishr, ismin, ssec, pshift
     &, am0, am1, am2, tslen, ncoef, xcoef(500)
      common /source_file/ source_name, isunit
      character ssstn*4, source_name*80
      common /source_pick/ lpickfile, pickfile, pickphase, pickshift
      character pickphase*4
!       data lpickfile/.false./
!       data pickshift/0.0/
!       data isunit/17/,ipunit/20/
!
      character pickfile*80
!
!       search file for station and name of event
!       read header assumed to be three lines
      character event*6, line*100, sphase*7
      if (not(lpickfile)) then
          write(unit=6, fmt=*) ' No pick file '
          pickshift = 0.0
          return 
!
      end if
      open(unit=ipunit, file=pickfile, status='old', err=999) 
      goto 1000
  999 write(unit=6, fmt=*) ' Error opening pick file ', pickfile
      write(unit=6, fmt=*) ' Pick shift has been set to zero'
      pickshift = 0.0
!
      return 
 1000 continue
      do i = 1, 2
          read(unit=ipunit, fmt='(a)') line
          write(unit=*, fmt='(a)') line
!         read record
      end do

      write(unit=6, fmt=*) ' Event parameters from header are :-'
      write(unit=6, fmt=*) ' STN, YEAR, DAY = ', stn, iyr, iday
    1 read(unit=ipunit, fmt=100, end=99) tdif, x_pick1, deltat, loc1, 
     &loc2, event, ssstn, sphase, isyr, isday, ishr, ismin, ssec, sdt
      write(unit=6, fmt=*) ' stn,yr,day = ', ssstn, isyr, isday
      if (((ssstn .eq. stn) .and. (isyr .eq. iyr)) .and. (isday .eq. 
     &iday)) then
          write(unit=*, fmt=*) 'event and station found', ssstn, isyr,
     &                         isday
      else
          goto 1
      end if

      pickshift = x_pick1 - (float(loc1 - 1) * dt)
      write(unit=6, fmt=*) ' Pick shift returned is ', pickshift
      close(unit=ipunit) 
      return 

   99 write(unit=*, fmt=*) 
     &'stn or event not found - pick shift set to zero'
!100     format(1x,2f8.2,f6.2,2i7,1x,a6,1x,a4,1x,a7,
      pickshift = 0.0
  100 format(2f8.2,f6.2,2i7,1x,a6,1x,a4,1x,a7,i5,i4,2i3,2e10.3)
      close(unit=ipunit) 
      return 
      end

!*****************************************************
      subroutine hamwin(x, n)
      dimension x(1)
      tupi = 2. * 3.141592654
      win = tupi / float(n - 1)
      do i = 1, n
          w1 = i - 1
          sg = sign(0.5,x(i))
          x(i) = (x(i) * (0.08 + (0.46 * (1. - cos(w1 * win))))) + sg
      end do
      return 
      end

!*****************************************************
!***    linearly interpolates an unequally spaced time series
!       to produce an equally spaced one with spacing dx
!       n is # pts in original series,npts is the number of
!       equally spaced time points, and nmax is the maximum
!       allowable.  if npts>nmax,npts is set to nmax and a warning
!       is issued. Note: npts is set by the subroutine, not by you.
!       Output is in ybuf. Paul Silver 2/13/85
      subroutine linpol(x, y, n, ybuf, dx, nmax, npts)
!       xlen=x(n)-x(1)
!       npts=xlen/dx +1.5
!       write(6,*)xlen,npts
!       if(npts.gt.nmax)then
!         write(6,*)'# pts too large, pgm aborted'
!         stop
!       endif
      dimension x(1), y(1), ybuf(1)
      x1 = x(1)
      x2 = x(2)
      xnow = x1
      y1 = y(1)
      y2 = y(2)
      indx = 2
!***    fill ybuf with linearly interpolated time series
      ifl = 0
      i = 0
      do while (indx .ge. 0)
    1     if (xnow .gt. x2) then
              indx = indx + 1
              if (indx .gt. n) goto 2
              y1 = y2
              x1 = x2
              y2 = y(indx)
              x2 = x(indx)
              ifl = 0
              goto 1
          end if
          if (ifl .eq. 0) then
              slope = (y2 - y1) / (x2 - x1)
              ifl = 1
          end if
          i = i + 1
          if (i .gt. nmax) then
              write(unit=6, fmt=*) 
     &'warning: npts is greater than maximum.only max is given'
              i = i - 1
              goto 2
          end if
          ybuf(i) = y1 + (slope * (xnow - x1))
!         write(6,*)ybuf(i),xnow
          xnow = xnow + dx
      end do
    2 npts = i
      return 
      end

!*****************************************************
      subroutine rmean(c, a, npts, nr)
! IT MIGHT BE EASIER IF NR, THE NUMBER OF FILTER POINTS BE EVEN
      dimension c(1), a(1)
      nh = int(nr / 2)
      nb = (npts - nh) + 1
      do i = 1, npts
!
! FIRST NR POINTS
          a(i) = 0.
          if (i .le. nh) then
              is = i - 1
              do k = i - is, i + is
                  a(i) = a(i) + c(k)
              end do
              nds = (2 * is) + 1
!
! FROM NR TO RIGHT BEFORE LAST NR POINTS
              a(i) = a(i) / nds
          else if ((i .gt. nh) .and. (i .lt. nb)) then
              do k = i - nh, i + nh
                  a(i) = a(i) + c(k)
              end do
!
! LAST NR POINTS
              a(i) = a(i) / (nr + 1)
          else if ((i .ge. nb) .and. (i .lt. npts)) then
              idf = npts - i
              do k = i - idf, i + idf
                  a(i) = a(i) + c(k)
              end do
              ndb = (2 * idf) + 1
! LST POINT
              a(i) = a(i) / ndb
          else if (i .eq. npts) then
              a(i) = a(i - 1)
          end if
      end do
      return 
      end

!*****************************************************
!
!   routine SCALAR multiplies the first ns elements of ss by scal
!
      subroutine scalar(ss, ns, scal)

      real ss(*)
      do i = 1, ns
          ss(i) = scal * ss(i)

      end do
      return 
      end

!*****************************************************
      subroutine scdcm(a, n, a1, a2)
      dimension a(1)
      amin = a(1)
      amax = a(1)
      imin = 1
      imax = 1
      if (n .eq. 1) return 
      do 10 i = 2, n
      if (amax .ge. a(i)) goto 9
      amax = a(i)
      imax = i
    9 if (amin .le. a(i)) goto 10
      amin = a(i)
      imin = i
   10 continue
      if (imax - imin) 11, 11, 12
   11 a1 = amax
      a2 = amin
      return 
   12 a1 = amin
      a2 = amax
      return 
      end

!*****************************************************
!   routine adds (dt>0.0) or takes out (dt<0.0) a sinc**4 function
!
      subroutine sincfilt(a, len, dt)
      logical ladd
      complex a(*)
!
      pi = 4.0 * atan(1.0)
      ladd = dt .gt. 0.0
      dt0 = abs(dt)
      nt2 = (len / 2) + 1
      fny = 0.5 / dt0
      df = 1.0 / (len * dt0)
!
      fr = df
      do i = 2, nt2
      x = (pi * fr) / fny
      sinc = sin(x) / x
      sinc = sinc ** 4
      if (ladd) then
      a(i) = sinc * a(i)
      else
      a(i) = a(i) / sinc
      end if
      fr = fr + df
!
      end do
      return 
      end

!*****************************************************
!
!   routine TSADD adds two time series s1,s2.
!   Result is placed in s1.
!
      subroutine tsadd(s1, s2, ns)
      real s1(*), s2(*)
!
      common /errcom/ iuerr,lognowr
      logical lognowr
      if (iuerr .eq. 0) iuerr = 6
      if ((ns .lt. 1) .and. (.not. lognowr)) then
      write(unit=iuerr, fmt=*) ' error in TSADD - ns = ', ns
      stop
!
      end if
      do i = 1, ns
      s1(i) = s1(i) + s2(i)
!
      end do
      return 
      end

!*****************************************************
      subroutine tscopy(s1, s2, ns)
!
      real s1(*), s2(*)
      do i = 1, ns
      s2(i) = s1(i)
!
      end do
      return 
      end

!*****************************************************
!
!   routine TSCOSTAPER applies a cosine taper of length LEN to time seri
!es A.
!   Taper decays or grows as ISW is 1 or -1. To apply other than at star
!t of
!   time series A, pass to this routine as e.g. A(ipoint).
!   Adapted from FTAPER
!
      subroutine tscostaper(a, len, isw)
      real a(*)
!
      pi = 4.0 * atan(1.0)
      if (len .eq. 1) return 
      arg = pi / float(len - 1)
      if (isw .eq. 1) then
      do 10 i = 1, len
      a(i) = (.5 * (1. + cos(arg * float(i - 1)))) * a(i)
   10 continue
      else if (isw .eq. (-1)) then
      do 20 i = 1, len
      a(i) = (.5 * (1. - cos(arg * float(i - 1)))) * a(i)
   20 continue
      else
      write(unit=*, fmt='('' INCORRECT ISW IN FTAPER:PGM ABORTED'')') 
      stop
      end if
      return 
      end

!*****************************************************
!
!   routine TSCUT takes time series s, length ns, & extracts wavelet
!   with start point n1, end point n2. This is output as ss, starting
!   at point n3 and padded with zeros to total length ns.
!
      subroutine tscut(s, n1, n2, ss, n3, ns)
!
      real s(*), ss(*)
      do i = 1, ns
      ss(i) = 0.0
!
      end do
      nup = (n2 - n1) + n3
      if (((((((n1 .lt. 0) .or. (n1 .gt. ns)) .or. (n2 .lt. 0)) .or. (n2
     & .gt. ns)) .or. (n1 .gt. n2)) .or. (n3 .lt. 1)) .or. (nup .gt. ns)
     &) then
      write(unit=6, fmt=*) ' error in TSCUT - ns,n1,n2,n3 = ', ns, n1, 
     &n2, n3
      stop
!
      end if
      do i = n1, n2
      ii = (i - n1) + n3
      ss(ii) = s(i)
!
      end do
      return 
      end

!*****************************************************
!
!   routine TSDEMEAN takes a time series S1 of NP points, 
!   & removes the mean
!
      subroutine tsdemean(np, s1)
!
      real s1(*)
      smean = 0.0
      do i = 1, np
      smean = smean + s1(i)
      end do
!
      smean = smean / float(np)
      do i = 1, np
      s1(i) = s1(i) - smean
!
      end do
      return 
      end

!*****************************************************
!
!   routine TSERR calculates the L(nnorm) norm distance between sa & sb,
!   for the portion between n1 & n2. It is returned as err.
!
      subroutine tserr(sa, sb, n1, n2, nnorm, err)
!
      real sa(*), sb(*)
      snorm = 0.0
      if ((n1 .lt. 1) .or. (n2 .lt. 1)) then
      write(unit=6, fmt=*) ' error in TSERR - n1,n2 = ', n1, n2
      return 
!       
      end if
      do i = n1, n2
      s = abs(sa(i) - sb(i))
      snorm = snorm + (s ** nnorm)
!
      end do
      if (snorm .eq. 0.0) return 
      snorm = alog(snorm)
      snorm = snorm / float(nnorm)
      snorm = exp(snorm)
!
      err = snorm
      return 
      end

!*****************************************************
!
!   routine TSGAUSS sets up a Gaussuan wavelet of length NPTS,
!   centered on N0 & with standard deviation SD
!
      subroutine tsgauss(ss, npts, n0, sd)
!
      real ss(*)
      if ((n0 .lt. 1) .or. (n0 .gt. npts)) then
      write(unit=6, fmt=*) ' Error in TSGAUSS - n0 = ', n0
      stop
      end if
      if (sd .eq. 0.0) then
      write(unit=6, fmt=*) ' Error in TSGAUSS - sd = 0.0 '
      stop
!
      end if
      do i = 1, npts
      arg = float(i - n0)
      arg = - ((arg ** 2) / (2.0 * sd))
      if (arg .gt. (-30.0)) then
      ss(i) = exp(arg)
      else
      ss(i) = 0.0
      end if
!
      end do
      call tsnorm(ss, 1, npts, 2, snorm)
      sfac = 1.0 / snorm
!
      call scalar(ss, npts, sfac)
      return 
      end

!*****************************************************
      subroutine tsline(a, i1, i2)
      dimension a(1)
      slope = (a(i2) - a(i1)) / float(i2 - i1)
      do 10 i = i1, i2
      y = (slope * float(i - i1)) + a(i1)
      a(i) = y
   10 continue
      return 
      end

!*****************************************************
!
!   routine TSMULT multiplies two time series s1,s2.
!   Result is placed in s1.
!
      subroutine tsmult(s1, s2, ns)
      real s1(*), s2(*)
      common /errcom/ iuerr,lognowr
      logical lognowr
      if (iuerr .eq. 0) iuerr = 6
      if ((ns .lt. 1) .and. (.not. lognowr)) then
      write(unit=iuerr, fmt=*) ' error in TSADD - ns = ', ns
      stop
!
      end if
      do i = 1, ns
      s1(i) = s1(i) * s2(i)
!
      end do
      return 
      end

!*****************************************************
      subroutine tsnorm(ss, n1, n2, nnorm, snorm)
!
      real ss(*)
      snorm = 0.0
      if ((n1 .lt. 1) .or. (n2 .lt. 1)) then
      write(unit=6, fmt=*) ' error in TSNORM - n1,n2 = ', n1, n2
      return 
!       
      end if
      if (nnorm .lt. 0) then
      write(unit=6, fmt=*) ' Error in TSNORM - nnorm = 0 '
!
      return 
      else if (nnorm .eq. 0) then
      do i = n1, n2
      s = abs(ss(i))
      if (s .gt. snorm) snorm = s
      end do
!
      return 
      else
      do i = n1, n2
      s = abs(ss(i))
      snorm = snorm + (s ** nnorm)
!
      end do
      if (snorm .eq. 0.0) return 
      snorm = alog(snorm)
      snorm = snorm / float(nnorm)
      snorm = exp(snorm)
!
      end if
      return 
      end

!*****************************************************
!
!   routine TSPLOTSET sets up a multiple time series plot
!   it returns appropriate values of height, width etc
!   Allows 10 time series per screen
!
      subroutine tsplotset(w, h, hplot, ymin, xmin, ymax)
      w = 10.0
      h = 13.0
      nts = 10
      hplot = w / float(nts)
      ymin = 1.0
      xmin = 1.0
!
      ymax = w
      return 
      end

!*****************************************************
!
!   routine TSRAISE takes ss & raises the first ns elements to the n'th 
!power
!
      subroutine tsraise(ss, ns, n)
!
      real ss(*)
      do i = 1, ns
      ss(i) = ss(i) ** n
!
      end do
      return 
      end

!*****************************************************
!
!   routine TSREAD reads ss from an unformatted file open on unit iunit.
!   The number of points npts is taken from the file.   
!   If an error is encountered, ierr is returned as 1 (normal return 0)
!   
      subroutine tsread(iunit, npts, ss, ierr)
!
      real ss(*)
      ierr = 0
      read(unit=iunit, err=1000) npts, (tdum, ss(i),i = 1, npts)
!
      return 
 1000 ierr = 1
      return 
      end

!*****************************************************
!
!   routine TSREVERSE reverses the real time series ss,
!   and places the output in st
!
      subroutine tsreverse(ns, ss, st)
      real ss(*)
!
      real st(20000)
      do i = 1, ns
      st(i) = ss((ns - i) + 1)
!
      end do
      return 
      end
!
!   routine TSSCALE scales first n elements of
!   array ss such that Max(ss(i)))=1.0 
!   sfac is factor that array was multiplied by (output)
!   **  calls SCALAR    **
!
      subroutine tsscale(ns, ss, sfac)
      real ss(*)
      amax = 0.0
      do i = 1, ns
      as = abs(ss(i))
      if (as .gt. amax) amax = as
!
      end do
      if (amax .eq. 0.0) return 
      as = 1.0 / amax
      call scalar(ss, ns, as)
!
      sfac = as
      return 
      end

!*****************************************************
!
!   routine TSWRITE writes ss(i),i=nmin to nmax
!   to an unformatted file open on unit iunit. The time values
!   are also output.
!   
      subroutine tswrite(ss, nmin, nmax, tzero, dt, iunit)
!
      real ss(*), tt(20000)
      if (dt .eq. 0.0) then
      write(unit=6, fmt=*) ' dt is zero in TSWRITE - stop '
      stop
!
      end if
      t = tzero
      do i = nmin, nmax
      tt(i) = t
      t = t + dt
!
      end do
      nn = (nmax - nmin) + 1
!
      write(unit=iunit) nn, (tt(i), ss(i),i = nmin, nmax)
      return 
      end

!*****************************************************
!
!   routine TSZERO sets the first ns elements of ss to zero
!
      subroutine tszero(ss, ns)
!
      real ss(*)
      do i = 1, ns
      ss(i) = 0.0
!
      end do
      return 
      end

!*****************************************************
! PAD THE REST OF THE ARRAY WITH ZERO'S
! WHEN LEN IS NO A POWER OF TWO
      subroutine two(a, len)
      dimension a(1)
      itwo = 1
    1 itwo = itwo * 2
      if (len - itwo) 2, 4, 1
    2 continue
      do 3 i = len + 1, itwo
    3 a(i) = 0.
      len = itwo
    4 continue
      return 
      end

!*****************************************************
      subroutine zeroarray(a, n1, n2)
!       zeros out a real array from n1 to n2
      dimension a(1)
      do i = n1, n2
      a(i) = 0.
      end do
      return 
      end

!*****************************************************
!
!   routine looks at array , and if all elements are zero it replaces
!   the first by +1 & the second by -1
!
      subroutine zerochk(array, npts)
!
      real array(*)
      if (npts .lt. 2) return 
      do i = 1, npts
      if (array(i) .ne. 0.0) return 
!
      end do
      array(1) = 1.0
!
      array(2) = -1.0
      return 
      end

