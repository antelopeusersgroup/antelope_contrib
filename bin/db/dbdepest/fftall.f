      subroutine fast (b, n)
c
c replaces the real vector b(k), for k=1, 2, ...., n.
c with its finite descrete fourier transform.
c
c the dc term is returned in location b(1) with b(2) set to 0.
c thereafter the jth harmonic is returned as a complex number
c stored as b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is returned in b(n+1) with b(n+2) set to 0.
c hence, b must be dimensioned to size n + 2.
c the subroutine is called as  fast(b, n) where n=2**m and
c b is the real array described above.
c
c  author: g. d. bergland and m. t. dolan
c          bell lab., murray hill, nj 07974.
c
c  reference: program for digital signal processing,
c             (ed., digital signal processing committee, ieee
c             acoustics, speech, and signal processing society)
c             ieee press, new york, 1979.
c
c
      dimension b(2)
      common /cons/ pii, p7, p7two, c22, s22, pi2
c
c iw is a machine dependent write device number
c
      iw = 6
c
      pii = 4.0*atan(1.0)
      pi8 = pii/8.0
      p7  = 1.0/sqrt(2.0)
      p7two = 2.0*p7
      c22 = cos(pi8)
      s22 = sin(pi8)
      pi2 = 2.0*pii
      do 10 i=1, 25
        m = i
        nt = 2**i
        if (n.eq.nt)  go to 20
  10  continue
      write (iw,9999)
9999  format ( 33h n is not a power of two for fast )
      stop
  20  n4pow = m/2
c
c do a radix 2 iteration first if one is required
c
      if (m-n4pow*2)  40, 40, 30
  30  nn = 2
      int = n/nn
      call fr2tr (int, b(1), b(int+1))
      go to 50
  40  nn = 1
c
c perform radix 4 iterations.
c
  50  if (n4pow .eq. 0)  go to 70
      do 60 it=1, n4pow
        nn = nn*4
        int = n/nn
        call fr4tr (int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     *              b(1), b(int+1), b(2*int+1), b(3*int+1))
  60  continue
c
c perform in-place reordering.
c
  70  call ford1 (m, b)
      call ford2 (m, b)
      t = b(2)
      b(2) = 0.0
      b(n+1) = t
      b(n+2) = 0.0
      do 80 it=4, n, 2
        b(it) = -b(it)
  80  continue
      return
      end
      subroutine fsst (b, n)
c
c fourier synthesis subroutine.
c
c this subroutine synthesizes the real vector b(k), for
c k=1, 2, ..., n. from the fourier coefficients stored in the
c b array of size  n + 2.  the dc term is in b(1) with b(2) equal
c to 0.  the jth harmonic is stored as b(2*j+1) + i b(2*j+2).
c the n/2 harmonic is in b(n+1) with b(n+2) equal to 0.
c the subroutine is called as  fsst(b, n) where n=2**m and
c b is the real array discussed above.
c
      dimension b(2)
      common /cons/ pii, p7, p7two, c22, s22, pi2
c
c iw is a machine dependent write device number
c
      iw = 6
c
      pii = 4.0*atan(1.0)
      pi8 = pii/8.0
      p7  = 1.0/sqrt(2.0)
      p7two = 2.0*p7
      c22 = cos(pi8)
      s22 = sin(pi8)
      pi2 = 2.0*pii
      do 10 i=1, 25
        m = i
        nt = 2**i
        if (n.eq.nt)  go to 20
  10  continue
      write (iw,9999)
9999  format ( 33h n is not a power of two for fast )
      stop
  20  b(2) = b(n+1)
      do 30 i=4, n, 2
        b(i) = -b(i)
  30  continue
c
c scale the input by n.
c
      do 40 i=1,n
        b(i) = b(i)/float(n)
  40  continue
      n4pow = m/2
c
c scramble the inputs.
c
      call ford2 (m, b)
      call ford1 (m, b)
      if (n4pow .eq. 0)  go to 60
      nn = 4*n
      do 50 it=1, n4pow
        nn = nn/4
        int = n/nn
        call fr4syn (int, nn, b(1), b(int+1), b(2*int+1), b(3*int+1),
     *              b(1), b(int+1), b(2*int+1), b(3*int+1))
  50  continue
c
c do a radix 2 iteration if one is required.
c
  60  if (m-n4pow*2)  80, 80, 70
  70  int = n/2
      call fr2tr (int, b(1), b(int+1))
  80  return
      end
      subroutine fr2tr (int, b0, b1)
c
c radix 2 iteration subroutine.
c
      dimension b0(2), b1(2)
      do 10 k=1, int
        t = b0(k) + b1(k)
        b1(k) = b0(k) - b1(k)
        b0(k) = t
  10  continue
      return
      end
      subroutine fr4tr (int, nn, b0, b1, b2, b3, b4, b5, b6, b7)
c
c radix 4 iteration subroutine.
c
      dimension l(15), b0(2),b1(2),b2(2),b3(2),b4(2),b5(2),b6(2),b7(2)
      common /cons/pii, p7, p7two, c22, s22, pi2
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *  (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *  (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *  (l1,l(15))
c
c jthet is a reversed binary counter. jr steps two at a time to
c locate the real parts of intermediate results, and ji locates
c the imaginary part corresponding to jr.
c
      l(1) = nn/4
      do 40 k=2, 15
        if (l(k-1)-2)  10, 20, 30
  10    l(k-1) = 2
  20    l(k) = 2
        go to 40
  30    l(k) = l(k-1)/2
  40  continue
c
      piovn = pii/float(nn)
      ji = 3
      jl = 2
      jr = 2
c
      do 120 j1=2,  l1, 2
      do 120 j2=j1, l2, l1
      do 120 j3=j2, l3, l2
      do 120 j4=j3, l4, l3
      do 120 j5=j4, l5, l4
      do 120 j6=j5, l6, l5
      do 120 j7=j6, l7, l6
      do 120 j8=j7, l8, l7
      do 120 j9=j8, l9, l8
      do 120 j10=j9, l10, l9
      do 120 j11=j10, l11, l10
      do 120 j12=j11, l12, l11
      do 120 j13=j12, l13, l12
      do 120 j14=j13, l14, l13
      do 120 jthet=j14, l15, l14
        th2 = jthet - 2
        if (th2)  50, 50, 90
  50    do 60 k=1, int
          t0 = b0(k) + b2(k)
          t1 = b1(k) + b3(k)
          b2(k) = b0(k) - b2(k)
          b3(k) = b1(k) - b3(k)
          b0(k) = t0 + t1
          b1(k) = t0 - t1
  60    continue
c
        if (nn-4)  120, 120, 70
  70    k0 = int*4 + 1
        kl = k0 + int - 1
        do 80 k=k0, kl
          pr = p7*(b1(k) - b3(k))
          pi = p7*(b1(k) + b3(k))
          b3(k) = b2(k) + pi
          b1(k) = pi - b2(k)
          b2(k) = b0(k) - pr
          b0(k) = b0(k) + pr
  80    continue
        go to 120
c
  90    arg = th2*piovn
        c1 = cos(arg)
        s1 = sin(arg)
        c2 = c1**2 - s1**2
        s2 = c1*s1 + c1*s1
        c3 = c1*c2 - s1*s2
        s3 = c2*s1 + s2*c1
c
        int4 = int*4
        j0 = jr*int4 + 1
        k0 = ji*int4 + 1
        jlast = j0 + int - 1
        do 100 j=j0, jlast
          k = k0 + j - j0
          r1 = b1(j)*c1 - b5(k)*s1
          r5 = b1(j)*s1 + b5(k)*c1
          t2 = b2(j)*c2 - b6(k)*s2
          t6 = b2(j)*s2 + b6(k)*c2
          t3 = b3(j)*c3 - b7(k)*s3
          t7 = b3(j)*s3 + b7(k)*c3
          t0 = b0(j) + t2
          t4 = b4(k) + t6
          t2 = b0(j) - t2
          t6 = b4(k) - t6
          t1 = r1 + t3
          t5 = r5 + t7
          t3 = r1 - t3
          t7 = r5 - t7
          b0(j) = t0 + t1
          b7(k) = t4 + t5
          b6(k) = t0 - t1
          b1(j) = t5 - t4
          b2(j) = t2 - t7
          b5(k) = t6 + t3
          b4(k) = t2 + t7
          b3(j) = t3 - t6
 100    continue
c
        jr = jr + 2
        ji = ji - 2
        if (ji-jl)  110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end
      subroutine fr4syn (int, nn, b0, b1, b2, b3, b4, b5, b6, b7)
c
c radix 4 synthesis.
c
      dimension l(15), b0(2),b1(2),b2(2),b3(2),b4(2),b5(2),b6(2),b7(2)
      common /cons/pii, p7, p7two, c22, s22, pi2
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *  (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *  (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *  (l1,l(15))
c
      l(1) = nn/4
      do 40 k=2, 15
        if (l(k-1)-2)  10, 20, 30
  10    l(k-1) = 2
  20    l(k) = 2
        go to 40
  30    l(k) = l(k-1)/2
  40  continue
c
      piovn = pii/float(nn)
      ji = 3
      jl = 2
      jr = 2
c
      do 120 j1=2,  l1, 2
      do 120 j2=j1, l2, l1
      do 120 j3=j2, l3, l2
      do 120 j4=j3, l4, l3
      do 120 j5=j4, l5, l4
      do 120 j6=j5, l6, l5
      do 120 j7=j6, l7, l6
      do 120 j8=j7, l8, l7
      do 120 j9=j8, l9, l8
      do 120 j10=j9, l10, l9
      do 120 j11=j10, l11, l10
      do 120 j12=j11, l12, l11
      do 120 j13=j12, l13, l12
      do 120 j14=j13, l14, l13
      do 120 jthet=j14, l15, l14
        th2 = jthet - 2
        if (th2)  50, 50, 90
  50    do 60 k=1, int
          t0 = b0(k) + b1(k)
          t1 = b0(k) - b1(k)
          t2 = b2(k)*2.0
          t3 = b3(k)*2.0
          b0(k) = t0 + t2
          b2(k) = t0 - t2
          b1(k) = t1 + t3
          b3(k) = t1 -t3
  60    continue
c
        if (nn-4)  120, 120, 70
  70    k0 = int*4 + 1
        kl = k0 + int - 1
        do 80 k=k0, kl
          t2 = b0(k) - b2(k)
          t3 = b1(k) + b3(k)
          b0(k) = (b0(k) + b2(k))*2.0
          b2(k) = (b3(k) - b1(k))*2.0
          b1(k) = (t2 + t3)*p7two
          b3(k) = (t3 - t2)*p7two
  80    continue
        go to 120
c
  90    arg = th2*piovn
        c1 = cos(arg)
        s1 = -sin(arg)
        c2 = c1**2 - s1**2
        s2 = c1*s1 + c1*s1
        c3 = c1*c2 - s1*s2
        s3 = c2*s1 + s2*c1
c
        int4 = int*4
        j0 = jr*int4 + 1
        k0 = ji*int4 + 1
        jlast = j0 + int - 1
        do 100 j=j0, jlast
          k = k0 + j - j0
          t0 = b0(j) + b6(k)
          t1 = b7(k) - b1(j)
          t2 = b0(j) - b6(k)
          t3 = b7(k) + b1(j)
          t4 = b2(j) + b4(k)
          t5 = b5(k) - b3(j)
          t6 = b5(k) + b3(j)
          t7 = b4(k) - b2(j)
          b0(j) = t0 + t4
          b4(k) = t1 + t5
          b1(j) = (t2 + t6)*c1 - (t3 + t7)*s1
          b5(k) = (t2 + t6)*s1 + (t3 + t7)*c1
          b2(j) = (t0 - t4)*c2 - (t1 - t5)*s2
          b6(k) = (t0 - t4)*s2 + (t1 - t5)*c2
          b3(j) = (t2 - t6)*c3 - (t3 - t7)*s3
          b7(k) = (t2 - t6)*s3 + (t3 - t7)*c3
 100    continue
c
        jr = jr + 2
        ji = ji - 2
        if (ji-jl)  110, 110, 120
 110    ji = 2*jr - 1
        jl = jr
 120  continue
      return
      end
      subroutine ford1 (m, b)
c
c in-place reordering subroutine.
c
      dimension b(2)
c
      k = 4
      kl = 2
      n = 2**m
      do 40 j=4, n, 2
        if (k-j)  20, 20, 10
  10    t = b(j)
        b(j) = b(k)
        b(k) = t
  20    k = k - 2
        if (k-kl)  30, 30, 40
  30    k = 2*j
        kl = j
  40  continue
      return
      end
      subroutine ford2 (m, b)
c
c in-place reordering subroutine.
c
      dimension l(15), b(2)
      equivalence (l15,l(1)), (l14,l(2)), (l13,l(3)), (l12,l(4)),
     *    (l11,l(5)), (l10,l(6)), (l9,l(7)), (l8,l(8)), (l7,l(9)),
     *    (l6,l(10)), (l5,l(11)), (l4,l(12)), (l3,l(13)), (l2,l(14)),
     *    (l1,l(15))
      n = 2**m
      l(1) = n
      do 10 k=2, m
      l(k) = l(k-1)/2
  10  continue
      do 20 k=m, 14
        l(k+1) = 2
  20  continue
      ij = 2
      do 40 j1=2,  l1, 2
      do 40 j2=j1, l2, l1
      do 40 j3=j2, l3, l2
      do 40 j4=j3, l4, l3
      do 40 j5=j4, l5, l4
      do 40 j6=j5, l6, l5
      do 40 j7=j6, l7, l6
      do 40 j8=j7, l8, l7
      do 40 j9=j8, l9, l8
      do 40 j10=j9, l10, l9
      do 40 j11=j10, l11, l10
      do 40 j12=j11, l12, l11
      do 40 j13=j12, l13, l12
      do 40 j14=j13, l14, l13
      do 40 ji=j14, l15, l14
        if (ij-ji)  30, 40, 40
  30    t = b(ij-1)
        b(ij-1) = b(ji-1)
        b(ji-1) = t
        t = b(ij)
        b(ij) = b(ji)
        b(ji) = t
  40    ij = ij + 2
      return
      end
