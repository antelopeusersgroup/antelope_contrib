*
*
********************************************************************************
*
      subroutine cl1(k, l, m, n, klmd, klm2d, nklmd, n2d,                       
     * q, kode, toler, iter, x, res, error, cu, iu, s)
c
c
c this subroutine uses a modification of the simplex
c method of linear programming to calculate an l1 solution
c to a k by n system of linear equations
c             ax=b
c subject to l linear equality constraints
c             cx=d
c and m linear inequality constraints
c             ex.le.f.
c description of parameters
c k      number of rows of the matrix a (k.ge.1).
c l      number of rows of the matrix c (l.ge.0).
c m      number of rows of the matrix e (m.ge.0).
c n      number of columns of the matrices a,c,e (n.ge.1).
c klmd   set to at least k+l+m for adjustable dimensions.
c klm2d  set to at least k+l+m+2 for adjustable dimensions.
c nklmd  set to at least n+k+l+m for adjustable dimensions.
c n2d    set to at least n+2 for adjustable dimensions
c q      two dimensional real array with klm2d rows and
c        at least n2d columns.
c        on entry the matrices a,c and e, and the vectors
c        b,d and f must be stored in the first k+l+m rows
c        and n+1 columns of q as follows
c             a b
c         q = c d
c             e f
c        these values are destroyed by the subroutine.
c kode   a code used on entry to, and exit
c        from, the subroutine.
c        on entry, this should normally be set to 0.
c        however, if certain nonnegativity constraints
c        are to be included implicitly, rather than
c        explicitly in the constraints ex.le.f, then kode
c        should be set to 1, and the nonnegativity
c        constraints included in the arrays x and
c        res (see below).
c        on exit, kode has one of the
c        following values
c             0- optimal solution found,
c             1- no feasible solution to the
c                constraints,
c             2- calculations terminated
c                prematurely due to rounding errors,
c             3- maximum number of iterations reached.
c toler  a small positive tolerance. empirical
c        evidence suggests toler = 10**(-d*2/3),
c        where d represents the number of decimal
c        digits of accuracy available. essentially,
c        the subroutine cannot distinguish between zero
c        and any quantity whose magnitude does not exceed
c        toler. in particular, it will not pivot on any
c        number whose magnitude does not exceed toler.
c iter   on entry iter must contain an upper bound on
c        the maximum number of iterations allowed.
c        a suggested value is 10*(k+l+m). on exit iter
c        gives the number of simplex iterations.
c x      one dimensional real array of size at least n2d.
c        on exit this array contains a
c        solution to the l1 problem. if kode=1
c        on entry, this array is also used to include
c        simple nonnegativity constraints on the
c        variables. the values -1, 0, or 1
c        for x(j) indicate that the j-th variable
c        is restricted to be .le.0, unrestricted,
c        or .ge.0 respectively.
c res    one dimensional real array of size at least klmd.
c        on exit this contains the residuals b-ax
c        in the first k components, d-cx in the
c        next l components (these will be =0),and
c        f-ex in the next m components. if kode=1 on
c        entry, this array is also used to include simple
c        nonnegativity constraints on the residuals
c        b-ax. the values -1, 0, or 1 for res(i)
c        indicate that the i-th residual (1.le.i.le.k) is
c        restricted to be .le.0, unrestricted, or .ge.0
c        respectively.
c error  on exit, this gives the minimum sum of
c        absolute values of the residuals.
c cu     a two dimensional real array with two rows and
c        at least nklmd columns used for workspace.
c iu     a two dimensional integer array with two rows and
c        at least nklmd columns used for workspace.
c s      integer array of size at least klmd, used for
c        workspace.
c if your fortran compiler permits a single column of a two
c dimensional array to be passed to a one dimensional array
c through a subroutine call, considerable savings in
c execution time may be achieved through the use of the
c following subroutine, which operates on column vectors.
c     subroutine col(v1, v2, xmlt, notrow, k)
c this subroutine adds to the vector v1 a multiple of the
c vector v2 (elements 1 through k excluding notrow).
c     dimension v1(k), v2(k)
c     kend = notrow - 1
c     kstart = notrow + 1
c     if (kend .lt. 1) go to 20
c     do 10 i=1,kend
c        v1(i) = v1(i) + xmlt*v2(i)
c  10 continue
c     if(kstart .gt. k) go to 40
c  20 do 30 i=kstart,k
c       v1(i) = v1(i) + xmlt*v2(i)
c  30 continue
c  40 return
c     end
c see comments following statement labelled 440 for
c instructions on the implementation of this modification.
c
c
      double precision sum
      double precision dble
      real q, x, z, cu, sn, zu, zv, cuv, res, xmax, xmin,
     * error, pivot, toler, tpivot
      real abs
      integer i, j, k, l, m, n, s, ia, ii, in, iu, js, kk,
     * nk, n1, n2, jmn, jpn, klm, nkl, nk1, n2d, iimn,
     * iout, iter, klmd, klm1, klm2, kode, nklm, nkl1,
     * klm2d, maxit, nklmd, iphase, kforce, iineg
      integer iabs
      dimension q(klm2d,n2d), x(n2d), res(klmd),
     * cu(2,nklmd), iu(2,nklmd), s(klmd)
c
c initialization.
c
      maxit = iter
      n1 = n + 1
      n2 = n + 2
      nk = n + k
      nk1 = nk + 1
      nkl = nk + l
      nkl1 = nkl + 1
      klm = k + l + m
      klm1 = klm + 1
      klm2 = klm + 2
      nklm = n + klm
      kforce = 1
      iter = 0
      js = 1
      ia = 0
c set up labels in q.
      do 10 j=1,n
         q(klm2,j) = j
   10 continue
      do 30 i=1,klm
         q(i,n2) = n + i
         if (q(i,n1).ge.0.) go to 30
         do 20 j=1,n2
            q(i,j) = -q(i,j)
   20    continue
   30 continue
c set up phase 1 costs.
      iphase = 2
      do 40 j=1,nklm
         cu(1,j) = 0.
         cu(2,j) = 0.
         iu(1,j) = 0
         iu(2,j) = 0
   40 continue
      if (l.eq.0) go to 60
      do 50 j=nk1,nkl
         cu(1,j) = 1.
         cu(2,j) = 1.
         iu(1,j) = 1
         iu(2,j) = 1
   50 continue
      iphase = 1
   60 if (m.eq.0) go to 80
      do 70 j=nkl1,nklm
         cu(2,j) = 1.
         iu(2,j) = 1
         jmn = j - n
         if (q(jmn,n2).lt.0.) iphase = 1
   70 continue
   80 if (kode.eq.0) go to 150
      do 110 j=1,n
         if (x(j)) 90, 110, 100
   90    cu(1,j) = 1.
         iu(1,j) = 1
         go to 110
  100    cu(2,j) = 1.
         iu(2,j) = 1
  110 continue
      do 140 j=1,k
         jpn = j + n
         if (res(j)) 120, 140, 130
  120    cu(1,jpn) = 1.
         iu(1,jpn) = 1
         if (q(j,n2).gt.0.0) iphase = 1
         go to 140
  130    cu(2,jpn) = 1.
         iu(2,jpn) = 1
         if (q(j,n2).lt.0.0) iphase = 1
  140 continue
  150 if (iphase.eq.2) go to 500
c compute the marginal costs.
  160 do 200 j=js,n1
         sum = 0.d0
         do 190 i=1,klm
            ii = q(i,n2)
            if (ii.lt.0) go to 170
            z = cu(1,ii)
            go to 180
  170       iineg = -ii
            z = cu(2,iineg)
  180       sum = sum + dble(q(i,j))*dble(z)
  190    continue
         q(klm1,j) = sum
  200 continue
      do 230 j=js,n
         ii = q(klm2,j)
         if (ii.lt.0) go to 210
         z = cu(1,ii)
         go to 220
  210    iineg = -ii
         z = cu(2,iineg)
  220    q(klm1,j) = q(klm1,j) - z
  230 continue
c determine the vector to enter the basis.
  240 xmax = 0.
      if (js.gt.n) go to 490
      do 280 j=js,n
         zu = q(klm1,j)
         ii = q(klm2,j)
         if (ii.gt.0) go to 250
         ii = -ii
         zv = zu
         zu = -zu - cu(1,ii) - cu(2,ii)
         go to 260
  250    zv = -zu - cu(1,ii) - cu(2,ii)
  260    if (kforce.eq.1 .and. ii.gt.n) go to 280
         if (iu(1,ii).eq.1) go to 270
         if (zu.le.xmax) go to 270
         xmax = zu
         in = j
  270    if (iu(2,ii).eq.1) go to 280
         if (zv.le.xmax) go to 280
         xmax = zv
         in = j
  280 continue
      if (xmax.le.toler) go to 490
      if (q(klm1,in).eq.xmax) go to 300
      do 290 i=1,klm2
         q(i,in) = -q(i,in)
  290 continue
      q(klm1,in) = xmax
c determine the vector to leave the basis.
  300 if (iphase.eq.1 .or. ia.eq.0) go to 330
      xmax = 0.
      do 310 i=1,ia
         z = abs(q(i,in))
         if (z.le.xmax) go to 310
         xmax = z
         iout = i
  310 continue
      if (xmax.le.toler) go to 330
      do 320 j=1,n2
         z = q(ia,j)
         q(ia,j) = q(iout,j)
         q(iout,j) = z
  320 continue
      iout = ia
      ia = ia - 1
      pivot = q(iout,in)
      go to 420
  330 kk = 0
      do 340 i=1,klm
         z = q(i,in)
         if (z.le.toler) go to 340
         kk = kk + 1
         res(kk) = q(i,n1)/z
         s(kk) = i
  340 continue
  350 if (kk.gt.0) go to 360
      kode = 2
      go to 590
  360 xmin = res(1)
      iout = s(1)
      j = 1
      if (kk.eq.1) go to 380
      do 370 i=2,kk
         if (res(i).ge.xmin) go to 370
         j = i
         xmin = res(i)
         iout = s(i)
  370 continue
      res(j) = res(kk)
      s(j) = s(kk)
  380 kk = kk - 1
      pivot = q(iout,in)
      ii = q(iout,n2)
      if (iphase.eq.1) go to 400
      if (ii.lt.0) go to 390
      if (iu(2,ii).eq.1) go to 420
      go to 400
  390 iineg = -ii
      if (iu(1,iineg).eq.1) go to 420
  400 ii = iabs(ii)
      cuv = cu(1,ii) + cu(2,ii)
      if (q(klm1,in)-pivot*cuv.le.toler) go to 420
c bypass intermediate vertices.
      do 410 j=js,n1
         z = q(iout,j)
         q(klm1,j) = q(klm1,j) - z*cuv
         q(iout,j) = -z
  410 continue
      q(iout,n2) = -q(iout,n2)
      go to 350
c gauss-jordan elimination.
  420 if (iter.lt.maxit) go to 430
      kode = 3
      go to 590
  430 iter = iter + 1
      do 440 j=js,n1
         if (j.ne.in) q(iout,j) = q(iout,j)/pivot
  440 continue
c if permitted, use subroutine col of the description
c section and replace the following seven statements down
c to and including statement number 460 by..
c     do 460 j=js,n1
c        if(j .eq. in) go to 460
c        z = -q(iout,j)
c        call col(q(1,j), q(1,in), z, iout, klm1)
c 460 continue
      do 460 j=js,n1
         if (j.eq.in) go to 460
         z = -q(iout,j)
         do 450 i=1,klm1
            if (i.ne.iout) q(i,j) = q(i,j) + z*q(i,in)
  450    continue
  460 continue
      tpivot = -pivot
      do 470 i=1,klm1
         if (i.ne.iout) q(i,in) = q(i,in)/tpivot
  470 continue
      q(iout,in) = 1./pivot
      z = q(iout,n2)
      q(iout,n2) = q(klm2,in)
      q(klm2,in) = z
      ii = abs(z)
      if (iu(1,ii).eq.0 .or. iu(2,ii).eq.0) go to 240
      do 480 i=1,klm2
         z = q(i,in)
         q(i,in) = q(i,js)
         q(i,js) = z
  480 continue
      js = js + 1
      go to 240
c test for optimality.
  490 if (kforce.eq.0) go to 580
      if (iphase.eq.1 .and. q(klm1,n1).le.toler) go to 500
      kforce = 0
      go to 240
c set up phase 2 costs.
  500 iphase = 2
      do 510 j=1,nklm
         cu(1,j) = 0.
         cu(2,j) = 0.
  510 continue
      do 520 j=n1,nk
         cu(1,j) = 1.
         cu(2,j) = 1.
  520 continue
      do 560 i=1,klm
         ii = q(i,n2)
         if (ii.gt.0) go to 530
         ii = -ii
         if (iu(2,ii).eq.0) go to 560
         cu(2,ii) = 0.
         go to 540
  530    if (iu(1,ii).eq.0) go to 560
         cu(1,ii) = 0.
  540    ia = ia + 1
         do 550 j=1,n2
            z = q(ia,j)
            q(ia,j) = q(i,j)
            q(i,j) = z
  550    continue
  560 continue
      go to 160
  570 if (q(klm1,n1).le.toler) go to 500
      kode = 1
      go to 590
  580 if (iphase.eq.1) go to 570
c prepare output.
      kode = 0
  590 sum = 0.d0
      do 600 j=1,n
         x(j) = 0.
  600 continue
      do 610 i=1,klm
         res(i) = 0.
  610 continue
      do 640 i=1,klm
         ii = q(i,n2)
         sn = 1.
         if (ii.gt.0) go to 620
         ii = -ii
         sn = -1.
  620    if (ii.gt.n) go to 630
         x(ii) = sn*q(i,n1)
         go to 640
  630    iimn = ii - n
         res(iimn) = sn*q(i,n1)
         if (ii.ge.n1 .and. ii.le.nk) sum = sum +
     *    dble(q(i,n1))
  640 continue
      error = sum
      return
      end
c  8  3  2  5  0     1.e-5130
c  2  0  1  3  1  7
c  7  4  4 15  7  4
c  9  4  7 20  6  7
c  2  2  1  5  3  4
c  9  3  2 14 10  0
c  4  5  0  9  9  4
c  4  4  9 17 -1  9
c  1  6  2  9  5  6
c  0  4  5  9 -1  5
c  3  2  7 12 -2  1
c  3  6 12 21 -3  6
c  0  3  6  9 -3  5
c  6  2  4 12  4  6
 
 
 
 
 
 
 
 
 
