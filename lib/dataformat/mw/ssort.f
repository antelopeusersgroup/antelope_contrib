      subroutine ssort(x,y,n,kflag)
c***begin prologue  ssort
c***date written   761101   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  n6a2b1
c***keywords  quicksort,singleton quicksort,sort,sorting
c***author  jones, r. e., (snla)
c           wisniewski, j. a., (snla)
c***purpose  ssort sorts array x and optionally makes the same
c            interchanges in array y.  the array x may be sorted in
c            increasing order or decreasing order.  a slightly modified
c            quicksort algorithm is used.
c***description
c
c     written by rondall e. jones
c     modified by john a. wisniewski to use the singleton quicksort
c     algorithm.  date 18 november 1976.
c
c     abstract
c         ssort sorts array x and optionally makes the same
c         interchanges in array y.  the array x may be sorted in
c         increasing order or decreasing order.  a slightly modified
c         quicksort algorithm is used.
c
c     reference
c         singleton, r. c., algorithm 347, an efficient algorithm for
c         sorting with minimal storage, cacm,12(3),1969,185-7.
c
c     description of parameters
c         x - array of values to be sorted   (usually abscissas)
c         y - array to be (optionally) carried along
c         n - number of values in array x to be sorted
c         kflag - control parameter
c             =2  means sort x in increasing order and carry y along.
c             =1  means sort x in increasing order (ignoring y)
c             =-1 means sort x in decreasing order (ignoring y)
c             =-2 means sort x in decreasing order and carry y along.
c***references  singleton,r.c., algorithm 347, an efficient algorithm
c                 for sorting with minimal storage, cacm,12(3),1969,
c                 185-7.
c***routines called  none
c***end prologue  ssort
      dimension x(n),y(n),il(21),iu(21)
c***first executable statement  ssort
      nn = n
      if (nn.ge.1) go to 10
      write(6,*)' the number of values to be sorted was not positive'
      return
   10 kk = iabs(kflag)
      if ((kk.eq.1).or.(kk.eq.2)) go to 15
      write(6,*)' the sort control parameter, k,was not 2,1,-1, or -2'
      return
c
c alter array x to get decreasing order if needed
c
   15 if (kflag.ge.1) go to 30
      do 20 i=1,nn
   20 x(i) = -x(i)
   30 go to (100,200),kk
c
c sort x only
c
  100 continue
      m=1
      i=1
      j=nn
      r=.375
  110 if (i .eq. j) go to 155
  115 if (r .gt. .5898437) go to 120
      r=r+3.90625e-2
      go to 125
  120 r=r-.21875
  125 k=i
c                                  select a central element of the
c                                  array and save it in location t
      ij = i + ifix (float (j-i) * r)
      t=x(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if (x(i) .le. t) go to 130
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
  130 l=j
c                                  if last element of array is less thans
c                                  t, interchange with t
      if (x(j) .ge. t) go to 140
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if (x(i) .le. t) go to 140
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
      go to 140
  135 tt=x(l)
      x(l)=x(k)
      x(k)=tt
c                                  find an element in the second half ofs
c                                  the array which is smaller than t
  140 l=l-1
      if (x(l) .gt. t) go to 140
c                                  find an element in the first half of
c                                  the array which is greater than t
  145 k=k+1
      if (x(k) .lt. t) go to 145
c                                  interchange these elements
      if (k .le. l) go to 135
c                                  save upper and lower subscripts of
c                                  the array yet to be sorted
      if (l-i .le. j-k) go to 150
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 160
  150 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 160
c                                  begin again on another portion of
c                                  the unsorted array
  155 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  160 if (j-i .ge. 1) go to 125
      if (i .eq. 1) go to 110
      i=i-1
  165 i=i+1
      if (i .eq. j) go to 155
      t=x(i+1)
      if (x(i) .le. t) go to 165
      k=i
  170 x(k+1)=x(k)
      k=k-1
      if (t .lt. x(k)) go to 170
      x(k+1)=t
      go to 165
c
c sort x and carry y along
c
  200 continue
      m=1
      i=1
      j=nn
      r=.375
  210 if (i .eq. j) go to 255
  215 if (r .gt. .5898437) go to 220
      r=r+3.90625e-2
      go to 225
  220 r=r-.21875
  225 k=i
c                                  select a central element of the
c                                  array and save it in location t
      ij = i + ifix (float (j-i) *r)
      t=x(ij)
      ty= y(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if (x(i) .le. t) go to 230
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
  230 l=j
c                                  if last element of array is less thans
c                                  t, interchange with t
      if (x(j) .ge. t) go to 240
      x(ij)=x(j)
      x(j)=t
      t=x(ij)
       y(ij)= y(j)
       y(j)=ty
      ty= y(ij)
c                                  if first element of array is greater
c                                  than t, interchange with t
      if (x(i) .le. t) go to 240
      x(ij)=x(i)
      x(i)=t
      t=x(ij)
       y(ij)= y(i)
       y(i)=ty
      ty= y(ij)
      go to 240
  235 tt=x(l)
      x(l)=x(k)
      x(k)=tt
      tty= y(l)
       y(l)= y(k)
       y(k)=tty
c                                  find an element in the second half ofs
c                                  the array which is smaller than t
  240 l=l-1
      if (x(l) .gt. t) go to 240
c                                  find an element in the first half of
c                                  the array which is greater than t
  245 k=k+1
      if (x(k) .lt. t) go to 245
c                                  interchange these elements
      if (k .le. l) go to 235
c                                  save upper and lower subscripts of
c                                  the array yet to be sorted
      if (l-i .le. j-k) go to 250
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 260
  250 il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 260
c                                  begin again on another portion of
c                                  the unsorted array
  255 m=m-1
      if (m .eq. 0) go to 300
      i=il(m)
      j=iu(m)
  260 if (j-i .ge. 1) go to 225
      if (i .eq. 1) go to 210
      i=i-1
  265 i=i+1
      if (i .eq. j) go to 255
      t=x(i+1)
      ty= y(i+1)
      if (x(i) .le. t) go to 265
      k=i
  270 x(k+1)=x(k)
       y(k+1)= y(k)
      k=k-1
      if (t .lt. x(k)) go to 270
      x(k+1)=t
       y(k+1)=ty
      go to 265
c
c clean up
c
  300 if (kflag.ge.1) return
      do 310 i=1,nn
  310 x(i) = -x(i)
      return
      end

c $Id$ 
