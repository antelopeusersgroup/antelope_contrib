	subroutine gaussj(a,n,np,b,m,mp,istat)
c  gaussj.f
c  Linear equation solution bu Gauss-jordan elimination; from
c  Numerical Recipies, p. 28.
c  A is n by n matrix; physical dimensions np by np
c  b is n by m matrix of m right-side vectors, stored np by mp
c  output replaces a by its inverse, and b by solution vectors
c    Simple case, has m = 1.
c   returns istat=-1 if singular, -2 if bad-dimensions, 0 otherwise

c  nmax should ge .ge. n
	parameter (nmax = 2048)
	dimension a(np,np), b(np,mp)
	dimension ipiv(nmax),indxr(nmax), indxc(nmax)

	istat = 0
	do 11 j=1,n
	  ipiv(j) = 0
11	continue

c --- main loop over columns
	do 22 i=1,n
	  big = 0.
c --- outer loop of search for pivot elements
	  do 13 j=1,n
	    if (ipiv(j).ne.1) then
	      do 12 k=1,n
		if (ipiv(k).eq.0) then
		  if (abs(a(j,k)).ge.big) then
		    big = abs(a(j,k))
		    irow = j
		    icol = k
		  end if
		else if (ipiv(k).gt.1) then
		  print *,"GAUSSJ:  Singular Matrix"
		  istat = -1
		  return
	  	end if
12	      continue
	    end if
13	  continue
	  ipiv(icol) = ipiv(icol) + 1
c --- pivots now interchanged: indxc(i) = col. for ith piv. elem.
c	indxr(i) = row of original location of pivot.  
	  if (irow.ne.icol) then
	    do 14 l=1,n
	      dum = a(irow,l)
	      a(irow,l) = a(icol,l)
	      a(icol,l) = dum
14	    continue
	    do 15 l=1,m
	      dum = b(irow,l)
	      b(irow,l) = b(icol,l)
	      b(icol,l) = dum
15	    continue
	  end if

c--- divide pivot row by pivot elem. at irow,icol
	  indxr(i) = irow
	  indxc(i) = icol
	  if (a(icol,icol).eq.0.) then
	    print *,"GJINV:  Singular Matrix"
	    istat = -1
	    return
	  end if
	  pivinv=1./a(icol,icol)
	  a(icol,icol) = 1.
	  do 16 l=1,n
	    a(icol,l) = a(icol,l)*pivinv
16	  continue
	  do 17 l=1,m
	    b(icol,l) = b(icol,l)*pivinv
17	  continue
c  --- reduce rows now
	  do 21 ll=1,n
	    if (ll.ne.icol) then
	      dum=a(ll,icol)
	      a(ll,icol) = 0.
	      do 18 l=1,n
		a(ll,l)=a(ll,l)-a(icol,l)*dum
18	       continue
	      do 19 l=1,m
		b(ll,l)=b(ll,l)-b(icol,l)*dum
19	      continue
	    end if
21	  continue
22	continue

c  -- unscramble indexing
	do 24 l=n,1,-1
	  if(indxr(l).ne.indxc(l)) then
	    do 23 k=1,n
		dum=a(k,indxr(l))
		a(k,indxr(l))=a(k,indxc(l))
		a(k,indxc(l))=dum
23	    continue
	  end if
24	continue
	istat = 0
	return
	end

	subroutine dgaussj(a,n,np,b,m,mp,istat)
c  gaussj.f
c  Linear equation solution bu Gauss-jordan elimination; from
c  Numerical Recipies, p. 28.
c
c *** THIS is DOUBLE PRECISION version
C
c  A is n by n matrix; physical dimensions np by np
c  b is n by m matrix of m right-side vectors, stored np by mp
c  output replaces a by its inverse, and b by solution vectors
c    Simple case, has m = 1.
c   returns istat=-1 if singular, 0 otherwise

c  nmax should ge .ge. n
	implicit double precision (a-h,o-z)
	parameter (nmax = 1024)
	dimension a(np,np), b(np,mp)
	dimension ipiv(nmax),indxr(nmax), indxc(nmax)

	if (n.gt.nmax) then
	  print *,'GAUSSJ:  n>nmax; n=',n,', nmax=',nmax
	  istat=-2
	  return
	end if

	istat = 0
	do 11 j=1,n
	  ipiv(j) = 0
11	continue

c --- main loop over columns
	do 22 i=1,n
	  big = 0.
c --- outer loop of search for pivot elements
	  do 13 j=1,n
	    if (ipiv(j).ne.1) then
	      do 12 k=1,n
		if (ipiv(k).eq.0) then
		  if (abs(a(j,k)).ge.big) then
		    big = abs(a(j,k))
		    irow = j
		    icol = k
		  end if
		else if (ipiv(k).gt.1) then
		  print *,"GAUSSJ:  Singular Matrix"
		  istat = -1
		  return
	  	end if
12	      continue
	    end if
13	  continue
	  ipiv(icol) = ipiv(icol) + 1
c --- pivots now interchanged: indxc(i) = col. for ith piv. elem.
c	indxr(i) = row of original location of pivot.  
	  if (irow.ne.icol) then
	    do 14 l=1,n
	      dum = a(irow,l)
	      a(irow,l) = a(icol,l)
	      a(icol,l) = dum
14	    continue
	    do 15 l=1,m
	      dum = b(irow,l)
	      b(irow,l) = b(icol,l)
	      b(icol,l) = dum
15	    continue
	  end if

c--- divide pivot row by pivot elem. at irow,icol
	  indxr(i) = irow
	  indxc(i) = icol
	  if (a(icol,icol).eq.0.) then
	    print *,"GJINV:  Singular Matrix"
	    istat = -1
	    return
	  end if
	  pivinv=1./a(icol,icol)
	  a(icol,icol) = 1.
	  do 16 l=1,n
	    a(icol,l) = a(icol,l)*pivinv
16	  continue
	  do 17 l=1,m
	    b(icol,l) = b(icol,l)*pivinv
17	  continue
c  --- reduce rows now
	  do 21 ll=1,n
	    if (ll.ne.icol) then
	      dum=a(ll,icol)
	      a(ll,icol) = 0.
	      do 18 l=1,n
		a(ll,l)=a(ll,l)-a(icol,l)*dum
18	       continue
	      do 19 l=1,m
		b(ll,l)=b(ll,l)-b(icol,l)*dum
19	      continue
	    end if
21	  continue
22	continue

c  -- unscramble indexing
	do 24 l=n,1,-1
	  if(indxr(l).ne.indxc(l)) then
	    do 23 k=1,n
		dum=a(k,indxr(l))
		a(k,indxr(l))=a(k,indxc(l))
		a(k,indxc(l))=dum
23	    continue
	  end if
24	continue
	istat = 0
	return
	end
