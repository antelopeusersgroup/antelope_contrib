      subroutine mult(a,atrnsp,b,btrnsp,c,m,n,l,idima,idimb,idimc,symm)
c
c        matrix multiplication.                          a.shakal 8/74
c        form the matrix product c(m by l) = a(m by n) * b(n by l).
c        if atrnsp =-1, form c = a(transpose)*b;  if btrnsp =-1, form
c        c = a*b(transpose);  if both =-1, form c=a(transpose)*
c        b(transpose).
c          (note the matrices must be conformable - the 1st matrix must
c        be mxn, and the 2nd nxl, in the form (normal or transpose) in
c        which multiplication is to be performed.)
*
*
      integer atrnsp,btrnsp,symm
      logical atrns,btrns,sym
      dimension a(idima,*),b(idimb,*),c(idimc,*)
*
c        switch for transpose of 1st matrix.
*
      atrns = .false.
      if(atrnsp .eq. -1) atrns =.true.
c        switch for transpose of 2nd matrix
         btrns = .false.
      if(btrnsp .eq. -1) btrns = .true.
*
c        switch for symmetric product - if symmetric, just calculate
c        below and on diagonal, then fold.
*
      sym = .false.
      if(symm .eq. +1) sym = .true.
      j2 = l
      do 30 i=1,m
         if(sym) j2=i
         do 30 j=1,j2
           c(i,j) = 0.
           do 30 k=1,n
              aa = a(i,k)
              if(atrns) aa=a(k,i)
              bb = b(k,j)
              if(btrns) bb =b(j,k)
              c(i,j) = aa*bb +c(i,j)
30    continue
c
      if(.not. sym) go to 50
*
      do 40 j=2,l
         i2 = j-1
         do 40 i=1,i2
40         c(i,j) = c(j,i)
*
*
50    return
      end
