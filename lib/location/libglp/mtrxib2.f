      subroutine mtrxib2(a,n1dim,n2dim,nrowa,ncola,lun,ieof)
c---------------------------------------------------------------------- 
c        mtrxib2 reads in the matrix a by an unformated binary read.
c        this a second version similar to an earlier version called 
c  simply mtrxib.  they differ only in that this version allows the 
c  user to not be forced to read the entire array.  
c 
c  arguements-
c 
c      a-two dimensional array  
c      n1dim-first dimension of a in calling program
c      n2dim-second dimension of a in calling program 
c      nrowa-number of rows in a to read (read from file) 
c      ncola-ncola is compared to the number of columns 
c            that exist on the file.  it reads the smaller of the two.  
c      lun-logical unit number of i/o device
c      ieof.eq.0-normal return (successfully read ncola columns)
c          .eq.-1     end of file encountered 
c          .eq.+10    insufficient storage available in program 
c          .eq.100    ncola given on input was too large.  return 
c                     actual number read in ncola.  
c 
c---------------------------------------------------------------------- 
       dimension a(n1dim,n2dim) 
       ieof=0 
       read(lun,end=100) nrowa,ncread 
      if(ncread.lt.ncola) then  
          ncola=ncread
          ieof=100
      endif 
       if((nrowa.gt.n1dim).or.(ncola.gt.n2dim)) go to 300 
       do 10 j=1,ncola
       read(lun,end=100) (a(i,j),i=1,nrowa) 
   10 continue
       return 
  100 ieof=-1 
      return
  300 ieof=10 
      return
      end 

c $Id$ 
