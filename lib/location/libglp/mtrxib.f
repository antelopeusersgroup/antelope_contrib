      subroutine mtrxib(a,n1dim,n2dim,nrowa,ncola,lun,ieof) 
c---------------------------------------------------------------------- 
c        mtrxib reads in the matrix a by an unformated binary read. 
c  it's sibling is the alternate entry point called mtrxob.  it write 
c  a matrix by a fortran unformated write.  they are included together  
c  to clarify that they are complementary.  a third entry point is
c  called mtrxtob.  it is an output routine but writes the array out
c  by rows instead of colums.  nrowa and ncola are written in reverse 
c  order so than when mtrxib is called to read the array back in
c  one gets the transpose of the original.
c 
c  arguements-
c 
c      a-two dimensional array  
c      n1dim-first dimension of a in calling program
c      n2dim-second dimension of a in calling program 
c      nrowa-number of rows in a to write 
c            (when reading nrowa is read first) 
c      ncola-number of columns in a to write or read. 
c      lun-logical unit number of i/o device
c      ieof.eq.0-normal return (used only in input mode)
c          .eq.-1     end of file encountered 
c          .eq.+10    insufficient storage available in program 
c 
c  entry points - mtrxib,mtrxob,mtrxtob 
c---------------------------------------------------------------------- 
       dimension a(n1dim,n2dim) 
       ieof=0 
       read(lun,end=100) nrowa,ncola
       if((nrowa.gt.n1dim).or.(ncola.gt.n2dim)) go to 300 
       do 10 j=1,ncola
       read(lun,end=100) (a(i,j),i=1,nrowa) 
   10 continue
       return 
       entry mtrxob(a,n1dim,n2dim,nrowa,ncola,lun,ieof) 
       write(lun) nrowa,ncola 
       do 20 j=1,ncola
       write(lun) (a(i,j),i=1,nrowa)
   20 continue
       return 
      entry mtrxtob(a,n1dim,n2dim,nrowa,ncola,lun,ieof) 
      write(lun) ncola,nrowa
      do 30 i=1,nrowa 
   30 write(lun) (a(i,j),j=1,ncola) 
      return
  100 ieof=-1 
      return
  300 ieof=10 
      return
       end  

c $Id$ 
