      subroutine makeggt(g,n1dim,nrow,ncol,wk)
c---------------------------------------------------------------------- 
c       makeggt forms the inner product matrix of backus-gilbert
c  inversion.  this version is a space economizer in that the inner 
c  product matrix overwrites the input matrix of discretized kernels. 
c 
c  arguments- 
c 
c     g-n1dim by ncol array.
c       on input g contains the discretized kernels function in it's
c       columns.  on output g will contain the inner product matrix 
c       obtained from these functions (g will be ncol x ncol on output) 
c     n1dim-first dimension of g in calling program 
c           (note n1dim must be .ge. ncol)
c     nrow-number of rows in g on input.  
c     ncol-number of columns in g 
c          (g will returned as a ncol x ncol matrix)
c     wk-vector work area of length at least nrow 
c 
c  this routine utilizes the bla subroutines. 
c---------------------------------------------------------------------- 
      dimension g(n1dim,ncol),wk(nrow)
      data inc/1/ 
      do 150 j=1,ncol 
           call scopy(nrow,g(1,j),inc,wk,inc) 
           g(j,j)=dsdot(nrow,wk,inc,wk,inc) 
           if(j.ne.ncol) then 
                do 100 i=j+1,ncol 
  100                g(i,j)=dsdot(nrow,wk,inc,g(1,i),inc) 
           endif
  150 continue
      do 200 j=1,ncol-1 
           do 200 i=j+1,ncol
  200           g(j,i)=g(i,j) 
      return
      end 

c $Id$ 
