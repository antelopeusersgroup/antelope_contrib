c 
c input:                  * * ****
      	subroutine lstsqr(x,y,npts,a,b,r)
c output:                          + + +
c----------------------------------------------------------------------
c
c   Linear least-squares fit: y = a + bx
c
c   Input:
c   x,y   -=- arrays containing coordinates of points to be fit
c   npts  -=- size of above arrays
c
c   Output:
c   a,b   -=- fitted constants
c   r	  -=- correlation coefficients
c
c   Non-system routines: none
c
c----------------------------------------------------------------------
      	dimension x(npts), y(npts)
      	real n
      	n=float(npts)
c
c   Initialize sums
c
      	do 10 i = 1, npts
      	  sumx  = 0.0
      	  sumy  = 0.0
      	  sumxx = 0.0
      	  sumyy = 0.0
 	  sumxy = 0.0
10	continue
c
c   Accumulate sums
c
      	do 20 i = 1, npts
      	  sumx  = sumx  + x(i)
      	  sumy  = sumy  + y(i)
      	  sumxx = sumxx + x(i)*x(i)
      	  sumyy = sumyy + y(i)*y(i)
      	  sumxy = sumxy + x(i)*y(i)
20	continue
c
c   Calculate coefficients
c
      	tmp1=sumx*sumx/n
      	tmp2=sumy*sumy/n
      	b=(sumxy-(sumx*sumy/n))/(sumxx-tmp1)
      	a=(sumy/n)-(b*sumx/n)
      	tmp3=(sumxy-(sumx*sumy/n))**2
      	tmp4=sumxx-tmp1
      	tmp5=sumyy-tmp2
      	r=tmp3/(tmp4*tmp5)
c
      	return
      	end
