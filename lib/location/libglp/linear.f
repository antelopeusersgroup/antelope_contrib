      subroutine linear(x,y,nx,u,v,nu)
c-----------------------------------------------------------------------
c     this subprogram evaluates y(x) at the points x=u(i) by a simple 
c  linear interpolation between points. 
c 
c  arguements-
c 
c     x-input vector of abscissa values 
c     y-input vector of ordinates 
c     nx-length of x and y
c     u-input vector giving abscissa values at which v is to be 
c       evaluated 
c     v-output vector of v(u) 
c     nu-length of u and v
c 
c  entry points 
c 
c     call linear 
c         entry point for the case when x and u vector are an increasing
c         sequence  (that is x(i+1).ge.x(i) and u(i+1).ge.u(i)) 
c 
c     call rlinear
c         alternate entry point for the case when the x and u vectors 
c         are a decreasing sequence. (that is x(i+1).le.x(i) and
c         u(i+1).le.u(i)) 
c 
c  written-july 1979  
c  language-university of minnesota fortran (mnf) 
c-----------------------------------------------------------------------
      dimension x(nx),y(nx) 
      dimension u(nu),v(nu) 
      if(nx.lt.2) return
c--initialize pointers
      ix = 2
      iu = 1
  100 if((iu.gt.nu).or.(ix.gt.nx)) go to 130
          if(x(ix).ge.u(iu)) then 
               grad=(y(ix)-y(ix-1))/(x(ix)-x(ix-1)) 
  110          if(x(ix).lt.u(iu)) go to 120 
                    v(iu)=y(ix-1)+grad*(u(iu)-x(ix-1))
                    iu=iu+1 
                    if(iu.gt.nu) return 
                    go to 110 
  120          continue 
               ix=ix+1
          else
               ix=ix+1
          endif 
      go to 100 
  130 continue
      return
      entry rlinear(x,y,nx,u,v,nu)
c--this is the alternate entry point for the case when x and u are in 
c--reverse order. 
c--now start counters at far end and decrement the pointers instead of  
c--incrementing them. 
      if(nx.lt.2) return
      ix=nx-1 
      iu=nu 
  200 if((iu.lt.1).or.(ix.lt.1))go to 230 
          if(x(ix).ge.u(iu)) then 
               grad=(y(ix)-y(ix+1))/(x(ix)-x(ix+1)) 
  210          if(x(ix).lt.u(iu)) go to 220 
                    v(iu)=y(ix+1)+grad*(u(iu)-x(ix+1))
                    iu=iu-1 
                    if(iu.lt.1) return
                    go to 210 
  220          continue 
               ix=ix-1
          else
               ix=ix-1
          endif 
      go to 200 
  230 continue
      return
      end 

c $Id$ 
