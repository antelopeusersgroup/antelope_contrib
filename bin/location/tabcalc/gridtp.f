      subroutine gridtp(z,p,t,x,branch,dx,np) 
c-----------------------------------------------------------------------
c     gridtp takes the verbose irregularly spaced p,t, and x arrays and 
c  interpolates the table to obtain a set of travel times and ray 
c  parameter values regularly spaced in distance,dx.  reversed
c  branches remain and shadow zones are flagged by a single point.
c     this is a much improved version of an earlier subroutine of the 
c  same name that contained a hopeless swarm of bugs. 
c 
c  this version accepts layered models by trucating branches at large 
c  distance.
c 
c  arguments- 
c 
c     z-source depth  
c     p-verbose array of ray parameters.
c     t-travel time array 
c     x-epicentral distance array 
c     branch-name of this branch
c     dx-desired distance spacing in regularly spaced table 
c     np-length of verbose arrays.
c 
c  local variables- 
c     ip-pointer in verbose arrays
c     ix-pointer in regularly spaced arrays 
c     nx-total length of regularly spaced arrays (a counter)  
c     pwork,twork,xwork,brwork-gridtp equivalents of p,t,x,branch 
c 
c  written-july 1979  
c 
c  modifications-may 1980 
c 
c        changed interpolation from linear to three point lagrange
c        formula.  this amounted to writing a new interpolation 
c        subroutine and simply changing the calling statements here.
c 
c   august 1980 
c      modified to accept ray tracing results with layered models.
c      layered models produced reversed branches that in theory continue
c      forever.  this code simply trucates the travel time curves beyond
c      a predetermined distance set in a data statement.
c   january 1981 - changed interpolation again.  now the travel time
c      curves are interpolated by a quadratic formula. p-x curves are 
c      interpolated by a linear formula.  
c   july 1981 
c       another interpolation change but a major one.  a new routine was
c       added called intrp that does gridding much more cautiously
c       in that the raw x,t,p data fed to it is decimated and/or
c       suplimented with extra rays shot to fill in big gaps.  this 
c       subprogram then calls interpolators to interpolate this pseudo  
c       evenly spaced set of data onto a truely regular grid. 
c       this new routine also takes great pains to carefully define 
c       the crossover between upward and refracted arrivals.(an old 
c       frequently severe problem.) 
c   march 1982
c       name changed from "tidy" to gridtp which seems more descriptive.
c       with much reluctance former while-endwhile constructions
c       were converted to if statements with go to's.  my apologies.
c       blaim ansi for not including while in the new standard. 
c 
c-----------------------------------------------------------------------
	include 'switch.common'
	include 'first.common'
      dimension p(np),t(np),x(np) 
      character branch(np)
c--nxmax defines sizes of work space
c--maxdx defines the distance at which the curves are truncated.
c--that distance is maxdx*dx
c--careful maxdx must be consistent with same parameter in intrp
c     parameter(nxmax=20000,maxdx=50) 
c--rather than hardwired, we now make this = to first arrival table size
      parameter(maxdx=nxtab)
      parameter(nxmax=20000)
      real pwork(nxmax),twork(nxmax),xwork(nxmax) 
      character brwork(nxmax) 
      character bnorm,revs,shad,up
      parameter(bnorm='d',revs='r',shad='s',up='u') 
c--set counters 
      nx=2  
      ip=1  
c--take care of special first point 
      pwork(1)=p(1) 
      xwork(1)=x(1) 
      twork(1)=t(1) 
      brwork(1)=branch(1) 
c--begin major loop 
  100 if((ip.ge.np).or.(nx.gt.nxmax)) go to 250 
          if(branch(ip).eq.up) then 
c--mark the present position of the pointers
               ipstart=ip 
               ixstart=ifix(x(ipstart)/dx)+1
c--move the ip pointer until the branch type changes
  110          if((branch(ip).ne.up).or.(ip.ge.np)) go to 120 
                    ip=ip+1 
                    go to 110 
  120          continue 
c--mark this end of the branch  
               ipend=ip-1 
               ixend=ifix(x(ipend)/dx)
c--back up if we are too far out
c--note upward branches always start at zero so test of ixstart is not  
c--necessary
               if(ixend.gt.maxdx) ixend=maxdx 
c--fill this section of the xwork array 
               ix=ixstart 
               nxstart=nx 
  130          if((ix.gt.ixend).or.(nx.gt.nxmax)) go to 140 
                    brwork(nx)=up 
                    xwork(nx)=float(ix)*dx
                    nx=nx+1 
                    ix=ix+1 
                    go to 130 
  140          continue 
               iprange=ipend-ipstart + 1  
               nxrange=nx-nxstart 
c--interpolate to get gridded result
c--do nothing for short segments that do not cross a grid quadrature
c--point. 
               if(nxrange.gt.0) 
     $              call intrp(x(ipstart),t(ipstart),p(ipstart),
     $                 iprange,xwork(nxstart),twork(nxstart), 
     $                 pwork(nxstart),nxrange,z,dx,up)
          elseif(branch(ip).eq.bnorm) then
c--see above block for comments for the next two blocks as the
c--code is almost identical.
               if(ip.eq.1) then 
                    ipstart=1 
               else 
                    if(branch(ip-1).eq.shad) then 
                         ipstart = ip 
                    else
                         ipstart = ip - 1 
                    endif 
               endif  
               ixstart=ifix(x(ipstart)/dx) + 1
  150          if((branch(ip).ne.bnorm).or.(ip.ge.np)) go to 160
                    ip=ip+1 
                    go to 150 
  160          continue 
               ipend=ip-1 
               ixend=ifix(x(ipend)/dx)
               if(ixend.gt.maxdx) ixend=maxdx 
               if(ixstart.le.maxdx) then  
                    ix=ixstart
                    nxstart=nx
  170               if((ix.gt.ixend).or.(nx.gt.nxmax)) go to 180
                         brwork(nx)=bnorm 
                         xwork(nx)=float(ix)*dx 
                         nx=nx+1
                         ix=ix+1
                         go to 170
  180               continue
                    iprange=ipend-ipstart + 1 
                    nxrange=nx-nxstart
                    if(nxrange.gt.0)
     $                   call intrp(x(ipstart),t(ipstart),p(ipstart), 
     $                      iprange,xwork(nxstart),twork(nxstart),
     $                      pwork(nxstart),nxrange,z,dx,bnorm)
               endif  
          elseif(branch(ip).eq.revs) then 
               if(branch(ip-1).eq.shad) then
                    ipstart = ip
               else 
                    ipstart = ip - 1
               endif  
               ixstart=ifix(x(ipstart)/dx)
  190          if((branch(ip).ne.revs).or.(ip.gt.np)) go to 200 
                    ip=ip+1 
                    go to 190 
  200          continue 
               ipend=ip-1 
               ixend=ifix(x(ipend)/dx)+1
               if(ixstart.gt.maxdx) ixstart=maxdx 
               if(ixend.le.maxdx) then
                    ix=ixstart
                    nxstart=nx
  210               if((ix.lt.ixend).or.(nx.gt.nxmax)) go to 220
                         brwork(nx)=revs
                         xwork(nx)=float(ix)*dx 
                         nx=nx+1
                         ix=ix-1
                         go to 210
  220               continue
                    iprange=ipend-ipstart + 1 
                    nxrange=nx-nxstart
c--linear interpolation is used blindly for reversed branches as
c--this code is designed for earthquake sources and such branches 
c--are not very visible from earthquakes so high accuracy is not
c--required 
                    if(nxrange.gt.0) then 
                         call rlinear (x(ipstart),t(ipstart),iprange, 
     $                       xwork(nxstart),twork(nxstart),nxrange) 
                         call rlinear (x(ipstart),p(ipstart),iprange, 
     $                       xwork(nxstart),pwork(nxstart),nxrange) 
                    endif 
               endif  
          elseif(branch(ip).eq.shad) then 
c--flag one point as a shadow zone
               xwork(nx)=x(ip)
               twork(nx)=t(ip)
               pwork(nx)=p(ip)
               brwork(nx)=shad
               nx=nx+1
c--position ip pointer below the bottom of the lvz  
  230          if((branch(ip).ne.shad).or.(ip.ge.np)) go to 240 
                    ip=ip+1 
                    go to 230 
  240          continue 
          else
               call griderr(1,ip,nx,z)
               ip=ip+1
          endif 
          go to 100 
  250 continue
      if(nx.ge.nxmax) call griderr(2,ip,nx,z) 
      nx=nx-1 
c--output gridded table 
      call ttout(nx,z,pwork,twork,xwork,brwork,jprt)
      return
      end 

c $Id$ 
