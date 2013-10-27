      subroutine arriv1p(tone,pone,brone,maxone,dx,nx)
c-----------------------------------------------------------------------
c     this subroutine takes the full tidy travel time curve contained in
c  the ttab and xtab arrays and reduces it to strictly first arrivals.  
c  triplications are removed and low velocity zones shadows are removed 
c  by assuming a refraction off the top of the lvz.  this branch is 
c  continued until it crosses the branch corresponding to rays passing  
c  through the zone.  
c 
c  arguments
c     tone - on return tone contains the first arrival travel times.
c     pone - on return pone contains the first arrival ray parameters.
c     pone - on return brone contains the one character code specifying 
c            type of branch first arrival at that range is. 
c     maxone-length of "one" arrays in calling program. 
c     dx   - distance grid interval 
c     nx   - last arrival in the table always lies at distance nx*dx. 
c 
c  modified-may 1980  
c     fixed bug caused by short reversed segments resulting from
c     rounding error in regions where dp/dx is very large.
c   march, 1982 
c     converted while-endwhiles to if statements with goto's for
c     compatibility with ansi fortran.
c 
c-----------------------------------------------------------------------
	include 'table.common' 
      real tone(maxone),pone(maxone)
      character brone(maxone) 
      character revs,shad,refrac,cross
      logical revflag,lvzflag 
      parameter(revs='r',shad='s')
      parameter(refrac='l') 
      parameter(cross='c')
c--initialize tone array to zero to prevent mode errors and allow a 
c--later test to prevent moving outside the current maximum range in
c--the one arrays.
      do 10 i=1,maxone
   10 tone(i)=0.0 
c--the first point is always the straight up ray
      tone(1)=ttab(1) 
      pone(1)=ptab(1) 
      brone(1)=branch(1)
      ip=2  
      ix=2  
      revflag=.false. 
      lvzflag=.false. 
c--outer loop over all of /table/ arrays  
  100 if(ip.gt.ntab) go to 350  
          if(branch(ip).eq.revs) then 
c--reversed branches will come here 
c--mark the point before the reversal as long as it is not a shadow 
c--zone point.  this is needed when jumps occur at the end of a 
c--reversed branch. 
                if(.not.lvzflag) then 
                     tlast = ttab(ip-1) 
                     plast = ptab(ip-1) 
                     ixlast = ix - 1
                endif 
c--position ip pointer at end of reversed branch
  110          if(branch(ip).ne.revs)go to 120
                    if(ip.eq.ntab)then
                         nx=ix  
                         return 
                    endif 
                    ip=ip+1 
                    go to 110 
  120          continue 
c--raise a flag to indicate a reverse branch was found
               revflag=.true. 
          elseif(branch(ip).eq.shad) then 
c--come here when a shadow zone is found  
c--first note the end of this branch
c--keep previous value if jump occured on a reversed segment  
c--be careful of multiple shadow zones
               if(lvzflag) then 
                    ip=ip+1 
               elseif(branch(ip-1).ne.revs) then
                    tlast=ttab(ip-1)
                    plast=ptab(ip-1)
                    ixlast=ix-1 
                endif 
c--move ip pointer to the other side of the shadow zone 
  130          if(branch(ip).ne.shad) go to 140 
                    if(ip.eq.ntab) then 
                         nx = ix - 1
                         return 
                    endif 
                    ip=ip+1 
                    go to 130 
  140          continue 
c--raise a flag to indicate a shadow zone was found 
               lvzflag=.true. 
          elseif(.not.(revflag.or.lvzflag))then 
c--points away from triplications and shadow zones will come here 
               tone(ix)=ttab(ip)
               pone(ix)=ptab(ip)
               brone(ix)=branch(ip) 
               ix=ix+1
               ip=ip+1
          elseif(revflag.and.(.not.lvzflag))then
c--triplications away from shadow zones will come here
c--begin by positioning ix pointer back here
               ix=nint(xtab(ip)/dx)+1 
c--find crossover 
c--be careful of very short reversed segments and possible jump forward 
  150         if((tone(ix).ge.ttab(ip)).or.(tone(ix).eq.0.0))go to 160  
                    if(ip.eq.ntab) then 
                         nx=ix  
                         return 
                    endif 
                    ix=ix+1 
                    ip=ip+1 
                    if((branch(ip).eq.revs).or.(branch(ip).eq.shad))
     $                         go to 300  
                    go to 150 
  160          continue 
               tone(ix)=ttab(ip)
               pone(ix)=ptab(ip)
               brone(ix)=branch(ip) 
c--mark the crossover 
c--the point before the crossover is marked 
               brone(ix-1)=cross
c--lower flag 
               revflag=.false.
               ix=ix+1
               ip=ip+1
          else
c--when a shadow zone is nearby we will fall into this section. 
c--position ix pointer to present xtab. 
               ix=nint(xtab(ip)/dx)+1 
               if(ix.lt.ixlast) then
c--come here for a jump with no shadow zone 
                    istart=ix 
                    do 200 i=istart,ixlast
                         ix = ix + 1
                         ip=ip+1
                         if(ip.gt.ntab) then
                              nx=ixlast 
                              return
                         endif  
c--make sure another shadow zone or reversed segment is not present.
                         if((branch(ip).eq.shad).or.
     $                                    (branch(ip).eq.revs)) then
c--be careful of multiple shadow zones.  move the pointer to the
c--other size of the jump to prevent redefining tlast,plast, and ixlast 
                              if(branch(ip).eq.shad) then 
  170                              if(branch(ip).ne.shad) go to 180 
                                        ip = ip + 1 
                                        go to 170 
  180                              continue 
                              endif 
                              go to 300 
                         endif  
c--the following block will rarely be executed. 
c--it really should only be executed from rounding error with 
c--multiple lvz's 
                         if(ttab(ip).lt.tone(i)) then 
                              ix = i
                              tone(ix) = ttab(ip) 
                              pone(ix) = ptab(ip) 
                              brone(ix) = cross 
                              ix = ix + 1 
                              ip = ip + 1 
                              revflag = .false. 
                              lvzflag = .false. 
                              go to 300 
                         endif  
  200              continue 
               else 
c--come here if a true shadow zone is present.  fill the gap with 
c--refractions off the lid of the lvz.
c--firstmark this position and move the pointer back. 
                    ixstop=ix 
                    ix=ixlast+1 
  210               if(ix.ge.ixstop) go to 220
                         tone(ix)=tlast+float(ix-ixlast)*dx*plast 
                         pone(ix)=plast 
                         brone(ix)=refrac 
                         ix=ix+1
                         go to 210
  220               continue
               endif  
c--avoid a jump in this table by assuming a refraction off the lid
c--of the lvz.  find the crossover between this refracted branch and
c--the direct branch coming from the bottom of the lvz. 
               trefrac=tlast+float(ix-ixlast)*dx*plast
  230          if(trefrac.gt.ttab(ip)) go to 260
                    tone(ix)=trefrac
                    pone(ix)=plast
                    brone(ix)=refrac
                    ix=ix+1 
                    ip=ip+1 
                    if(ip.gt.ntab) then 
                         nx=ix-1
                         return 
                    endif 
c--make sure another shadow zone or reversed segment is not present.
                    if((branch(ip).eq.shad).or.(branch(ip).eq.revs))then
c--be careful of multiple shadow zones.  move the pointer to the
c--other size of the jump to prevent redefining tlast,plast, and ixlast 
                         if(branch(ip).eq.shad) then
  240                         if(branch(ip).ne.shad) go to 250
                                   ip = ip + 1
                                   go to 240
  250                         continue
                         endif  
                         go to 300
                    endif 
                    trefrac=tlast+float(ix-ixlast)*dx*plast 
                    go to 230 
  260          continue 
c--mark the crossover 
               brone(ix-1)=cross
c--lower flag 
               revflag=.false.
               lvzflag=.false.
          endif 
  300     continue
          go to 100 
  350 continue
      nx=ix-1 
      return
      end 

c $Id$ 
