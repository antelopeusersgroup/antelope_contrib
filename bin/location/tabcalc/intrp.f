      subroutine intrp(xraw,traw,praw,nraw,xgrid,tgrid,pgrid,ngrid, 
     $                            zhypo,dx,branch)
c-------------------------------------------------------------------- 
c        takes raw input distance,time and ray parameter tabular
c  data that can be very irregularly spaced and grids this data 
c  in a safe way.  that is the raw data is both decimated and 
c  suplimented by extra rays to produce a table that is relatively
c  evenly spaced.  this table is then interpolated onto a regular 
c  grid.  this is safer than interpolating the raw table as experience
c  has proven this can occasionally produce interpolation problems
c  caused by the erratic spacing of the usual raw input.  the 
c  routine was designed to also mark the crossover between direct 
c  upward ray paths and refracted arrivals as closely as possible.
c  routine is designed to only work on forward branches.  it will 
c  surely fail if used on retrograde branches.
c 
c  arguments- 
c    xraw  - vector of length nraw of raw table of distances. 
c             (xraw must be in ascending sequence.  
c              i.e. forward branches) 
c    traw  - vector of length nraw of raw table of travel times.
c    praw  - vector of length nraw of raw table of ray parameters.
c    nraw  - length of raw arrays.
c    xgrid - array of length ngrid of grid values to be interpolated. 
c    tgrid - array of length ngrid to contain interpolated travel times 
c    pgrid - array of length ngrid to contain interpolated
c             ray parameter table.
c    ngrid - length of grid arrays.  with refracted branches ngrid is 
c             not altered.  with direct (upward) rays ngrid may be
c             increased to better mark the crossover between direct 
c             and refracted arrivals. 
c   zhypo  - source depth raw table values correspond to. 
c   dx     - distance spacing of xgrid values.
c   branch - integer variable containing characters identifying 
c             branch type.  (see data statements below) 
c 
c  external variables-
c    velocity model variable are passed through common/surface/ 
c         v,d,ns
c 
c  written - july 1981
c  language- 1977 ansi standard fortran 
c  author  - gary l. pavlis 
c-------------------------------------------------------------------- 
	include 'model.common' 
	include 'first.common'
      integer nraw,ngrid
      character branch
      real dx,zhypo 
      real xraw(nraw),traw(nraw),praw(nraw) 
      real xgrid(ngrid),tgrid(ngrid),pgrid(ngrid) 
c--the following parameters determine amount of storage required
c--for work spaces used in this routine.  
c--maxdx should be the same as in subroutine gridtp 
c--nwsize should be set to some number .gt. maxdx/xminsc  (see below) 
c--the actual storage allocated is nwmax = nwsize + 1 
c--nwsize is used for loop termination to assure space for at least 
c--one extra ray used to mark end of upward branches. 
c     parameter(maxdx=50,nwsize=120)
c--rather than hardwired, we now make maxdx = first arrival table size
      parameter(maxdx=nxtab)
      parameter(nwsize=120)
      parameter(nwmax = nwsize + 1) 
c--these numbers are used to determine the size of dxmin and dxmax
c--compared to dx (see below)  entries in work array will all 
c--normally be .gt. dxmin and .lt.dxmax.
c--larger spacings will occasionally occur if large gaps have to be 
c--filled.
      parameter(xminsc=0.5,xmaxsc=0.75) 
      real xwork(nwmax),pwork(nwmax),twork(nwmax) 
      character uptest
      parameter(uptest='u') 
      dxmin = dx*xminsc 
      dxmax = dx*xmaxsc 
c--the curves are truncated beyond distance xmax
      xmax = dx*float(maxdx)
      nwork = 1 
      xmark = xraw(1) 
      pmark = praw(1) 
      xwork(1) = xraw(1)
      twork(1) = traw(1)
      pwork(1) = praw(1)
      do 200 i=2,nraw 
            deltax = abs(xraw(i) - xmark) 
            if(deltax.gt.dxmin) then
                  if(deltax.le.dxmax) then
c--block for regions to be decimated or left alone  
                        if(nwork.ge.nwsize) go to 250 
                        nwork = nwork + 1 
                        xmark = xraw(i) 
                        pmark = praw(i) 
                        xwork(nwork) = xraw(i)
                        twork(nwork) = traw(i)
                        pwork(nwork) = praw(i)
                  else
c--block for jumps that need to be filled.
                        nsplit = nint(deltax/dxmax) + 1 
                        if((nsplit+nwork-1).gt.nwsize)
     $                          nsplit = nwsize - nwork + 1 
                        if(nsplit.le.2) then
                              if(nwork.ge.nwsize) go to 250 
                              nwork = nwork + 1 
                              xmark = xraw(i) 
                              pmark = praw(i) 
                              xwork(nwork) = xraw(i)
                              twork(nwork) = traw(i)
                              pwork(nwork) = praw(i)
                        else
                              call split(pwork(nwork),xwork(nwork), 
     $                            pmark,xmark,praw(i),xraw(i),nsplit) 
                              if(branch.eq.uptest) then 
                                    do 100 j=nwork+1,nwork+nsplit-2 
                                          call txup(pwork(j),zhypo,vel, 
     $                                  dep,nvel,twork(j),xwork(j),ibot)
  100                               continue
                              else
                                    do 150 j=nwork+1,nwork+nsplit-2 
                                          call txref(pwork(j),zhypo,vel,
     $                                  dep,nvel,twork(j),xwork(j),ibot)
  150                               continue
                              endif 
                              nwork = nwork + nsplit - 1
                              xmark = xraw(i) 
                              pmark = praw(i) 
                              xwork(nwork) = xraw(i)
                              twork(nwork) = traw(i)
                              pwork(nwork) = praw(i)
                        endif 
                  endif 
                  if(xmark.gt.xmax) go to 250 
            endif 
  200 continue
c--alternate loop exit
  250 continue
c--this section makes sure the final raw grid point is kept verbatim
c--to avoid disasterous truncation of regular grid  
      if(xmark.le.xmax) then
            if(xmark.ne.xraw(nraw)) then  
                  nwork = nwork + 1 
                  xwork(nwork) = xraw(nraw) 
                  twork(nwork) = traw(nraw) 
                  pwork(nwork) = praw(nraw) 
                  deltax = xwork(nwork) - xwork(nwork-1)
                  if(deltax.lt.0.0) nwork = nwork - 1 
                  if(deltax.gt.dxmax) then
                        print *,'//////warning//////' 
                        print *,'possible error in curves at depth=', 
     $                           zhypo
                        print *,'gridded points between ',
     $                       xwork(nwork-1),' and ',xwork(nwork), 
     $                       ' are subject to interpolation error'
                 endif
            endif 
      endif 
c--the following actually does the interpolation. 
      call linear(xwork,twork,nwork,xgrid,tgrid,ngrid)
      call linear(xwork,pwork,nwork,xgrid,pgrid,ngrid)
      return
      end 

c $Id$ 
