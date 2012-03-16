*
*     program to compute the first arrival for a plane layered earth
*         and mark that arrival in the headers of the
*         green's functions used by mtinv
*
*     Chuck Ammon, Saint Louis University
*     Version 1.1 - September, 1994
*
      program marktime
*
      integer mxl, mxpts
      parameter (mxl = 100, mxpts = 4096)
      integer err
      real alpha(mxl), beta(mxl)
      real rho(mxl), th(mxl), xdummy, seis(mxpts)
      real src_depth, range, user(3)
      real t, dh, tmin
      character*256 modelfile, gftn_name, infile
      character*7 suf(8)
*
      integer  i, lsrc, iargc, nargs
      integer stdin, stdout, stderr
      integer inunit, ounit, nl, lyr, nlm1
      integer nlen,lnblnk
*      
      stdin = 5
      stdout = 6
      stderr = 6
      inunit = 10
      ounit = 11
      nl = mxl
*
*
      suf(1) = "_pz.vds"
      suf(2) = "_pz.vss"
      suf(3) = "_pz.clv"
      suf(4) = "_sr.vds"
      suf(5) = "_sr.vss"
      suf(6) = "_sr.clv"
      suf(7) = "_st.vds"
      suf(8) = "_st.vss"
*
      tmin = 1e6
*      
*
*      Terminal or command line IO
*   
       nargs = iargc()

       if(nargs .eq. 0) then
*
      write(stdout,*) 'What is the model file name?'
      read(stdin,*) modelfile
      write(stdout,*) 'What is the gftn file name?'
      read(stdin,*) gftn_name

*
       else if(nargs .eq. 2) then
*
         call getarg(1,modelfile)
         call getarg(2,gftn_name)
*
       else
*
         write(stdout,*)'e.g. usage: marktime CUS.mod lsct'
         stop
*
       end if
*
*      Message to the user
*
       nlen = lnblnk(modelfile)
       write(stdout,*)
     &   'Using model:  ',modelfile(1:nlen)
*
       nlen = lnblnk(gftn_name)
       write(stdout,*)
     &   '   Marking first arrival in  ',gftn_name(1:nlen)
*      
*
*      READ IN THE green's function header info
*
*
*      get the station azimuth used in the compuation of the mij
*        responses
*
       infile(1:nlen+7) = gftn_name(1:nlen)//suf(1)      
       call newhdr   
       call rsac1(infile,seis,npts,btime,dt,npts,nerr)
       call getfhv('DIST',range,nerr)
       call getfhv('EVDP',evdp,nerr)
       call getfhv('O',omarker,nerr)
       call getfhv('USER1',user(1),nerr)
       call getfhv('USER2',user(2),nerr)
       call getfhv('USER3',user(3),nerr)
       src_depth = evdp
*
      call get_grmodel(modelfile,alpha,beta,rho,th,nl)
*       
      call locate_src(th,nl,src_depth,dh,lsrc)
*
*     a crude convergence fix for sources just beneath a layer boundary
*
*         dh is the depth beneath the interface above the source
*
      if(dh .lt. 0.5 .and. lsrc .ne. 1) then
         src_depth = src_depth - 0.5
         if(src_depth .gt. 0.0)then
            call locate_src(th,nl,src_depth,dh,lsrc)
         else
            src_depth = src_depth + 0.5
         end if
      end if
*
*     compute the travel times of the direct wave
*
      call get_direct(alpha,th,nl,dh,lsrc,range,t,err)
*
      if(err .eq. 0) then
          tmin = t
      else
           write(stdout,*)'Error Computing the direct wave.'
      end if
*
*     compute head-wave travel times
*
      nlm1 = nl - 1
      do 100 lyr = 1, nlm1
        if(lyr .lt. lsrc) go to 100
        call get_refraction(alpha,th,nl,dh,lsrc,range,t,lyr,err)  
        if(t .gt. 0 .and. err .eq. 0) then  
          if(t .lt. tmin) tmin = t
        end if
100   continue
*  
*     set the headers in the green's functions
*   
      atime = tmin + omarker
      if(tmin .lt. 1e6)then
         do 200 i = 1, 8
           infile(1:nlen+7) = gftn_name(1:nlen)//suf(i)      
*           call newhdr   
           call rsac1(infile,seis,npts,btime,dt,mxpts,nerr)
           call setfhv('A',atime,nerr)
           call setfhv('DELTA',dt,nerr)
           call setfhv('B',btime,nerr)
           call setlhv('LEVEN',.TRUE.,nerr)
           call setihv('IFTYPE','ITIME',nerr)
           call setnhv('NPTS',npts,nerr)
           call setfhv('DIST',range,nerr)
           call setfhv('EVDP',evdp,nerr)
           call setfhv('O',omarker,nerr)
           call setfhv('USER1',user(1),nerr)
           call setfhv('USER2',user(2),nerr)
           call setfhv('USER3',user(3),nerr)
           call wsac0(infile,xdummy,seis,nerr)
200      continue
      end if
*
      stop
*     
      end
*
*     subroutine to compute the refraction travel time
*       through a 1D model from a src to a surface
*       receiver
*     
*     vel = velocity
*     th = layer thicknesses
*     nl = number of layers
*     dh = source depth within layer lsrc
*     r = receiver range
*     t = refraction travel time
*     lyr = index of the layer beneath which the wave travels
*
*     if no head wave occurs for this layer set t < 0
*     if lyr = nl no head wave possible - layer nl is
*       a half-space set t < 0, again
*
*     
      subroutine get_refraction(vel,th,nl,dh,lsrc,r,t,lyr,err) 
      real vel(nl),th(nl),r,t
      integer nl,lyr,lsrc, err
      integer stdin,stdout, stderr
      real ray_p,ray_p2,dummy
      integer im1,ip1,j
      
      stdin = 5
      stdout = 6
      stderr = 6
      
      err = 0
      
      if(nl .eq. lyr)then
        err = -1
        t = -100
      end if
      
      if(lsrc .gt. nl)then
         err = -1
         t = -100
         return
      end if  
      
      if(vel(lyr) .ge. vel(lyr+1))then
        t = -100
        return
      end if
*
*     end of loop for src depth
*
20    continue
*
*     make sure the refractor is beneath the src
*
      if(lsrc .gt. lyr)then
         t = -100
         return
      end if     
*
      t = 0.0
      ray_p = 1.0 / vel(lyr+1)
      ray_p2 = ray_p* ray_p
*      
*      compute the time above the source layer
*       travels through the layer once
*
      im1 = lsrc - 1
      do 30 j = 1, im1
        dummy = 1.0/(vel(j)*vel(j))
        dummy = dummy - ray_p2
        t = t + sqrt(dummy) * th(j)
30    continue
*
*     add in the src layer time
*
      dummy = 1.0/(vel(lsrc)*vel(lsrc))
      dummy = dummy - ray_p2
      t = t + sqrt(dummy) * (2.0*th(lsrc) - dh)
*
*     add the time below the src
*      
      ip1 = lsrc + 1
      do 40 j = ip1,lyr
        dummy = 1.0/(vel(j)*vel(j))
        dummy = dummy - ray_p2
        t = t + 2.0 * sqrt(dummy) * th(j)
40    continue
*
*     add in the horizontal phase delay
*
      t = t + ray_p * r
*
      return
      end


*
*     subroutine to locate the lyr containing the source
*      and the depth of the source within that layer
*
      subroutine locate_src(th,nl,src_dpth,dh,lsrc)
      real th(nl),src_dpth,dh
      integer nl,lsrc
      real depth
      integer stdin, stdout, stderr
*      
      stdin = 5
      stdout = 6
      stderr = 6
      
*     find layer containing the src
*
      depth = 0.0
*     depth is be the bottom of each layer
*
      lsrc = 1
10    depth = depth + th(lsrc)
      if(depth .gt. src_dpth) go to 20      
      lsrc = lsrc + 1
      if(lsrc .gt. nl)then
         write(stderr,*) 'Src below model.'
         return
      end if      
      go to 10
*
*     end of loop for src depth
*
20    continue
*     compute the depth of the src in the ith layer
*
      dh = th(lsrc) - (depth - src_dpth)

      return
      end
*
*     subroutine to compute the direct wave travel time
*
      
      subroutine get_direct(vel,th,nl,dh,lsrc,r,t,err) 
      
      real vel(nl),th(nl),r,t
      integer nl,lsrc,err
      integer stdin,stdout, stderr
      integer j, i, itry
      real pl,ph,dp, tol, eta, p, deg_to_rad
      real x, xl,xh, angle, xold, tmp
      
      stdin = 5
      stdout = 6
      stderr = 6
      
      err = 0
      
      deg_to_rad = 0.017453292
      
      tol = 0.001 * r
      itry = 0
*
      j = lsrc - 1
*
*     Check to see if we only have one layer
*
      if(j .eq. 0) then
         t = sqrt(r*r + dh*dh ) / vel(lsrc)
         return
      end if
*
*     start with a vertically incident
*
      pl = 0.0
      ph = sin(deg_to_rad * 89.999) / vel(lsrc)
      dp = ph - pl
            
      angle = asin(pl * vel(lsrc))
      xl = dh * tan(angle)
      
      do 10 i = j,1,-1
        angle = asin(pl * vel(lsrc))
        xl = xl + th(i) * tan(angle)
10    continue
*
*
      x = xl
      if(abs(xl - r) .le. tol) go to 500
*
*     check for horizontal propagation
*
      angle = asin(ph * vel(lsrc))
      xh = dh * tan(angle)
      do 20 i = j,1,-1
        angle = asin(ph * vel(lsrc))
        xh = xh + th(i) * tan(angle)
20    continue
*
      x = xh
      if(abs(xh - r) .le. tol) go to 500
*
*     Now start bisection search at 45 degree take-off angle
*
1     continue
*
      p = 0.5*(ph + pl)

      angle = asin(p * vel(lsrc))
      x = dh * tan(angle)
      do 30 i = j,1,-1
        tmp = p*vel(i)
*
*         check to see if you have an upside down head wave
*             from a LVZ
*
        if(tmp .gt. 1.0) then
          x = 1.1*r
          goto 35
        end if
*
        angle = asin(tmp)
        x = x + th(i) * tan(angle)
30    continue

      if(xold .eq. x) go to 998
      xold = x

35    continue

      itry = itry + 1

      if(abs(x - r) .le. tol) go to 500
      if(itry .gt. 1000) go to 999

      if(x .gt. r) then
        ph = p
      end if
      if(x .lt. r) then
        pl = p
      end if
*
      go to 1
*
500   continue
*
*     compute the time
*
      eta = sqrt(1/(vel(lsrc)*vel(lsrc)) - p*p)
      t = dh * eta + p * x
      do 40 i = j,1,-1
        eta = sqrt(1/(vel(i)*vel(i)) - p*p)
        t = t + th(i) * eta
40    continue
*
      return
*
*     the special case when the root is missed by bisection
*
998   write(stdout,*) 'x = xold, accuracy problem; using x = ',x
      eta = sqrt(1/(vel(lsrc)*vel(lsrc)) - p*p)
      t = dh * eta + p * x
      do 50 i = j,1,-1
        eta = sqrt(1/(vel(i)*vel(i)) - p*p)
        t = t + th(i) * eta
50    continue
      return
*
*     No convergence
*
999   write(stdout,*) 'Direct wave not found in 1000 trys.'
      err = -1
      t = -1
      return
      
      end



*
*
*     Read in a velocity model from an mijkennett input file
*
*      
      subroutine get_grmodel(name,pvel,svel,dens,th,n)
*
      character*(*) name
      real pvel(n),svel(n),dens(n),th(n)
      integer n, nlyrs, iunit,ounit,stdin,stdout
      integer nlen,lnblnk
*     
      iunit = 10
      ounit = 11
      stdin = 5
      stdout = 6
*     
      nlen = lnblnk(name)
      open(unit = iunit,file=name)
*     
      read(iunit,*,err=99) nlyrs
*
*     read in each line of the velocity model
*      
      do 1, n = 1,nlyrs
       read(iunit,*,err=99) th(n),pvel(n),svel(n),dens(n)
1     continue
*
      write(stdout,*) 'Read in ',nlyrs,' layers from ',name(1:nlen)
*     
      return

*      
99    continue
      
      write(stdout,*)'Problem reading file ',name(1:nlen),'.'

      stop

      end
      
