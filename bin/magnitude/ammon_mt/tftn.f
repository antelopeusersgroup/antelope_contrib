      program tftn
*
*     f77 -g -o tftn tftn.f -lsac
*
*     program to convolve a "unit area"
*       trapezoid into a sac file
*
*     the trapezoid rise time, center duration, fall time
*       are input.  The total duration is
*         rise + cduration + fall
*
*     For BOX CARS, use rise = 0, fall = 0,
*       the specified cduration corresponds to the total width,
*       the time function is not flat all the way across, it is flat
*       for cduration - 2 dt 
*       (you cannot represent the zero rise fall with a finite sample rate)
*
*     UNIT AREA:  by unit area i mean that if you integrate the area under
*      the curve, you get unity, however, you'll notice that this means that 
*      the area of the time function is 1/dt (so that when you integrate the trace
*      in the time domain you get unity).
*
*     To check, generate a unit-area delta function and convolve in the trapezoid
*       of you choice, the output will be a unit area time function.
*
*
*     author:  Chuck Ammon, UC Santa Cruz
*     date:    March 13, 1993
*
****************************************************************************************   

      integer mxpts, mxpts_tftn
      parameter (mxpts = 4096, mxpts_tftn = 1024)
      real x(mxpts),y(mxpts),f(mxpts_tftn)
      character*256 infile, ofile
      real rise,fall,cduration,dt,dummy
      integer stdin, stdout, npts, nerr, nf, lag
      
      stdin = 5
      stdout = 6
      
      write(stdout,*)'What is the sac file name?'
      read(stdin,*) infile
      write(stdout,*)'What is the output sac file name?'
      read(stdin,*) ofile
      
      call zerov(x,mxpts)
      call newhdr
      call rsac1(infile,x,npts,begin,dt,mxpts,nerr)
      if(nerr .gt. 0) then
         write(stdout,*)'Problem reading file.'
      end if
      write(stdout,*) ' '      
      write(stdout,*) 'Time increment = ',dt
      write(stdout,*) 'npts = ',npts
      write(stdout,*) ' '      
      
      write(stdout,*)'Enter rise,cen width,fall time of trap.'
      read(stdin,*) rise,cduration,fall
      
      call zerov(f,mxtpts_tftn)
      call make_trap(lag, rise,cduration,fall,f,mxpts_tftn,dt,nf)
      write(stdout,*) ' '      
      write(stdout,*) 'npts in trapezoid = ',nf
      write(stdout,*) ' '   
            
      call zerov(y,mxpts)      
      call td_convolve(x,f,y,npts,nf)
      
*      call wsac1(ofile,y,npts,begin,dt,nerr)
      call wsac0(ofile,dummy,y,nerr)
      
      call wsac1("trapezoid",f,nf,0.0,dt,nerr)
      
      stop
      end
*
*     end of main
*
*
******************************************************************************
*
      subroutine make_trap(lag,rise,cduration,fall,trap,mxpts,dt,ntotal)
*
*      routine returns a trapezoidal time series - thus can represent
*        a trapezoid, triangle, or box-car
*
*      lag is an integer pointer to the first element to begin this trapezoid
*
*      rise, cduration, fall are the rise, central duration, 
*         and fall times of the trap
*      mxpts maximum number of pts in the time funciton array trap
*      ntotal = total number of points in the time function
*
*******************************************************************************
*
      real rise,cduration,fall, trap(mxpts),dt
      integer nr,nd,nf, mxpts, ntotal, ipoint,n
      integer stdin, stdout
      real slope, area
      
      stdin = 5
      stdout = 6
      
      if(rise .lt. 0 .or. fall .lt. 0 .or. cduration .lt. 0)then
        write(stdout,*) "Trapezoid width parameter < 0, quitting."
        stop
      end if
*
*     set up the index limits
*      
      nr = (rise / dt + 0.5)
      nd = (cduration / dt + 0.5)
      nd = nd + 1
*      if(nd .ne. 0) nd = nd + 1
      nf = (fall / dt + 0.5)
*
*     handle the special case of one-sided triangles
*
      if(nr .gt. 0 .and. nf .eq. 0 .and. nd .eq. 0)then
         nr = nr + 1
      end if
      if(nr .eq. 0 .and. nf .gt. 0 .and. nd .eq. 0)then
         nf = nf + 1
      end if
*
      ntotal = nr + nd + nf + lag + 1
*            
      if(ntotal .gt. mxpts) then
         write(stdout,*) 'Trapezoid too wide, quitting.'
         stop
      end if
*
*     ipoint is a "pointer" to the index of the trapezoid
*
      ipoint = lag + 1
      if(nr .eq. 0) then
        trap(ipoint) = 0
        ipoint = ipoint + 1
      end if
*
*     the initial rise
*      
*     special case of a 2 dt wide triangle
*
      if(nr .eq. 1 .and. nf .eq. 1 .and. nd .eq. 0)then
        trap(ipoint) = 0.0
        ipoint = ipoint + 1
        trap(ipoint) = 1.0 / (dt * dt)
        ipoint = ipoint + 1
        trap(ipoint) = 0.0
        ntotal = 3
        return
       end if
*
*     end of the special case
*       
*     the trap will have maximum amp of one, for now
*
      if(nr .gt. 0) slope = 1 / real(nr)
      do 1 i = 1, nr
        trap(ipoint) = (i-1) * slope
        ipoint = ipoint + 1
1     continue
*
*     the flat part
*
      do 2 i = 1, nd
        trap(ipoint) = 1
        ipoint = ipoint + 1
2     continue
*
*     the fall-off
*
      if(nf .gt.0) slope = -1 / real(nf)
      do 3 i = 1, nf
        trap(ipoint) = 1.0 + slope * real(i)
        ipoint = ipoint + 1
3     continue
*
*     normalize the trapezoid to unit area
*       note that no rise or fall can be less than dt
*
*     the area is the sum of the area of
*     two triangles and a rectangle
*
      fall = nf * dt
      rise = nr * dt
      cduration = nd * dt
      write(stdout,*) "nr,nd,nf = ", nr,nd,nf
*
      if(rise .eq. 0 .and. fall .gt. 0) then
        if(cduration .gt. 0) then
          area = (0.5 * (dt + fall) + (cduration - dt)) / dt
        else
          area = (0.5 * fall ) / dt
        end if
      else if(rise .gt. 0 .and. fall .eq. 0) then
        if(cduration .gt. 0) then
          area = (0.5 * (rise + dt) + (cduration - dt)) / dt
        else
          area = (0.5 * rise ) / dt
        end if
      else if(rise .eq. 0 .and. fall .eq. 0 )then
        area = (0.5 * (dt + dt) + (cduration - dt)) / dt
      else
        area = (0.5 * (rise + fall) + (cduration - dt)) / dt
      end if
*
      n = ntotal 
      j = lag + 1        
      do 4 i = j, n
        trap(i) = trap(i) / area
4     continue
*
      return
      end
*
**********************************************************************
*
      subroutine td_convolve(x,f,y,nx,nf)
*
*     time-domain convolution
*
*     return y = x * f
*
      real x(*),f(*),y(*)
      integer i,j,ii,nx,nf
      real sum
c
c     convolve the filter response and the input trace
c
      do 21 i=1,nx
        sum = 0.0
        do 20 j=1,nf
          ii = i - j + 1
          if(ii .gt. 0) y(i) = y(i) + x(ii)*f(j)
20     continue
21     continue

       return
       end
*
******************************************************************
*
      subroutine zerov(x,n)
      realx(n)
      integer n, i
      
      do 1 i = 1,n
        x(i) = 0.0
1     continue
      return
      end
