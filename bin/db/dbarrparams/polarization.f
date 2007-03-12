c Copyright (c) 1993-1996 University of Colorado 
c All rights reserved 


      subroutine polarization(s,buff,nsamp,nsmpm,iwndocntr,lwndo,
     &                        srate,xlpass,xhpass,itype,
     &			      backaz,inc,rect)
c
c Compute and write (to standard output) some polarization attributes,
c  	mainly to estimate the back azimuth of the source.
c The attributes computed depend on the input parameter "itype".
c
c calling sequence
c	s(i,j)	input 3-component signals of dimension nsamp by 3 with  
c				i = 1,nsamp
c				j=1 -> vertical
c				j=2 -> north
c				j=3 -> east 
c	
c	buff		work area of dimension nsamp
c	nsamp		number of samples in 3-component signals
c	iwndocntr	position of center of computation window in samples
c	lwndo		length of computation window in samples
c	srate		sampling rate in samples/sec
c	xlpass		frequency of -3db point for lowpass filter
c	xhpass		frequency of -3db point for highpass filter
c	itype		integer controling types of attributes output;
c				itype=1 -> P wave
c				itype=2 -> Rayleigh wave
c routines required
c	butterfilt.f, eigen.f, others supplied in this file
c
c author
c	Andy Jurkevics, Center for Seismic Studies, Nov 2, 1987
c			

      real s(nsmpm,3), buff(nsamp), srate, xlpass, xhpass
      real backaz, inc, rect
      integer nsamp, iwndocntr, lwndo, itype
      real covar(6),attrib(5)
      real vectors3(3,3), values3(3),vectors2(2,2),values2(2)
      character*10 attribname(10)

	character*130 rcsid

c
      rad	= 180./acos(-1.)
      dt	= 1./srate 
      fn	= 1./(2.*dt)
      fcenter	= .5*(xlpass + xhpass)
      ibgn	= iwndocntr - nint(lwndo/2.)
      if (ibgn.lt.1) ibgn = 1
      if ((ibgn+lwndo).gt.nsamp) lwndo = nsamp-ibgn 
      if (lwndo.lt.10) return
c
c bandpass
c
c     if (xhpass.gt..01*fn.and.xhpass.lt..98*fn) then
c       call buttercoeff(xhpass,-1.,dt,4)
c       do 100 j	= 1,3
c       call butterapply(s(1,j),nsamp,buff,1)
c100    continue
c     endif
c     if (xlpass.lt..99*fn.and.xlpass.gt..02*fn) then
c       call buttercoeff(-1.,xlpass,dt,4)
c       do 120 j	= 1,3
c       call butterapply(s(1,j),nsamp,buff,1)
c120    continue
c     endif
c
c compute covariance matrix in symmetric storage mode
c
      do 150 j	= 1,6
      covar(j)	= 0.0
 150  continue
      call covmtrx(s,nsamp,nsmpm,ibgn,lwndo,0.,covar,diag)
c
c solve 3-d and 2-d eigenproblem
c
      call eigenprob(covar,values3,vectors3,values2,vectors2)
c
c extract attributes from eigensolution
c
c do short period
c
      if (itype.eq.1) then
       call azimuth1(values3,vectors3,attrib(1),attribname(1))
       attrib(1)	= attrib(1) + 180.
       call incidenc1(values3,vectors3,attrib(2),attribname(2))
       call rectilin(values3,vectors3,attrib(3),attribname(3))
       backaz = attrib(1)
       inc = attrib(2)
       rect = attrib(3)
      endif
c
c do long period
c
      if (itype.eq.2.) then
       call planarity(values3,vectors3,attrib(1),attribname(1))
       buff(2) = vectors2(1,1)
       buff(3) = vectors2(2,1)
c
c This radial vector is now +- Rayleigh azimuth; now need
c  the sign from the sense of elliptical motion
c Find the sense of motion of the ellipse
c
       sum       = 0.
       do 550 i  = 2,lwndo
        it      = ibgn+i-1
        sum     = sum
     *   +((s(it,2)*buff(2)+s(it,3)*buff(3))
     *   -(s(it-1,2)*buff(2)+s(it-1,3)*buff(3)))
     *   *(s(it,1)+s(it-1,1))/2. *i/fltaper
 550  continue
c Back azimuth to source in in direction +radial
       if (sum.lt.0.) then
        buff(2)       = -buff(2)
        buff(3)       = -buff(3)
       endif
       attrib(2)  = rad*atan2(buff(3),buff(2)) + 180.
       if (attrib(2).gt.360.) attrib(2) = attrib(2) - 360.
       backaz = attrib(2)
       rect = attrib(1)
c      write(6,6040) 
c6040  format(' backazimuth  planarity  (for Rayleigh motion)')
c      write(6,6050) attrib(2),attrib(1)
c6050  format('   ',f6.1,'      ',f4.1)
      endif
c
      return
      end
c *******************************************************************
      subroutine covmtrx(s,ldimt,nsmpm,ibgn,lwndo,tlag,covar,diag)
c
c Computes the 3x3 covariance matrix for given window  
c
c input:
c s	 	array of 3-comp data s(i,j); i=time
c					     j=comp; 1=z,2=n,3=e	
c ldimt		first dimension of s exactly as in calling program
c ibgn		first time sample of window
c lwndo		number of time samples in window
c tlag		time lag in sample units to shift window
c
c output:
c covar	 	contains 3X3 covariance matrix in symmetric storage mode
c diag	 	sum of diagonal terms	
c routines rquired:
c none 
c
      real s(nsmpm,3) 
      real covar(6),avrg(3)
c
c use linear taper of 1/2 window length
c
      ltaper	= ifix(lwndo/2. +.51)
      fltaper	= float(ltaper)
c
c compute mean of each component
c
      do 250 j	= 1,3
      avrg(j)	= 0.
      do 240 i	= 1,lwndo
      it	= ifix(float(ibgn+i-1) + tlag +.5) 
      avrg(j)	= avrg(j) + s(it,j)
 240  continue
      avrg(j)	= avrg(j)/float(lwndo)
 250  continue
c
c compute covariance matrix in symmetric storage mode 
c
      jk	= 0
      do 390 j	= 1,3
      do 390 k	= 1,j
      jk	= jk + 1
      sum	= 0.
      do 350 i	= 1,ltaper 
        it	= ifix(float(ibgn+i-1) + tlag +.5) 
        sum	= sum + 
     *    (s(it,j)-avrg(j))*(s(it,k)-avrg(k))*i/fltaper
 350  continue
      do 360 i	= ltaper+1,lwndo
        it	= ifix(float(ibgn+i-1) + tlag +.5) 
        sum	= sum + 
     *    (s(it,j)-avrg(j))*(s(it,k)-avrg(k))*(lwndo-i+1)/fltaper
 360  continue
      covar(jk)	= sum/float(lwndo)
 390  continue
c
      diag	= covar(1) + covar(3) + covar(6)
c
      return
      end
c *************************************************************
      subroutine eigenprob(covar,values3,vectors3,values2,vectors2)
c
c Computes the eigenproblem from covariance matrix for both
c 3-d (vertical & horizontals) and 2-d (horizontals only) 
c
c input:
c covar	 	3-d covariance matrix in symmetric storage mode 	
c		covar is preserved in this routine
c
c output:
c values3	3 eigenvalues of 3-d case
c vectors3	3 eigenvectors of 3-d case
c values2	2 eigenvalues of 2-d case
c vectors2	2 eigenvectors of 2-d case
c routines rquired:
c eigen 
c
      real covar(6),buff(6)
      real values3(3),vectors3(3,3),values2(2),vectors2(2,2)
c
c do 3-d case
c copy covariance matrix because eigen destroys original
c
      do 1100 j  = 1,6
      buff(j)   = covar(j)
 1100 continue
      call eigen(buff,vectors3,3,0)
      do 1120 j  = 1,3
      itemp     = j + (j*j-j)/2
      values3(j) = buff(itemp) + buff(1)*1.e-6
 1120 continue
c
c 2-d case
c copy covariance matrix because eigen destroys original
c
      buff(1) = covar(3)
      buff(2) = covar(5)
      buff(3) = covar(6)
      call eigen(buff,vectors2,2,0)
      values2(1) = buff(1)
      values2(2) = buff(3) + buff(1)*1.e-6
c
      return
      end
c ***************************************************************
      subroutine threeamp(values,vectors,attrib,attribname)
c
c Computes attribute
c          three-component amplitude 
c                   from eigenvalues and eigenvectors 
c
c input:
c values	3 eigenvalues ordered such that 1 = largest
c vectors	eigenvectors corresponding to three eigenvalues 
c output:
c attrib	three-component amplitude 
c name 		attribute name 
c routines required:
c none 
c
      real vectors(3,3),values(3)
      character*10 attribname
c
      write(attribname,2001)
 2001 format('3-comp amp')
      attrib	= 0.
      do 100 j	= 1,3
      attrib	= attrib + sqrt(values(j))
 100  continue
c
      return
      end
c **************************************************************
      subroutine rectilin(values,vectors,attrib,attribname)
c
c Computes attribute 
c          rectilinearity 
c                  from eigenvalues and eigenvectors 
c
c input:
c values	3 eigenvalues ordered such that 1 = largest
c vectors	eigenvectors corresponding to three eigenvalues 
c output:
c attrib 	rectilinearity	
c name 		attribute name 
c routines required:
c none 
c
      real vectors(3,3),values(3)
      character*10 attribname
c
      write(attribname,2006)
 2006 format('  rectilin')
      attrib   = (1. - (values(3)+values(2))/(2.*values(1)))**2
c
      return
      end

c **************************************************************
      subroutine azimuth1(values,vectors,attrib,attribname)
c
c Computes attribute
c          azimuth of eigenvector 1
c                   from eigenvalues and eigenvectors 
c
c input:
c values	3 eigenvalues ordered such that 1 = largest
c vectors	eigenvectors corresponding to three eigenvalues 
c output:
c attrib 	azimuth of eigenvector 1	
c name 		attribute name 
c routines required:
c none 
c
      real vectors(3,3),values(3)
      character*10 attribname
      rad	= 180./acos(-1.)
c
      write(attribname,2018)
 2018 format('azimuth1')
      sgn       = sign(1.,vectors(1,1))
      attrib  	= rad*atan2(sgn*vectors(3,1),sgn*vectors(2,1))
c
      return
      end
c **************************************************************
      subroutine incidenc1(values,vectors,attrib,attribname)
c
c Computes attribute
c          incidence of eigenvector 1 (measured from vertical)
c                   from eigenvalues and eigenvectors 
c
c input:
c values	3 eigenvalues ordered such that 1 = largest
c vectors	eigenvectors corresponding to three eigenvalues 
c output:
c attrib 	azimuth of eigenvector 1	
c name 		attribute name 
c routines required:
c none 
c
      real vectors(3,3),values(3)
      character*10 attribname
      rad	= 180./acos(-1.)
c
      write(attribname,2012)
 2012 format('incidence1')
      sgn       = sign(1.,vectors(1,1))
      attrib  = acos(sgn*vectors(1,1))*rad
c
      return
      end
c **************************************************************
      subroutine incidenc3(values,vectors,attrib,attribname)
c
c Computes attribute
c          incidence of eigenvector 3 (measured from vertical)
c                   from eigenvalues and eigenvectors 
c
c input:
c values	3 eigenvalues ordered such that 1 = largest
c vectors	eigenvectors corresponding to three eigenvalues 
c output:
c attrib 	azimuth of eigenvector 3	
c name 		attribute name 
c routines required:
c none 
c
      real vectors(3,3),values(3)
      character*10 attribname
      rad	= 180./acos(-1.)
c
c #14 is incidence of eigenvector 3
c
      write(attribname,2014)
 2014 format('incidence3')
      sgn       = sign(1.,vectors(1,3))
      attrib  	= acos(sgn*vectors(1,3))*rad
c
      return
      end
c **************************************************************
      subroutine planarity(values,vectors,attrib,attribname)
c
c Computes attribute
c          planarity 
c                   from eigenvalues and eigenvectors 
c
c input:
c values	3 eigenvalues ordered such that 1 = largest
c vectors	eigenvectors corresponding to three eigenvalues 
c output:
c attrib  	planarity of polarization ellipsoid	
c name 		attribute name 
c routines required:
c none 
c
      real vectors(3,3),values(3)
      character*10 attribname
      rad	= 180./acos(-1.)
c
c #7 is planarity
c
      write(attribname,2007)
 2007 format('planarity ')
      attrib   = (1. - values(3)/(values(2)))**2
c
      return
      end
c **************************************************************
      subroutine horizvert(covar,attrib,attribname)
c
c Computes attribute
c          horizontal-to-vertical amplitude ratio 
c                   from covariance matrix 
c
c input:
c covar		 3-d covariance matrix in symmetric storage mode
c output:
c attrib  	horizontal-to-vertical amplitude of motion
c name 		attribute name 
c routines required:
c none 
c
      real covar(6)
      character*10 attribname
c
c #9 is horizontal to vertical ratio
c    
      write(attribname,2009)
 2009 format('horiz/vert')
      vert	= covar(1)
      horiz	= (covar(3) + covar(6))/2.
      attrib	= horiz/vert
c
      return
      end
c **************************************************************
      subroutine verthoriz(covar,attrib,attribname)
c
c Computes attribute
c          vertical-to-horizontal amplitude ratio 
c                   from covariance matrix 
c
c input:
c covar		 3-d covariance matrix in symmetric storage mode
c output:
c attrib  	vertical-to-horizontal amplitude of motion
c name 		attribute name 
c routines required:
c none 
c
      real covar(6)
      character*10 attribname
c
c #10 is vertical to vertical ratio
c    
      write(attribname,2010)
 2010 format('vert/horiz')
      vert	= covar(1)
      horiz	= (covar(3) + covar(6))/2.
      attrib	= vert/horiz
c
      return
      end
c **************************************************************
      subroutine hmaxhmin(values,vectors,attrib,attribname)
c
c Computes attribute
c          maximum horizontal to minimum horizontal amp ratio
c                   from 2-d eigenvalues and eigenvectors
c
c input:
c values        2 eigenvalues ordered such that 1 = largest
c vectors       eigenvectors corresponding to two eigenvalues
c output:
c attrib       	max-to-min horizontal amplitude ratios 
c name          attribute name
c routines required:
c none
c
      real vectors(2,2),values(2)
      character*10 attribname(21)
c
c #21 is Hmax/Hmin where H is horizontal
c
      write(attribname(21),2021)
 2021 format('Hmax/Hmin')
      attrib	= sqrt(values(1)/values(2))
c
      return
      end
c *************************************************************
      subroutine azimhmin(values,vectors,attrib,attribname)
c
c Computes attribute
c          azimuth of minimum horizontal eigenvalue  
c                   from 2-d eigenvalues and eigenvectors
c
c input:
c values        2 eigenvalues ordered such that 1 = largest
c vectors       eigenvectors corresponding to two eigenvalues
c output:
c attrib        azimuth of minimum horizontal eigenvalue in degrees
c name          attribute name
c routines required:
c none
c
      real vectors(2,2),values(2)
      character*10 attribname(22)
      rad	= 180./acos(-1.)
c
c #22 is azimuth of Hmin
c
      write(attribname(22),2022)
 2022 format('azim Hmin')
      attrib	= rad*atan2(vectors(2,2),vectors(1,2))
c
      return
      end
c *************************************************************
      subroutine rtrv(covar,values,vectors,baz,rt,rv)
c
c compute Radial/Transverse and Radial/Vertical from 
c covariance matrix 
c
c input:
c covar		3-d covariance matrix in symmetric storage mode
c values	2-d horizontal eigenvalues
c vectors	2-d horizontal eigenvectors
c baz:	 	true back azimuth of event in degrees
c
c output:
c rt	 	R/T (amplitude)	
c rv		R/V (amplitude)
c
      real covar(6),values(2),vectors(2,2)
      character*10 attribname
c
      call hmaxhmin(values,vectors,ratio,attribname)
      call horizvert(covar,hv,attribname)
      vh	= 1./hv
      call azimhmin(values,vectors,azhmin,attribname)
      daz	= baz - azhmin
c
      rad	= 180./acos(-1.)
      theta	= daz/rad
      xr	= sin(theta)
      yr	= cos(theta)
      xt	= cos(theta)
      yt	= sin(theta)
      r		= sqrt(1./(xr**2 + (ratio*yr)**2))
      t		= sqrt(1./(xt**2 + (ratio*yt)**2))
      rt	= r/t
      rv	= sqrt(2./(vh*(1. + 1./rt**2)))
c
      return
      end
