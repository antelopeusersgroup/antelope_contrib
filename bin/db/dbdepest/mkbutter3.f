	subroutine mkbutter3(delt,df,numah,nfilt,fmin,fmax,zfilt)
c  band-pass filter fmin to fmax
c generates 3-pole butterworth filter, BUT: zero phase
c  filters both ways: Multiplies filter by its complex congjgate
C output is:
C	zfilt  complex freq-domain transfer function
C	df	frequency spacing
C	nfilt	=2^l, number of points in zfilt
c  GAA 
	complex zfilt(*)

	complex z0, zone
	data si/0.8660254/,co/0.5/
c find pwr of two .ge.numah
	l = 1
1	continue
	    l = l * 2
	    if (l.ge.numah) go to 2
	    go to 1
2	continue
c  make sure at least more than 2x points than data
	nfilt = l * 2
	df = 1./(float(nfilt-1)*delt)

	dfn = df/fmax
	dfn2 =df/fmin
	nfd2 = nfilt/2
	fn = dfn
	fn2 = dfn2
	zone = cmplx(1.,0.)
	do 10 i=2,nfd2
c low-pass stages
	    z0 = cmplx(1.,fn)
	    z0 = z0*cmplx(co,fn + si)*cmplx(co,fn - si)
	    zfilt(i) = zone/(z0*conjg(z0))
c modify for high-pass, also 2x 3pole
	    z0 = cmplx(1.,fn2)
	    z0  = z0*cmplx(co,fn2 + si)*cmplx(co,fn2 - si)
	    zfilt(i) = zfilt(i)*cmplx(fn2**6,0.)/(z0*conjg(z0))
	    zfilt(nfilt-i+2) = zfilt(i)
	    fn = dfn + fn
	    fn2 = dfn2 + fn2
10	continue
	zfilt(1) = cmplx(0.,0.)
	zfilt(nfd2 + 1) = cmplx(0.,0.)
	return
	end
