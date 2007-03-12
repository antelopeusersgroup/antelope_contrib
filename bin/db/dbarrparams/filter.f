

c
c*******************************************************************************
c
c    FORTRAN subroutine filter
c
c*******************************************************************************
c
      subroutine filter (np, t0, dt, din, tsout, twout, ifilt,
     +                   param1, param2, param3, param4, param5,
     +                   npomax, npout, t0out, dout)
c
      integer            np
      real*4                 t0
      real*4                     dt
      real*4                         din(np)
      real*4                              tsout
      real*4                                     twout
      integer                                           ifilt
      real*4             param1
      real*4                     param2
      real*4                             param3
      real*4                                     param4
      real*4                                             param5
      integer            npomax
      integer                    npout
      real*4                            t0out
      real*4                                   dout(npomax)
c
c    Subroutine filter will filter data.
c
c    Inputs -	np	= The number of input data points.
c		t0	= The input data start time.
c		dt	= The input data time sampling increment.
c		din(np)	= The input data samples.
c		tsout	= The start time of the desired output filtered data.
c			  The filtering will start at t0, but the output
c			  filtered data will not be saved until time tsout.
c			  This allows filtering transients to be mitigated.
c		twout	= The output filtered data time duration.
c		ifilt	= Filter type.
c			  = 1 - Minimum phase bandpass butterworth.
c				param1 = The lower butterworth filter corner 
c					 frequency in hz. If 0, then the filter
c					 is low pass.
c				param2 = The order number of the lower cutoff.
c				param3 = The upper butterworth filter corner 
c					 frequency in hz. If 0, then the filter
c					 is high pass.
c				param4 = The order number of the upper cutoff.
c				param5 = Unused.
c			  = 2 - Zero phase bandpass butterworth. Parameters
c				same as for ifilt = 1.
c		param1,param2,param3,param4,param5
c			= Filter parameters according to ifilt.
c		npomax	= Maximum dimension of dout array.
c
c    Outputs -	npout	= Actual number of filtered output samples.
c		t0out	= Start time of first output sample.
c		dout(npout)
c			= Output filtered sample values.
c
      if (ifilt .eq. 1) then
	npad = (tsout - t0) / dt + 0.5
	npo = twout / dt + 0.5
	is = npad + 1
	ie = is + npo + 1
	if (is .lt. 1) is = 1
	if (ie .gt. np) ie = np
	npo = ie - is + 1
	if (npo .lt. 1) then
	  npout = 0
	  return
	end if
	t0out = t0 + npad*dt
	i1 = param2 + 0.5
	i2 = param4 + 0.5
	call butfil (dt, din, npad, npo, param1, i1, param3, i2, dout)
	npout = npo
      else if (ifilt .eq. 2) then
	npad = (tsout - t0) / dt + 0.5
	nnpo = twout / dt + 1.5
	npo = nnpo + npad
	is = npad + 1
	ie = is + npo + 1
	if (is .lt. 1) is = 1
	if (ie .gt. np) ie = np
	npo = ie - is + 1
	if (npo .lt. 1) then
	  npout = 0
	  return
	end if
	t0out = t0 + npad*dt
	i1 = param2 + 0.5
	i2 = param4 + 0.5
	call butfil (dt, din, npad, npo, param1, i1, param3, i2, dout)
	do 100  i = 1, npo/2
	  j = npo - i + 1
	  tmp = dout(i)
	  dout(i) = dout(j)
	  dout(j) = tmp
  100   continue
	npad = npo - nnpo
	if (npad .lt. 1) npad = 0
	npo = npo - npad
	call butfil (dt, dout, npad, npo, param1, i1, param3, i2, dout)
	do 110  i = 1, npo/2
	  j = npo - i + 1
	  tmp = dout(i)
	  dout(i) = dout(j)
	  dout(j) = tmp
  110   continue
	npout = npo
      else
	write (6, '(a)') 'filter: Unknown filter type.'
      end if
      return
      end
c
c*******************************************************************************
c
c    FORTRAN subroutine butfil
c
c*******************************************************************************
c
      subroutine butfil (dt, din, npad, nout, fl, ol, fu, ou,
     +                   dout)
c
      real*4             dt
      real*4                 din(1)
      integer                     npad
      integer                           nout
      real*4                                  fl
      integer                                     ol
      real*4                                          fu
      integer                                             ou
      real*4             dout(nout)
c
c    Subroutine butfil will apply a minimum phase bandpass butterworth
c    filter to a sampled data array.
c
c    Inputs -	dt	= The data time sampling increment.
c		din()	= The input data.
c		npad	= The number of startup padding samples. The filter
c			  is started with the first input sample but the
c			  output data array is not started until npad points
c			  have been filtered. This is intended to minimize
c			  filter startup transients.
c		nout	= The desired number of output filtered data samples.
c		fl	= The lower butterworth filter corner frequency in
c			  hz. If 0, then the filter is low pass.
c		ol	= The order number of the lower cutoff.
c		fu	= The upper butterworth filter corner frequency in
c			  hz. If 0, then the filter is high pass.
c		ou	= The order number of the upper cutoff.
c
c    Output -	dout(nout)
c			= The output filtered data. The first sample
c			  corresponds to time dt*npad relative to the first
c			  input sample in din(1).
c
      call setbfl (fl, ol, fu, ou, dt)
      call inifil (din(1))
      call filrec (npad, din, 0, dum)
      call filrec (nout, din(1+npad), 1, dout)
c
      return
      end

c $Id$ 
