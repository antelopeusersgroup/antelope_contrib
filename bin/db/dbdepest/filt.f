	subroutine filt(delt,df,numah,nfilt,zfilt,datin,dat)
c filters waveform datin by applying filter zfilt, output is in dat
	dimension datin(*),dat(*)
c clever use of redimensioning
	dimension zfilt(*)
c
	do 10 i=1,numah
10	    dat(i) =datin(i)

	if (nfilt.gt.numah) then
	    do 20 i=numah+1,nfilt+2
20		dat(i) = 0.
	end if

c fft the data, and pray that sign convention is OK, filter and return
	call fast(dat,nfilt)
	do 30 i=1,nfilt+2,2
	    temp = dat(i)*zfilt(i+1) + dat(i+1)*zfilt(i)
	    dat(i) = dat(i) * zfilt(i) - dat(i+1)*zfilt(i+1)
	    dat(i+1) = temp
30	continue
	dat(nfilt/2+1) = 0.
	dat(nfilt/2+2) = 0.
	call fsst (dat,nfilt)
	return
	end
