	subroutine readpf(fname, vpm, vsm, rhom, vpb,vsb,rhob,
     -		vpvs, vpvssed) 
	parameter (NLMX=5)
	character*(*) fname
	real*8 vpm, vsm, rhom
	character*80 aline, atype
	data lun/21/
	open (lun, FILE=fname(1:lnblnk(fname)), STATUS="OLD")
1	continue
	read(lun,'(a80)',end=9) aline
	if (aline(1:1).eq."#") go to 1

	call rmleading(aline)
	lb=lnblnk(aline)
	if (lb.lt.3) go to 1  
	do i=1,lb
	  if (aline(i:i).eq."\t") aline(i:i)=" "
	  if (aline(i:i).eq.",") aline(i:i)=" "
	end do
	ix=index(aline," ")
	if (ix.le.1)  go to 1
	atype = aline(1:ix-1)

	la=lnblnk(atype)
	atype(la+1:la+1) = "\0"
	if (atype(1:3).eq."vpm") then
	  read(aline(ix+1:lb),*) vpm

	else if (atype(1:3).eq."vsm") then
	  read(aline(ix+1:lb),*) vsm

	else if (atype(1:4).eq."rhom") then
	  read(aline(ix+1:lb),*) rhom

	elseif (atype(1:3).eq."vpb") then
	  read(aline(ix+1:lb),*) vpb

	else if (atype(1:3).eq."vsb") then
	  read(aline(ix+1:lb),*) vsb
	  vpvssed = vpb/vsb

	else if (atype(1:4).eq."rhob") then
	  read(aline(ix+1:lb),*) rhob

	else if (atype(1:4).eq."vpvs") then
	  read(aline(ix+1:lb),*) vpvs

	else if (atype(1:7).eq."vpvssed") then
	  read(aline(ix+1:lb),*) vpvssed
	  vsb = vpb/vpvssed


	endif
	go to 1

9	continue
	close(lun)
	return
	end

	subroutine rmleading(aline)
c remove leading blanks
	character*(*) aline

2	continue
	lb = lnblnk(aline)
	if (lb.lt.3) go to 3
	do i=1,lb
	  if (aline(i:i).eq." ") then
	      aline(j:lb-1) = aline(j+1:lb)
	    go to 2
	  else
	    go to 3
	  endif
	end do
3	continue
	return
	end
