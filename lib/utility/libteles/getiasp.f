	subroutine readiasp(lun)
c reads iaspei travel-time curves for P,S

	common /iasptabl/delmin,ddel,ndel,ndepth,depth(15),
     -		ptime(181,15),stime(181,15),dpdd(181,15),
     -		dpdz(181,15),dsdd(181,15),dsdz(181,15)
	character*10 antenv
	character*80 antval
        character*100 fname
	antenv="ANTELOPE"
	call getenv(antenv,antval)
        tkb = index(antval,' ') - 1
        fname = antval(1:tkb)//"/"//"teles/iaspei.time2"

	open(lun,file=fname)
	read(lun,*) delmin,ddel,ndel,ndepth
	read(lun,*) (depth(i),i=1,ndepth)
	do 1 j=1,ndepth
	  read(lun,1001) (ptime(i,j),dpdd(i,j),dpdz(i,j),
     -		stime(i,j),dsdd(i,j),dsdz(i,j),i=1,ndel)
1	continue
1001	  format(4(f8.2,f8.3,f8.5))

	close (lun)
	return
	end

	subroutine getiasp(del,dep,pt,st,dpdx,dpddep,dsdx,dsddep)
c  interpolates iaspei table to get times at del,dep and derivatives
c  also, numerically calculate derivatives w/distance
c  d.dx is in sec/deg, d.dz id in sec/km; del in deg, dep in km

	common /iasptabl/delmin,ddel,ndel,ndepth,depth(15),
     -		ptime(181,15),stime(181,15),dpdd(181,15),
     -		dpdz(181,15),dsdd(181,15),dsdz(181,15)
	data eps/.001/

c -- get indices
	dd = del
	if (del.gt.180.) dd = 360.-del
	if (float(int(dd)).eq.dd) dd = dd+eps
	if (dd.ge.180.) dd = 180.-eps
	id = int(dd)
	if (id.le.0) id =1
	if (id.ge.181) id = 180
	delx = 1.
	dd1 = dd - float(id)

	zz = dep
	do i=1,ndepth
	  if (zz.eq.depth(i)) zz = zz + eps
	  if (depth(i).lt.zz) iz = i
	end do
	if (zz.lt.depth(1)) iz = 1
	if (iz.ge.ndepth) iz = ndepth-1
	delz = depth(iz+1)-depth(iz)
	dz1 = (zz - depth(iz))/delz

c interpolate

c P:	
	A = ptime(id,iz)
	B = (ptime(id+1,iz) - A)
	C = (ptime(id,iz+1) - A)
	D = (ptime(id+1,iz+1) - A) - B - C
	pt = A + (B*dd1 + (C + D*dd1)*dz1)

	A = dpdd(id,iz)
	B = (dpdd(id+1,iz) - A)
	C = (dpdd(id,iz+1) - A)
	D = (dpdd(id+1,iz+1) - A) - B - C
	dpdx = A + (B*dd1 + (C + D*dd1)*dz1)

	A = dpdz(id,iz)
	B = (dpdz(id+1,iz) - A)
	C = (dpdz(id,iz+1) - A)
	D = (dpdz(id+1,iz+1) - A) - B - C
	dpddep = A + (B*dd1 + (C + D*dd1)*dz1)
c S:	
	A = stime(id,iz)
	B = (stime(id+1,iz) - A)
	C = (stime(id,iz+1) - A)
	D = (stime(id+1,iz+1) - A) - B - C
	st = A + (B*dd1 + (C + D*dd1)*dz1)

	A = dsdd(id,iz)
	B = (dsdd(id+1,iz) - A)
	C = (dsdd(id,iz+1) - A)
	D = (dsdd(id+1,iz+1) - A) - B - C
	dsdx = A + (B*dd1 + (C + D*dd1)*dz1)

	A = dsdz(id,iz)
	B = (dsdz(id+1,iz) - A)
	C = (dsdz(id,iz+1) - A)
	D = (dsdz(id+1,iz+1) - A) - B - C
	dsddep = A + (B*dd1 + (C + D*dd1)*dz1)

	return
	end
