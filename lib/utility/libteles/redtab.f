	subroutine redtab(lun)
	common /elcor/  ecpd(37), ecp0(37,3), ecp1(37,3), ecp2(37,3),
     +         ecsd(22), ecs0(22,3), ecs1(22,3), ecs2(22,3)
	data nh3/19/,ndt3/71/,nh4/19/,ndt4/38/,nh5/19/,ndt5/23/

c**************************************************************
c										    
c     read file of ellipticity corrections
c										    
        character*10 antenv
        character*80 antval
        character*100 fname
        call getenv(antenv,antval)
	antenv="ANTELOPE"
        fname = antval//"teles/elpcor"

	open (lun,file='/etna/home/abers/src/R3D90/elpcor')
	read(lun,fmt='(//)')
130	format ( 2x, f5.0, 9f7.2 )
	do 140 i = 1,22
140	read (lun,130) ecpd(i),(ecp0(i,j),j=1,3),
     +       (ecp1(i,j),j=1,3),(ecp2(i,j),j=1,3)
	read (lun,fmt='(/)')
	do 150 i = 1,15
150	read (lun,130) ecpd(i+22),(ecp0(i+22,j),j=1,3),
     +       (ecp1(i+22,j),j=1,3),(ecp2(i+22,j),j=1,3)
	read (lun,fmt='(/)')
	do 160 i = 1,8
160	read (lun,130) 
	read (lun,fmt='(/)')
	do 170 i = 1,6
170	read (lun,130)
	read (lun,fmt='(/)') 
	do 180 i = 1,19
180	read (lun,130)
	read (lun,fmt='(/)') 
	do 190 i = 1,22
190	read (lun,130) ecsd(i),(ecs0(i,j),j=1,3),
     +       (ecs1(i,j),j=1,3),(ecs2(i,j),j=1,3)
	close (lun)
	return
	end

