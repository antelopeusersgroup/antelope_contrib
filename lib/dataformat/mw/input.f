      subroutine input(filnam,form,nblock,nskip,npoints,data)
c     include '/usr/local/include/v1'
      character*(*) filnam,form
      character prompt*80
      dimension data(1)
      integer*2 idata(100000) 
      call decode(filnam,fileno,1,nvar)
      if (nvar.gt.0) then
        ifile = fileno
      else
        go to 200
      end if
c
c  read nio files
c
      nhigh = longis(ifile)
      if (( nskip + npoints ) .gt. nhigh ) then
        nup  = nhigh
        npoints = nhigh - nskip 
      else 
        nup = nskip + npoints
      end if
      call realio( data, nskip+1, nup, ifile, 0)
      return
  200 continue
      prompt(1:80) = 'ascii (1) or css2 (0)'
      itemp = 0
      ifiletype = iask(prompt,itemp)
c      ifiletype = isbora(filnam)
c
c  read ascii files 
c
      if (ifiletype.eq.1) then
        write(6,*) 'ascii file'
        open (unit=10,file=filnam,status='old')
        do 210 i=1,nskip
  210     read(10,*)
        do 220 k=1,npoints
  220     read(10,*,end=300)data(k)
  300   npoints=k-1
        close (unit=10)
c
c read css2 files
c
      else if (ifiletype.eq.0) then
        write(6,*) 'css2 file'
        nhigh = longiscss(filnam)
        if (( nskip + npoints) .gt. nhigh) then
          nup  = nhigh
          npoints = nhigh - nskip 
        else 
          nup = nskip + npoints
        end if
        call css2io( idata, nskip+1, nup, filnam, 0)
        do 400 ipoint = 1, npoints
           data(ipoint) = idata(ipoint)
  400   continue
      end if
      return
      end

c $Id$ 
