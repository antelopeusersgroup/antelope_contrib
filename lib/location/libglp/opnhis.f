      subroutine opnhis(datfile,lun,neq,nstat,lastrec,latorg,longorg) 
c-------------------------------------------------------------------- 
c         opens and positions data history file associated with 
c  data file named datfile.  if history file does not exist it is 
c  created and header records are written.  the file name for the 
c  history file is generated from the data file name.  the name 
c  created is the same as the data file but with extension 'his'. 
c         when the history file already exists a consistency check
c  is made with the variables neq and nstat (see below) passed to 
c  the routine.  if they are inconsistent a fatal error results 
c  to avoid scrambling the history file as its structure is 
c  locked to the size of these variables.  (see documentation for 
c  pmel.) 
c 
c  arguments- 
c    datfile - character string giving pe os/32 file descriptor of
c              data file.  history file name is created from this 
c              name.  
c    lun     - logical unit to be associated with history file. 
c    neq     - number of earthquakes in data file.  
c    nstat   - number of stations in station table on data file.
c    lastrec - total number of records currently on the history file. 
c              in creation mode lastrec will be set to 2.  otherwise
c              it will be some larger number. 
c    latorg,longorg - latitude and longitude of local cartesian 
c              coordinate system origin.  (deg,min,seconds format)
c              note   these are real arrays.
c 
c  author 
c    gary l. pavlis 
c    geophysics program ak-50 
c    university of washington 
c    seattle, wa  98195 
c 
c  written   december 1982
c-------------------------------------------------------------------- 
      character*(*) datfile 
      integer lun,neq,nstat,lastrec 
      real latorg(3),longorg(3) 
c--used in inquire
      logical exchec  
c+++++++++old code for perkin elmer os/32++++++++++++++ 
c--volume, file name, and extension parsed from datfile 
      character*4 vol 
      character*8 fname 
      character*3 ext,hisext
c--extension used to denote history files 
      parameter(hisext='his') 
c--holds file name generated for history file 
      character*17 hisfile
c++++++++end perkin elmer code+++++++++++++++ 
c+++++++++++++++changed for cdc 10/2/83++++++++++++++++ 
c     character*(*) hisext
c     parameter(hisext='his') 
c     character*7 hisfile 
c     character*7 mfnwa 
c     external mfnwa  
c+++++++++++++++++++++++++++++++++++++++++++++++++++
c--string pointers
      integer iv,iname 
c--used in comparison with neq and nstat dummy arguments
      integer nstatin,neqin 
c++++++++++old code for perkin elmer os/32+++++++++++++++++ 
c--first derive the file name for the history file from datfile 
      call parsefd(datfile,vol,fname,ext) 
      iv = lenstr(vol)
      iname = lenstr(fname)
      if(iv.eq.0) then
            hisfile = fname(1:iname)//'.'//hisext
      else  
            hisfile = vol(1:iv)//' '//fname(1:iname)//'.'//hisext
      endif 
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c--replacement of above for cdc systems 
c     hisfile = mfnwa(hisfile,hisext) 
c--now find out if hisfile exists or not and then open or create it.
      inquire(file=hisfile,exist=exchec)
      if(exchec) then 
            open(lun,file=hisfile,form='formatted',status='old',
     $               access='direct',recl=80) 
            print *,'history file ->',hisfile,' now open' 
            read(lun,'(2i5)',rec=1) neqin,nstatin 
            if((neqin.ne.neq).or.(nstatin.ne.nstat)) then 
                  print *,'fatal error(opnhis)   inconsistent data' 
                  print *,datfile,' has ',neq,' events and ',nstat, 
     $                              ' stations.'
                  print *,hisfile,' has ',neqin,' events and ',nstatin, 
     $                              ' stations.'
                  stop
            endif 
            read(lun,1010,rec=2) lastrec  
            print *,'appending from recond number ',lastrec 
      else  
            open(lun,file=hisfile,form='formatted',status='new',
     $               access='direct',recl=80) 
            print *,'creating history file ->',hisfile
            lastrec = 2 
            write(lun,1000,rec=1) neq,nstat,latorg,longorg
            write(lun,1010,rec=2) lastrec 
      endif 
      return
c--formats for header records 
 1000 format(2i5,2(2f4.0,f6.2,6x))
 1010 format(i10) 
      end 

c $Id$ 
