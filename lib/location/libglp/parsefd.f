      subroutine parsefd(fd,vol,fname,ext)
c---------------------------------------------------------------------
c          decomposes perkin elmer os/32 file descriptor stored 
c  as character string "fd" into volume, root file name, and extension. 
c 
c  arguments- 
c    fd - character string containing file descriptor to be parsed. 
c    vol - volume name extracted from fd. 
c    fname - root file name extracted from fd.
c    ext - extension extracted from fd. 
c 
c  note   if any parts of fd are missing vol, fname, or ext will be 
c         returned blank filled.
c 
c  written   december 1982
c  author 
c   gary l. pavlis
c   geophysics program ak-50
c   university of washington
c   seattle, wa  98195
c---------------------------------------------------------------------
      character*(*) fd
      character*4 vol 
      character*8 fname 
      character*3 ext 
      integer iv,ie 
      vol = '    '
      fname = '        '
      ext = '   ' 
c     iv = index(fd,' ')
      iv = 0
      ie = index(fd,'.')
c
c--Changed for Unix as a simple fix.  Ignore vol and always return it
c--as an empty string
c
c     if(iv.gt.1) vol = fd(1:iv-1)
      if(ie.eq.0) then
           fname = fd(iv+1:)
      else  
           fname = fd(iv+1:ie-1)
           ext = fd(ie+1:)
      endif 
      return
      end 

c $Id$ 
