      subroutine mtrxie(a,n1dim,n2dim,nrowa,ncola,lun,ieof) 
c---------------------------------------------------------------------- 
c       mtrxie and the alternate entry points read and write an array 
c  in coded e format with various options.  (see entry point description
c  below.)  format used is appropriate to cdc computers to prevent
c  loss of significant figures. 
c 
c  arguements-
c 
c      a-two dimensional array  
c      n1dim-first dimension of a in calling program
c      n2dim-second dimension of a in calling program 
c      nrowa-number of rows in a to write 
c            (when reading nrowa is read first) 
c      ncola-number of columns in a to write or read. 
c            (entry point mtrxies will read only this many and skip 
c             remaining columns to maintain file positioning.  if 
c             number of columns actually on the file is less than 
c             this value ncola is reset to the number of columns
c             actually read.) 
c      lun-logical unit number of i/o device
c      ieof.eq.0   read operation was successful
c          .eq.-1  endfile encountered before read operation was
c                  completed.  fatal error. 
c          .eq.10 insufficient storage space available in the calling 
c                  program to read this array.
c                  (ieof is used only in the input mode)
c 
c  entry points - 
c     mtrxie - main entry point.  input mode of this procedure. 
c     mtrxies - selective input mode entry.  in this mode procedure 
c              will only read the first ncola columns of the array. 
c     mtrxoe - outputs array by rows. (fastest) 
c     mtrxtoe - outputs effectively the transpose of the array
c               by writting in the same format as mtrxoe but output 
c               is written by columns instead of rows.  when data 
c               written by this entry point is subsequently read
c               back in by mtrxie the result will be the transpose
c               of the matrix written by mtrxtoe. 
c 
c  note-
c  this subroutine has an important sibling called iotrap that
c  writes a diagnostic and terminates a program if ieof is nonzero. 
c 
c---------------------------------------------------------------------- 
       dimension a(n1dim,n2dim) 
       character skip 
       ieof=0 
       read(lun,1000,end=150) nrowa,ncola 
 1000  format(2i5)
       if((nrowa.gt.n1dim).or.(ncola.gt.n2dim)) go to 200 
       read(lun,1010,end=150) ((a(i,j),i=1,nrowa),j=1,ncola)
 1010  format(4e20.13)
       return 
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
       entry mtrxies(a,n1dim,n2dim,nrowa,ncola,lun,ieof)
       read(lun,1000,end=150) nrowa,ncolin
       if(ncolin.lt.ncola) ncola = ncolin 
       if((nrowa.gt.n1dim).or.(ncola.gt.n2dim)) go to 200 
       read(lun,1010,end=150) ((a(i,j),i=1,nrowa),j=1,ncola)
c--note - every use of the magic number 4 below is imtimately 
c         connected to the 4 in format 1010.  if that format is 
c         changed the 4's must be changed below also. 
c 
c--nread is the number of data values read
c--this assumes record oriented i/o in the sense that if the  
c--program reads only one number from a line, the file is left
c--positioned after that line.  
      if(mod(nrowa*ncola,4).eq.0) then
            nread = nrowa*ncola 
      else  
            nread = nrowa*ncola + 4 - mod(nrowa*ncola,4)
      endif 
c--nleft is the number of elements left on the file 
c--nrleft is the number of records left that are to be skipped
c--for proper file positioning. 
       nleft = nrowa*ncolin - nread 
       if(mod(nleft,4).eq.0) then 
             nrleft = nleft/4 
       else 
             nrleft = nleft/4 + 1 
       endif
       do 100 i=1,nrleft
             read(lun,'(a)') skip 
  100  continue 
       return 
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c--these statements are error exits for input.
  150  ieof=-1
       return 
  200  ieof=10
       return 
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       entry mtrxoe(a,n1dim,n2dim,nrowa,ncola,lun,ieof) 
       write(lun,1000) nrowa,ncola
       write(lun,1010) ((a(i,j),i=1,nrowa),j=1,ncola) 
       return 
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c--this entry point outputs the transpose of the given matrix.
       entry mtrxtoe(a,n1dim,n2dim,nrowa,ncola,lun,ieof)
       write(lun,1000) ncola,nrowa
       write(lun,1010) ((a(i,j),j=1,ncola),i=1,nrowa) 
       return 
       end  

c $Id$ 
