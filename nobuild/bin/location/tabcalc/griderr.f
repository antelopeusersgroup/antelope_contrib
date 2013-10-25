      subroutine griderr(nerror,ip,nx,z)
c---------------------------------------------------------------------- 
c       writes error messages for subroutine gridtp.
c  arguments- 
c    nerror-error code (see below)
c    ip - index of verbose arrays when error detected.
c    nx - index of regular table arrays when error was detected.
c    z  - source depth when error was detected. 
c---------------------------------------------------------------------- 
      print *,'warning-error number ',nerror,' detected by gridtp.' 
      if(nerror.eq.1) then
           print *,'branch name does not match any known name.' 
      elseif(nerror.eq.2) then  
           print *,'grid arrays overflowed.'
           print *,'gridded travel time table was probably tructated.'
      endif 
      print *,'error detected at source depth = ',z 
      print *,'index positions when error was detected '
      print *,'verbose array index value = ',ip 
      print *,'gridded array index value = ',nx 
      return
      end 

c $Id$ 
