      subroutine opnerr(iochec,fname) 
c---------------------------------------------------------------------- 
c       execution trap for errors during attempt to open file=fname 
c  iochec=value returned as iostat
c  fname=7 character file name of faulty file.
c---------------------------------------------------------------------- 
      character*(*) fname 
      print *,' cannot open file=',fname
      print *,' iostat in open return value of - ',iochec 
      print *,' fatal error-execution terminated' 
      stop  
      end 

c $Id$ 
