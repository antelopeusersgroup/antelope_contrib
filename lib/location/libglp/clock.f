      subroutine clock(time)
c-------------------------------------------------------------------- 
c      corrects times in (yr,day,hr,min,sec) format when seconds
c  are greater than 60 or less than 0.0.  routine never corrects
c  greater than one minute per call.  ie if seconds are greater than
c  120 seconds the correction will not be complete.  routine works
c  for any day of the year and over new years including leap years. 
c 
c  arguments -
c    time - 5 component floating point array holding time in  
c           sequence - (yr,day,hr,min,sec)
c  language - 1977 ansi standard fortran  
c-------------------------------------------------------------------- 
      real time(5)
      if(time(5).ge.60.0) then  
            time(5) = time(5) - 60.0
            time(4) = time(4) + 1.0 
            if(time(4).ge.60.0) then
                  time(4) = time(4) - 60.0
                  time(3) = time(3) + 1.0 
                  if(time(3).ge.24.0) then
                        time(3) = time(3) - 24.0
                        time(2) = time(2) + 1.0 
                        if(time(2).gt.365.0) then 
                              if(mod(time(1),4.0).ne.0.0) then
                                    time(1) = time(1) + 1.0 
                                    time(2) = 0.0 
                              elseif(time(2).gt.366) then 
                                    time(1) = time(1) + 1.0 
                                    time(2) = 0.0 
                              endif 
                        endif 
                  endif 
            endif 
      elseif(time(5).lt.0.0) then 
            time(5) = time(5) + 60.0
            time(4) = time(4) - 1.0 
            if(time(4).lt.0.0) then 
                  time(4) = time(4) + 60.0
                  time(3) = time(3) - 1.0 
                  if(time(3).lt.0.0) then 
                        time(3) = time(3) + 24.0
                        time(2) = time(2) - 1.0 
                        if(time(2).lt.0.0) then 
                              time(1) = time(1) - 1.0 
                              if(mod(time(1),4.0).eq.0.0) then
                                    time(2) = 366.  
                              else
                                    time(2) = 365.  
                              endif 
                        endif 
                  endif 
            endif 
      endif 
      return
      end 

c $Id$ 
