      function istaptr(stanam,statab,nstat)                              istaptr  
c----------------------------------------------------------------------  istaptr  
c        returns index of station requested (stanam) that matches        istaptr  
c  those in station table.                                               istaptr  
c                                                                        istaptr  
c  arguments -                                                          istaptr 
c                                                                        istaptr  
c    stanam - station name to be searched for (character*4)              istaptr  
c    statab - table of station names (character*4)                       istaptr  
c    nstat - number of stations in station name table.                   istaptr  
c                                                                        istaptr  
c  returns -1 if station is not found.                                   istaptr  
c--------------------------------------------------------------------    istaptr  
      character*4 stanam,statab(nstat)                                   istaptr  
      do 100 i=1,nstat                                                   istaptr  
           if(stanam.eq.statab(i)) then                                  istaptr  
                istaptr = i                                              istaptr  
                return                                                   istaptr  
           endif                                                         istaptr  
  100 continue                                                           istaptr  
      istaptr = -1                                                       istaptr  
      return                                                             istaptr  
      end                                                                istaptr  

c $Id$ 
