      function izptr(zhypo,dep,ndep)                                     izptr
c----------------------------------------------------------------------  izptr
c        returns pointer to point in array of depths (dep)               izptr
c  immediately above the source depth (zhypo).                           izptr
c                                                                        izptr
c  arguments-                                                           izptr 
c                                                                        izptr
c     zhypo-depth of source                                              izptr
c     dep - array of depths points at which the velocity model           izptr
c           is discretized.                                              izptr
c     ndep - length of dep array.                                        izptr
c                                                                        izptr
c  returns - izptr = index of point in dep array immediately above       izptr
c                    the source.                                         izptr
c---------------------------------------------------------------------   izptr
      dimension dep(ndep)                                                izptr
      if(zhypo.lt.dep(1)) then                                           izptr
           izptr = 1                                                     izptr
           return                                                        izptr
      endif                                                              izptr
      do 100 i=1,ndep-1                                                  izptr
           if((zhypo.ge.dep(i)).and.(zhypo.lt.dep(i+1))) then            izptr
                izptr = i                                                izptr
                return                                                   izptr
           endif                                                         izptr
  100 continue                                                           izptr
      izptr=ndep                                                         izptr
      return                                                             izptr
      end                                                                izptr

c $Id$ 
