      real function wtshot(iwt)                                          wtshot 
c--------------------------------------------------------------------    wtshot 
c  returns statistical weighting parameter for this datum.  formula      wtshot 
c  used is consistent with that in lquake2.                              wtshot 
c  arguments-                                                           wtshot
c    iwt - integer weight read from phase data card.                    wtshot
c--------------------------------------------------------------------    wtshot 
      sigma=0.05+iwt*0.05                                                wtshot 
      wtshot=1.0/sigma                                                    wtshot  
      return                                                             wtshot 
      end                                                                wtshot 

c $Id$ 
