      real function wtlqk(iwt)                                           wtlqk
c--------------------------------------------------------------------    wtlqk
c  returns statistical weighting parameter for this datum.  formula      wtlqk
c  used is consistent with that in lquake2.                              wtlqk
c  arguments-                                                           wtlqk 
c    iwt - interger weight read from phase data card.                    wtlqk
c--------------------------------------------------------------------    wtlqk
      sigma=0.05+iwt*0.05                                                wtlqk
      wtlqk=1.0/sigma                                                    wtlqk
      return                                                             wtlqk
      end                                                                wtlqk

c $Id$ 
