      subroutine slant(p,zstat,zzero,vzero,time,dist) 
c---------------------------------------------------------------------
c     slant calculates simple slant path travel time and distance 
c  corrections for stations with nonzero elevations.
c 
c  arguments-(input)
c 
c     p-ray parameter 
c     zstat-station depth (note positive station elevations are 
c           negative depths so zstat is almost always negative
c     zzero-depth to correct travel time and distance to
c     vzero-stripping velocity for upper layer (assumed constant) 
c 
c  arguments-(output) 
c 
c     time-travel time correction 
c     dist-distance correction
c 
c---------------------------------------------------------------------
      if(zzero.eq.zstat) then 
            time = 0.0
            dist = 0.0
      else  
            dz = zzero - zstat  
            rad = sqrt(1. - (p*vzero)**2) 
            time = dz/(vzero*rad) 
            dist = p*vzero*dz/rad 
      endif 
      return
      end 

c $Id$ 
