      function ttlvz(delta,hpz)                                         ttlvz.2 
c -- ttlvz calculates travel time of seismic wave from hypocenter       ttlvz.3 
c -- earth model is plane parallel layers (n-1 layers over halfspace)   ttlvz.4 
c -- n is number of layers including halfspace                          ttlvz.5 
c -- v is array of length n containing layer velocities                 ttlvz.6 
c -- z is array of length n containing depths to layer tops             ttlvz.7 
c -- this routine is designed to work for all layer velocity combinationttlvz.8 
      dimension h(40),term(40)                                          ttlvz.9 
      common /model/ n,v(20),z(20)                                      ttlvz.10  
      data test/0.002/                                                  ttlvz.11  
c -- locate layer containing hypocenter and store index in lhpz         ttlvz.12  
      do 110 j=1,n                                                      ttlvz.13  
      if (hpz.le.z(j)) go to 111                                        ttlvz.14  
110   continue                                                          ttlvz.15  
      lhpz = n                                                          ttlvz.16  
      go to 112                                                         ttlvz.17  
111   lhpz = j-1                                                        ttlvz.18  
      if (lhpz.eq.0) lhpz = 1                                           ttlvz.19  
112   continue                                                          ttlvz.20  
c -- assign internal depths to layer tops including correction for      ttlvz.21  
c    station elevation                                                  ttlvz.22  
      do 120 j=1,n                                                      ttlvz.23  
      if (j-lhpz) 121,122,123                                           ttlvz.24  
121   h(j) = z(j+1)-z(j)                                                ttlvz.25  
      go to 120                                                         ttlvz.26  
122   h(j) = abs(hpz-z(j))                                              ttlvz.27  
      go to 120                                                         ttlvz.28  
123   h(j) = z(j)-z(j-1)                                                ttlvz.29  
120   continue                                                          ttlvz.30  
      t = 1.00e+10                                                      ttlvz.31  
      if(lhpz.eq.n) go to 360                                           ttlvz.32  
      h(lhpz+1) = z(lhpz+1)-amax1(hpz,z(1))                             ttlvz.33  
c -- calculate smallest refracted wave time                             ttlvz.34  
      istrt = lhpz+1                                                    ttlvz.35  
      vmax = 0.                                                         ttlvz.36  
      do 760 j=1,lhpz                                                   ttlvz.37  
760   vmax = amax1(v(j),vmax)                                           ttlvz.38  
      do 190 lowlr=istrt,n                                              ttlvz.39  
c -- check to see if ray exists                                         ttlvz.40  
      if(v(lowlr).le.vmax) go to 190                                    ttlvz.41  
      vmax = v(lowlr)                                                   ttlvz.42  
      jlim = lowlr-1                                                    ttlvz.43  
c -- calculate offset distance                                          ttlvz.44  
      p = 1./v(lowlr)                                                   ttlvz.45  
      sum = 0.                                                          ttlvz.46  
      do 710 j=1,lhpz                                                   ttlvz.47  
      term(j) = sqrt(1.-(p*v(j))**2)+1.e-10                             ttlvz.48  
710   sum = h(j)*v(j)/term(j)+sum                                       ttlvz.49  
      do 720 j=lhpz,jlim                                                ttlvz.50  
      term(j) = sqrt(1.-(p*v(j))**2)+1.e-10                             ttlvz.51  
720   sum = 2.*h(j+1)*v(j)/term(j)+sum                                  ttlvz.52  
      offset = sum*p                                                    ttlvz.53  
      if (offset-delta) 780,780,190                                     ttlvz.54  
c -- calculate refraction path travel time for lowlr                    ttlvz.55  
780   sum = 0.                                                          ttlvz.56  
      do 730 j=1,lhpz                                                   ttlvz.57  
730   sum = h(j)*term(j)/v(j)+sum                                       ttlvz.58  
      do 740 j=lhpz,jlim                                                ttlvz.59  
740   sum = 2.*h(j+1)*term(j)/v(j)+sum                                  ttlvz.60  
      t1 = delta*p+sum                                                  ttlvz.61  
      t=amin1(t,t1)                                                     ttlvz.62  
190   continue                                                          ttlvz.63  
c -- calculate direct wave travel time                                  ttlvz.64  
360   if (lhpz.eq.1) go to 370                                          ttlvz.65  
      vmax=v(1)                                                         ttlvz.66  
      do 175 j=2,lhpz                                                   ttlvz.67  
175   vmax=amax1(vmax,v(j))                                             ttlvz.68  
c -- test to find maximum value for p                                   ttlvz.69  
      pmax = 1./vmax                                                    ttlvz.70  
      p = 0.5*pmax                                                      ttlvz.71  
155   p = (p+pmax)/2.                                                   ttlvz.72  
      sdel = 0.                                                         ttlvz.73  
      do 160 j=1,lhpz                                                   ttlvz.74  
160   sdel = v(j)*h(j)/sqrt(1.-(p*v(j))**2)+sdel                        ttlvz.75  
      if (delta-p*sdel) 166,161,155                                     ttlvz.76  
c -- now perform newton convergence from top down                       ttlvz.77  
166   continue                                                          ttlvz.78  
      sdel = 0. 
      sder = 0. 
      do 162 j=1,lhpz                                                   ttlvz.80  
      temp = sqrt(1.-(p*v(j))**2)                                       ttlvz.81  
      term1 = v(j)*h(j)/temp                                            ttlvz.82  
      sdel = term1+sdel                                                 ttlvz.83  
      sder = term1/temp**2+sder                                         ttlvz.84  
162   continue                                                          ttlvz.85  
      dmdp0 = delta-p*sdel                                              ttlvz.86  
      if(abs(dmdp0).lt.test) go to 161                                  ttlvz.87  
      p = dmdp0/sder+p                                                  ttlvz.88  
      go to 166                                                         ttlvz.89  
161   continue                                                          ttlvz.90  
c -- p has been determined to sufficient accuracy                       ttlvz.91  
c -- calculate direct wave travel time by summation                     ttlvz.92  
      sum = 0.                                                          ttlvz.93  
      do 180 j=1,lhpz                                                   ttlvz.94  
180   sum = h(j)/(v(j)*sqrt(1.-(p*v(j))**2))+sum                        ttlvz.95  
      t1=sum                                                            ttlvz.96  
      go to 350                                                         ttlvz.97  
370   t1 = sqrt(delta**2+(hpz-z(1))**2)/v(1)                            ttlvz.98  
350   t=amin1(t1,t)                                                     ttlvz.99  
      ttlvz = t                                                         ttlvz.10  
      return                                                            ttlvz.10  
      end                                                               ttlvz.10  

c $Id$ 
