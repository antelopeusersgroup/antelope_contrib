      subroutine crotg(ca,cb,c,s)                                        crotg       2
c***begin prologue  crotg                                                crotg       3
c***category no.  f1a                                                    crotg       4
c***purpose  constructs complex givens rotation  --see srotg             crotg       5
c***refer to srotg                                                       crotg       6
c***routines called  (none)                                              crotg       7
c***end prologue  crotg                                                  crotg       8
      complex ca,cb,s                                                    crotg       9
      real c                                                             crotg      10
      real norm,scale                                                    crotg      11
      complex alpha                                                      crotg      12
c***first executable statement  crotg                                    crotg      13
      if (cabs(ca) .ne. 0.) go to 10                                     crotg      14
         c = 0.                                                          crotg      15
         s = (1.,0.)                                                     crotg      16
         ca = cb                                                         crotg      17
         go to 20                                                        crotg      18
   10 continue                                                           crotg      19
         scale = cabs(ca) + cabs(cb)                                     crotg      20
         norm = scale * sqrt((cabs(ca/scale))**2 + (cabs(cb/scale))**2)  crotg      21
         alpha = ca /cabs(ca)                                            crotg      22
         c = cabs(ca) / norm                                             crotg      23
         s = alpha * conjg(cb) / norm                                    crotg      24
         ca = alpha * norm                                               crotg      25
   20 continue                                                           crotg      26
      return                                                             crotg      27
      end                                                                crotg      28

c $Id$ 
