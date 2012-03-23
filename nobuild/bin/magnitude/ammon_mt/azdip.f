      subroutine azdip(vec,az,dip)
      dimension vec(3)
c
c     convert eigenvectors to angles
c
      degrad=3.1415826/180.
      a1=vec(1)
      a2=vec(2)
      if(a1.ne.0.0.or.a2.ne.0.0) go to 65
c
      az=0.0
      if(vec(3).gt.0.0) az=180.
      go to 70
c
   65 az=atan2(a1,a2)
      az=az/degrad
   70 a3=sqrt(a1**2+a2**2)
      a4=abs(vec(3))
      dip=atan2(a4,a3)
      dip=dip/degrad
      if(vec(3).gt.0.0) az=az-180.
      if(abs(az).gt.180.) az=az-sign(360.,az)
c
      return
      end
