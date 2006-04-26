      subroutine ahat(v1,v2,ah)
      dimension v1(3),v2(3),ah(6)
c
c     routine to compute the operator matrix defined
c     below eq. 3.6 on p.355 of hext(1963)
c
      ah(1)=v1(1)*v2(1)
      ah(2)=v1(2)*v2(2)
      ah(3)=v1(3)*v2(3)
      ah(4)=v1(2)*v2(3)+v1(3)*v2(2)
      ah(5)=v1(1)*v2(3)+v1(3)*v2(1)
      ah(6)=v1(1)*v2(2)+v1(2)*v2(1)
      return
      end
