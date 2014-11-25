*-----        Location Routines      -------------------------------- + ----
c                                                                     ydist
      subroutine ydist
     ^        (dlats, dlons, dlatr, dlonr, delta, cazim, bazim, azima)
c
c     AUTHOR:  Brian L.N. Kennett  RSES, ANU
c     DATE:    January 1985
c     PURPOSE:
c             YDIST        Calculates distance and azimuth
c                          for spheroidal earth between
c                          specified geographic source and
c                          receiver station coordinates
c
*----------------------------------------------------------------------*------*
c     PARAMETERS
c
      real    dlats, dlons, dlatr, dlonr, delta, cazim, bazim
c
c     dlats  latitude of source
c     dlons  longitude of source
c     dlatr  latitude of receiver
c     dlonr  longitude of receiver
c     delta  angular distance
c     cazim  apparent azimuth at an array
c     bazim   azimuth from epicentre to receiver
c
*----------------------------------------------------------------------*------*
c
c     implicit real*8 (a-h,o-z)
      real*8 ecc,re,ec1,pi,pib2,degr,rlats,rlons,rlatr
      real*8 rlonr,glats,glatr,sps,cps,spr,cpr,rs,rr
      real*8 trs,prs,trr,prr,AS,BS,CS,DS,ES,GS,HS,KS
      real*8 AR,BR,CR,DR,ER,GR,HR,KR
      real*8 cosdr,deltar,sindr,deltak,szs,czs,szr,czr
      real*8 e,x,y
      real azima
c                          radius on spheroid
      gra(x,y,e) = dsqrt( (1.0d0-e)**2 /
     &                   ((1.0d0-e*y)**2 + e*e*x*y ) )
      ecc = 0.003367
      re = 6378.388
      ec1 = (1.0d0-ecc)**2
      pi = 3.141592653589793
      pib2 = pi/2.0
      degr = pi/180.0
      rlats = dlats*degr
      rlons = dlons*degr
      rlatr = dlatr*degr
      rlonr = dlonr*degr
c                          geocentric coordinates
      glats = datan2 ( ec1*dsin(rlats) ,dcos(rlats) )
      glatr = datan2 ( ec1*dsin(rlatr) ,dcos(rlatr) )
      sps = dsin(glats)**2
      cps = dcos(glats)**2
      spr = dsin(glatr)**2
      cpr = dcos(glatr)**2
c                          radii at source,receiver
      rs = re*gra(sps,cps,ecc)
      rr = re*gra(spr,cpr,ecc)
c
      trs = pib2 - glats
      prs = dlons*degr
      trr = pib2 - glatr
      prr = dlonr*degr
c                          direction cosines for source
      AS = dsin(trs)*dcos(prs)
      BS = dsin(trs)*dsin(prs)
      CS = dcos(trs)
      DS = dsin(prs)
      ES = -dcos(prs)
      GS = dcos(trs)*dcos(prs)
      HS = dcos(trs)*dsin(prs)
      KS = -dsin(trs)
c                          direction cosines for receiver
      AR = dsin(trr)*dcos(prr)
      BR = dsin(trr)*dsin(prr)
      CR = dcos(trr)
      DR = dsin(prr)
      ER = -dcos(prr)
      GR = dcos(trr)*dcos(prr)
      HR = dcos(trr)*dsin(prr)
      KR = -dsin(trr)
c                          distance
      cosdr = AS*AR + BS*BR + CS*CR
      deltar = dacos(cosdr)
      sindr = dsin(deltar)
c
      deltak = deltar*0.5d0*(rr+rs)
      delta = deltar/degr
c                          azimuth
      szs = DS*AR + ES*BR
      czs = GS*AR + HS*BR + KS*CR
      szr = DR*AS + ER*BS
      czr = GR*AS + HR*BS + KR*CS
c                          azima - azimuth to source
c                          bazim - backazimuth from source
c                          cazim - apparent azimuth at an array
      if (szr.eq.0.0) then
        bazim = 0.0
        if(dlats.gt.dlatr)then
           azima = 360.0
        else
           azima = 180.0
        endif
      else
        bazim = datan2(-szs ,-czs ) /degr
        azima = datan2(-szr ,-czr ) /degr
      end if
      if( bazim .lt. 0.0) bazim = bazim + 360.0
      cazim = azima + 180.0
      if( azima .lt. 0.0) azima = azima + 360.0
c
      if( cazim.lt. 0.0) cazim = cazim + 360.0
c
      return
      end
