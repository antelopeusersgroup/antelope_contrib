      subroutine calnder(yr,day,mon,idy)
c -- calnder converts days of year to calender date 
      dimension month(12), iday(12) 
      data (month(i),i=1,12)/4h jan,4h feb,4h mar,4h apr,4h may,4hjune, 
     $4hjuly,4h aug,4hsept,4h oct,4h nov,4h dec/
      data (iday(i),i=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/
      if (amod(yr-64.,4.).eq.0.) iday(2) = 29 
      isum = 0
      do 100 i=1,12 
        iprev = isum  
        isum = isum+iday(i) 
        if (ifix(day).gt.isum) go to 100  
        idy = ifix(day)-iprev 
        mon = month(i)
        go to 110 
c 
 100  continue
 110  iday(2) = 28
      return
      end 

c $Id$ 
