      subroutine strcat(str1,str2)
c---------------------------------------------------------------------
c        concatenates character string stored in "str2" onto the
c  end of the character string stored in "str1".  hence, str1 is
c  always altered unless str1 is already full.
c---------------------------------------------------------------------
      character*(*) str1
      character*(*) str2
      len1 = len(str1)
      lens1 = lenstr(str1)
      lens2 = lenstr(str2)
      ii = 0
      do 100 i=lens1+1,min(len1,lens1+lens2)
           ii = ii + 1
           str1(i:i) = str2(ii:ii)
  100 continue
      return
      end 

c $Id$ 
