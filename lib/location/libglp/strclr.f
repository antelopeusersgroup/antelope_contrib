      subroutine strclr(string) 
c---------------------------------------------------------------------
c        initializes character string variable "string" of
c  arbitrary length to all blank characters.
c---------------------------------------------------------------------
      character*(*) string
      do 100 i=1,len(string)
           string(i:i) = ' '
  100 continue
      return
      end 

c $Id$ 
