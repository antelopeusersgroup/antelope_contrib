        integer function nalpha(line,nchar) 
c---------------------------------------------------------------------- 
c     test first nchar characters of character variable line for
c  alphabetic characters.  returns the number of alphabetic characters
c  actually found.  returns 0 if none are found.
c---------------------------------------------------------------------- 
        character token 
        character *(*) line 
        nalpha = 0
        do 100 i=1,min(nchar,len(line)) 
                token = line(i:i) 
                if((token.ge.'A').and.(token.le.'z'))nalpha=nalpha+1
  100   continue
        return
        end 

c $Id$ 
