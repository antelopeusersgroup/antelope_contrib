
      subroutine ciptok( typbuf,ibp,token )
c
c   purpose:
c      To take a typed in string of characters and break it
c      up into smaller strings called tokens. Tokens are
c      fields delimited by blanks or commas. Multiple or
c      redundant blanks are treated as one blank however
c      two successive commas will result in a blank token
c      being returned to the calling program whether are not
c      there are blanks in between them.
c
c   args:
c      typbuf      buffer containing the string to be parsed
c      ibp         buffer pointer ( 0 or -> delimiter )
c      token       token found ( or eoi for end of input )
c
c   versions and revisions:
c      for UNIVAC R.Goff Sept. 1981
c      for VAX/UNIX R.Goff Dec. 1981
c
      character*(*) typbuf,token
      character*1 olddel
        common/sccsciptok/ sccsid
        character*80 sccsid
        data sccsid /'@(#)ciptok.f      41.1    12/21/90'/
c
c   see that buffer pointer is in bounds
c
      l  =  len( typbuf )
      if( ibp .lt. 0 ) then
        ibp  =  0
        token  =  ' '
        return
      end if
      if( ibp .ge. l ) then
        ibp  =  l
        token  =  'eoi'
        return
      end if
c
c   pick up old delimiter
c
      olddel  =  ' '
      if( ibp .gt. 0 ) olddel  =  typbuf( ibp:ibp )
c
c   skip white space
c
100   ibp  =  ibp + 1
      if( typbuf(ibp:ibp) .ne. ' ' ) go to 200
      if( ibp .lt. l ) go to 100
c
c   end of buffer - token = 'eoi'
c
150   token  =  'eoi'
      return
c
c   check for two commas
c
200   if( typbuf(ibp:ibp) .ne. ',' ) go to 300
      if( olddel .eq. ',' ) then
        token  =  ' '
        return
      end if
      olddel  =  ','
      if( ibp .ge. l ) go to 150
      go to 100
c
c   copy token to output
c
300   ist  =  ibp
400   ibp  =  ibp + 1
      if( typbuf(ibp:ibp) .eq. ' ' ) go to 500
      if( typbuf(ibp:ibp) .eq. ',' ) go to 500
      if( ibp .lt. l ) go to 400
      token  =  typbuf( ist:l )
      return
500   token  =  typbuf( ist:ibp-1 )
      return
      end


c $Id$ 
