        subroutine iotrap(nrow,ncol,ieof) 
c---------------------------------------------------------------------- 
c         prints error messages returned by glplib matrix i/o routines. 
c  and aborts calling program.  note this is slightly dangerous as
c  this fact is not communicated to the operating system.  a full abort 
c  sequence requires a system dependent call that defeats the 
c  portability goal of primel.  this version is a replacement for an
c  earlier version of the same name that was somehow lost in moving this
c  code between various different computer systems. 
c 
c  arguments- 
c    nrow - number of rows of array routine attempted to read/write 
c    ncol - number of columns of array routine attemped to read/write.
c    ieof - error code returned.
c 
c  written:  june 1984 as replacement ot lost older version written 
c            in about 1981. 
c  author:  gary l. pavlis
c           dept. of geology
c           indiana univ. 
c           bloomington, in  47405
c---------------------------------------------------------------------- 
        integer nrow,ncol,ieof
        if(ieof.lt.0) then
                print *,'unexpected eof encounted by matrix ',
     $                   'input routine'  
        elseif(ieof.eq.10) then 
                print *,'insufficient storage available for matrix read'
                print *,'trying to read ',nrow,' by ',ncol,' array' 
        elseif(ieof.eq.100) then
                print *,'read error from mtrxib2' 
                print *,'number of columns requested to be',  
     $                      ' read was too large' 
                print *,'number of columns available=',ncol 
        else
                print *,'matrix i/o error'
                print *,'unknown error code = ',ieof
        endif 
        print *,'////////////fatal error////////////' 
        stop
        end 

c $Id$ 
