	  function glat(hlat) 
c------convert geographic latitude to geocentric latitude------------- 
c	 hlat (input) = geographic latitude in radians (north positive) 
c	 glat (output)= geocentric latitude in radians (north positive) 
c--------------------------------------------------------------------- 
	data halfpi /1.570796/
	if(halfpi - abs(hlat).ge.0.05) then 
	  glat = atan(0.993277*sin(hlat)/cos(hlat))
	else 
c------special formula near pole 
	  glat = hlat/0.993277 - sign(0.010632, hlat)
	end if
 
	return 
	end 
