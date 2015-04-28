	parameter (MAXSYM = 20)

	x = 1.
	y = 0.
	do 100 i = 0, MAXSYM-1
		x = x + 1.5
		if (mod(i,4) .eq. 0) then
			y = y + 1.0
			x = 1.0
		endif
		call symbol (x, y, i, .5, 0.)
100	continue
	end
