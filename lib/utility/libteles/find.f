
	subroutine find (x,xt,n,ians)
c
c     x is a monotonically increasing vector of dimension n.
c     xt is greater than or equal to x(ians) but less than
c     x(ians+1).
c
	dimension x(n)
	if (xt .le. x(1)) go to 104
	if (xt .ge. x(n)) go to 105
	il=1
	im=n
102	itst=im-il
	if (itst .gt. 1) go to 100
	ians=il
	go to 103
100	ihalf=(im+il)/ 2
	if(xt .ge. x(ihalf)) go to 101
	im=ihalf
	goto 102
101	il=ihalf
	go to 102
104	ians=1
	go to 103
105	ians=n
103	return
	end
