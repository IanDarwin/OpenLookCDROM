c for fitting eclipse transition
	function user(a, x)
	real a(*), x

	if(x .lt. a(1))then
		user = a(3)
	else if(x .gt. a(2))then
		user = a(4)
	else
		user = (x - a(1))/(a(2) - a(1)) * (a(4) - a(3))
		user = user + a(3)
	end if

	end
