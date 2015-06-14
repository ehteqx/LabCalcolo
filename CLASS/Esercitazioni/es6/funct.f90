function square (x) result (s)

	implicit none
	integer,intent (in) ::x
	real ::s,p
	
	p=2.0
	s=p*x**2
	
end function square


program est

	implicit none
	real :: a,p
	real, external :: square
	
	a=0.1
	p=-1.0
	
	print'(es13.6)',3*square(a)
	print*,p
	
end program est
