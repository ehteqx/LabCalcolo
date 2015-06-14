! EUCLIDIV
! Euclidean algorithm for finding the GCD of two integer numbers
! (C) Emanuele Ballarin - 06/05/2015

! ########## MODULES ##########
MODULE euclidiv
	implicit none
	integer,parameter :: ik = selected_int_kind(12)
	
	CONTAINS	
	
! Euclidean Algorithm
	recursive function euclidean(x,y) result(gcd)
	
		implicit none
		integer (kind = ik), intent(in) :: x, y
		integer (kind = ik) :: gcd
	
		if (x == 0) then
			gcd = y
			
		elseif (y == 0) then
			gcd = x
		else
			gcd = euclidean(y,mod(x,y))
		end if
	end function euclidean

end module euclidiv

! ########## PROGRAM ##########
PROGRAM main
	use euclidiv
	
	implicit none
	integer (kind = ik) :: first,second, gcdr
	
	print*, 'INSERIRE IL PRIMO NUMERO (maggiore): '
	read*, first
	
	print*, ' '
	print*, 'INSERIRE IL SECONDO NUMERO (minore): '
	read*, second
	print*, ' '	
	
	gcdr = euclidean(first,second)
	print*, gcdr

end program main
