module dummy

	implicit none
	
	contains
	elemental function raddoppia(x) result(dop)
		real,intent(in) :: x
		real            :: dop
		
		dop= 2*x
		
	end function raddoppia

end module dummy


program main
	use dummy
	implicit none

	integer :: arraynum
	real :: qlnumber, outnum	
	real, dimension(:), allocatable :: inarray
	real, dimension(:), allocatable :: outarray
	
	print*, 'Inserire il numero di elementi che deve contenere array...'
	read*, arraynum
	
	allocate (inarray(arraynum))
	allocate (outarray(arraynum))
	
	print*, 'Popolare array con dei valori reali...'
	read*, inarray
	
	outarray = raddoppia(inarray)
	
	print*, outarray
	
	print*, 'Inserire un numero a piacere...'
	read*, qlnumber
	
	outnum = raddoppia(qlnumber)
	
	print*, outnum
	
	
end program main
