module dummy

	implicit none
	
	contains
	function raddoppia(x) result(dop)
		
		real,dimension(:),intent(in) ::x
		real,dimension(size(x,1)) ::dop
			
		dop= 2*x
			
end function raddoppia

end module dummy


program main
	use dummy
	implicit none

	integer :: arraynum	
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
	
end program main
