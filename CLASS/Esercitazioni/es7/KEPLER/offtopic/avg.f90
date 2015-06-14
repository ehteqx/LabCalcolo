MODULE prova
contains
	function mediapesata(arr1,arr2) result (media)
	
	implicit none
		
		real, dimension(5), intent(in) :: arr1
		real, dimension(5), intent(in) :: arr2
		real :: media, somma
		integer :: ind
		
		do ind = 1,5
			somma = somma + arr1(ind)*arr2(ind)
		end do
		media = somma/sum(arr2)
	end function mediapesata
end module prova

program test

	use prova
	
	implicit none
	
	real, dimension(5) :: arr1 = 0
	real, dimension(5) :: arr2 = 0
	real :: media
	
	print*, 'LISTE'
	read*, arr1
	print*, 'PESI'
	read*, arr2
	
	media = mediapesata(arr1,arr2)
	print*, media
end program
