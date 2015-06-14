program istog
	
	integer :: lenght, cnt
	integer :: input
	integer, dimension(-5:5) :: I=0
	
	print*, 'QUANTI VALORI VUOI INSERIRE?'
	read*, lenght
	
	print*, ' '
	
	do cnt = 1,lenght,1
		print*, 'Inserisci il',cnt,'* valore... '
		print*, ' '
		read*, input
		I(input) = I(input) + 1
	end do
	
	print*,(I(k),k=-5,5)
	print*,(j, j=-5,5)
	
end program istog
