PROGRAM random
	
	implicit none
	
	real :: buffer, media
	integer :: numero, i
	
	read*, numero
	
	do i = 1,numero
		call random_number(buffer)
		media = (((media*i) + buffer)/(i+1))
		print*, buffer
	end do
	
	print*, media
	
end program random
