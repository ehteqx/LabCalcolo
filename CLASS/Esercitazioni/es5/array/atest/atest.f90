program arraytest
	
	real, dimension(10) :: x
	
	read*,x
	
	do i=1,10
		read*,x(i)
	end do
	
	do i=1,10
		print*,x(i)
	end do
	
	print*,(x(i),i=1,10)
	
end program arraytest
