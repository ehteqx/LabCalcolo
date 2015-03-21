program multieq


      implicit none


      real    :: a,b,c,realsol

      complex :: discr,xplus,xminus


      read*, a,b,c


	if ((a .EQ. 0) .AND. (b .EQ. 0)) then
		print*, "Cannot divide by zero!"

	elseif (a .EQ. 0) then
		print*, "Detected I degree equation!"
		print*, "Printing solutions..."
		realsol = (-c/b)
      		print*, realsol

      	else
      		print*, "Detected II degree equation!"
      		print*, "Printing solutions..."
      		discr = b**2 - 4*a*c
      		xplus = (-b+sqrt(discr))/(2*a)
      		xminus = (-b-sqrt(discr))/(2*a)
      		print*,xplus,xminus
	end if


end program multieq
