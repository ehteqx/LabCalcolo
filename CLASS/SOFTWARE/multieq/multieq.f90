program secondo_grado


     implicit none
	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(33)
	

      real (kind = rk)    :: a,b,c,realsol
      complex (kind = rk) :: discr,xplus,xminus,xplus_alt,xminus_alt


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
      		discr = b**2.0_rk - 4.0_rk*a*c
      		xplus = (-b+sqrt(discr))/(2.0_rk*a)
      		!xminus = (-b-sqrt(discr))/(2.0_rk*a)
      		print*,xplus,xminus
      		
      		print*, "ALTERNATE METHOD..."
      		xplus_alt = -2.0_rk*c/(b + sqrt(discr))
		!xminus_alt = -2.0_rk*c/(b - sqrt(discr))
		print*,xplus_alt,xminus_alt
      		
	end if


end program secondo_grado
