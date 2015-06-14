program realeq2


      implicit none


      real    :: a,b,c,discr,xplus,xminus

      read*, a,b,c
      
      discr = b**2 - 4*a*c      

	if (discr < 0) then
		print*, "No real solutions found!"

      	else
      		print*, "Printing solutions..."
      		xplus = (-b+sqrt(discr))/(2*a)
      		xminus = (-b-sqrt(discr))/(2*a)
      		
      			if (xplus == xminus) then
      				print*, xplus
      				
      			else
      				print*,xplus,xminus
      				
      			end if
	end if


end program realeq2
