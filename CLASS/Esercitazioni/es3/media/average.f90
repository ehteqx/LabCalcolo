program media

	implicit none
	real :: aver,x,sqavg,var
	integer :: i,N
   
	print*, " N= ... ? "
	read*, N    
	print*, "Immetti i dati uno alla volta... "
	print*, " "
	
	print*, " "
	print*, "DATO 1... "

	aver = 0.0
	sqavg = 0.0
	var = 0.0
	
	do i = 1,N
   		read*,x
   		print*, " "
   		print*, "DATO ", i+1, "... "
   		aver = aver + x
   		sqavg = sqavg + (x)**2.0
   		
	end do
	
	aver = aver/N
	sqavg = sqavg/N
	var = sqavg - (aver)**2.0
	
	print*, "Media dei ",N," dati: ",aver
	print*, "Media  dei quadrati dei ",N," dati: ",sqavg
	print*, "Varianza del campione dei ",N," dati: ",var
	
end program media
