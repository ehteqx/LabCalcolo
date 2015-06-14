program partsum

	implicit none
	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(33)
		
	integer (kind = ik) :: counter, n, counter2
	real (kind = rk) :: x, resexp, resinv, resexact, factn
	
	print*, "Inserire valore di centratura della serie (x)..."
	read*, x
	print*, " "
	
	!print*, "Inserire il numero di termini dello sviluppo (n)..."
	!read*, n
	!print*, " "
	n = 40	

! CALCOLO COME EXP(-x)

	counter = 0_ik
	resexp = 1_ik
	
	do counter = 1,n,1
	
		counter2 = 1_ik
		factn = 1.0_ik
		
		do counter2 = 1,counter,1

			factn = factn*(real(counter2))

		end do
	
		resexp = resexp + ((((-1.0_rk)*x)**real(counter))/factn)
		!print*, "Valore vero", resexact , " || Valore approssimato exp ", resexp

	end do
	
	resexact = EXP(-x)
	
	print*, "Valore vero", resexact , " || Valore approssimato exp ", resexp
	

! CALCOLO COME 1/(EXP(x))

	counter = 0_ik
	resexp = 1_ik
	
	do counter = 1,n,1
	
		counter2 = 1_ik
		factn = 1.0_rk
		
		do counter2 = 1,counter,1

			factn = factn*(real(counter2))

		end do
	
		resexp = resexp + ((((1.0_rk)*x)**real(counter))/real(factn))

	end do
	
	resinv = (1.0_rk/resexp)
	
	print*, "Valore vero", resexact , " || Valore approssimato inv ", resinv
	
end program partsum
