! ### MODULES ###

MODULE PREC

	implicit none
	integer, parameter :: rk= selected_real_kind(31) ! Modify here REAL precision
	integer, parameter :: ik= selected_int_kind(16)  ! Modify here INTEGER precision
	
END MODULE PREC

! ##############################################################################

MODULE AUXILIARY

	use PREC
	implicit none

	contains

	FUNCTION fun(x) result(y)
		real (kind = rk), intent(in) :: x
		real (kind = rk) :: y
		y = x**(2) - 2_ik	! Insert here the FUNCTION you want to "zero"
	END FUNCTION fun


END MODULE AUXILIARY

! ##############################################################################

MODULE BISECTZ

	use PREC
	use AUXILIARY

	implicit none
		
	contains

	FUNCTION Bisection(fun, tol) result(r)
	
		real(kind=rk), intent(in) :: tol
		
		real(kind=rk) :: sx = 0.0_rk, dx = 0.0_rk, guestep, zero, r
		
			interface
				function fun(x) result(y)
				use PREC
					real(kind = rk), intent(in) :: x
					real(kind = rk) :: y
				end function
			end interface
			
			guestep = 10 ! Change value; maybe not used!
		
	! SIGN CHECKER
		do while ((fun(dx)*fun(sx)) .GE. 0.0)
			dx = dx + guestep
			sx = sx - guestep			
			
			print*, 'HERE I AM!'
		end do
		
!	sx = a
!	dx = b		
	
	cycler: do
	
		zero = (dx + sx)/2.0_rk
		
		if (abs(fun(zero)) .LE. tol) then ! Modify tolerance
			print*, 'SOLUTION FOUND!! x = ', zero
			exit cycler
			
		else
		
			if ((fun(zero)*fun(dx)) .LT. 0.0) then
				sx = zero			
			else
				dx = zero
			end if
		end if
	
	end do cycler
	
	r = zero
	
	END FUNCTION Bisection

END MODULE BISECTZ

! ##############################################################################
! ##############################################################################

! ### PROGRAM ###

PROGRAM BISECTOR
	
	! Uses
	USE BISECTZ
	
	! Data declarations
	implicit none

	
	real (kind = rk) :: tol, zero
	
	! Cosmetics and user input
	print*, 'Benvenuto! Questo software calcola uno zero della funzione  .' ! Change function
	print*, 
	print*, 'Inserire la tolleranza assoluta... '	
	read*, tol
	
	! Algorithm
	zero = Bisection(fun,tol)
	
	! Cosmetics and printout
	print*, 
	print*, 'ZERO CALCOLATO... '	
	print*, zero
	
END PROGRAM BISECTOR
