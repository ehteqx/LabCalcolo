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
		y = x**(1.5_rk)	! Insert here the FUNCTION you want to integrate
	END FUNCTION fun


END MODULE AUXILIARY

! ##############################################################################

MODULE INTEGRATE

	use PREC
	use AUXILIARY
	implicit none
		
contains

	FUNCTION Trapezi(fun, a, b, n) result(r)
	
		real(kind=rk), intent(in) :: a, b
		integer (kind = ik), intent(in) :: n
		
		integer (kind = ik) :: i
		real(kind = rk) :: r, valueadd, h
		
			interface
				function fun(x) result(y)
				use PREC
					real(kind = rk), intent(in) :: x
					real(kind = rk) :: y
				end function
			end interface
		
		h = ((abs(b-a))/n)
		
		r = 0.0_rk
		
		valueadd = (fun(a) + fun(b))/2.0_rk
		
		do i = 1_ik,n-1_ik,1_ik
			
			r = (r + fun(a+(i*h)))
			
		end do
		
		r = (h*(r + valueadd))		
	
	END FUNCTION Trapezi

! ### ### ###
	
	FUNCTION Simpson(fun, a, b, n) result(r)
	
		real(kind=rk), intent(in) :: a, b
		integer (kind = ik), intent(inout) :: n
		
		integer (kind = ik) :: i
		real(kind=rk) :: r, h, valueadd
		
			interface
				function fun(x) result(y)
				use PREC
				real(kind=rk), intent(in) :: x
				real(kind=rk) :: y
				end function fun
			end interface
			
		! <oddity>
			if (floor(real(n)/2) /= real(n/2)) then
				n = n+1
			end if
		! </oddity>
		
		h = ((abs(b-a))/n)
		
		r = 0.0_rk
		
		valueadd = (fun(a) - fun(b)) ! VEDI SOTTO; Viene aggiunto il fun(a) e sottratto fun(b)
		
		do i = 1_ik,n-1_ik,2_ik
		
			r = r + 2*fun(a+(i*h)) + fun(a+((i+1_ik)*h)) 	! Il valore di fun(b) viene conteggiato ugualmente e VEDI SOTTO...
		
		end do
		
		r = r*2		! ... e moltiplicato per due. Cosi* si spiega la sottrazione dal valueadd.
		
		r = (h*(r + valueadd)/3.0_rk)		
	
	END FUNCTION Simpson	
	
	
END MODULE INTEGRATE

! ##############################################################################
! ##############################################################################

! ### PROGRAM ###

PROGRAM INTEGRALS
	
	! Uses
	USE PREC
	USE AUXILIARY
	USE INTEGRATE
	
	! Data declarations
	implicit none
	
	real (kind = rk) :: a=0.0_rk, b=1.0_rk, trapezio, simpsono
	integer (kind = ik) :: n
	
	! Cosmetics and user input
	print*, 'Benvenuto! Questo software integrera* la funzione exp(-(x**2)) .' ! Change function
	print*, 
	print*, 
	
	!print*, 'Inserire il punto iniziale di integrazione... '
	!read*, a	
	!print*, 
	!print*, 'Inserire il punto finale di integrazione... '	
	!read*, b
	!print*, 
	!print*, 'Inserire il numero di intervallini... '	
	!read*, n
	
	! Algorithm
	trapezio = (Trapezi(fun, a, b, n))
	simpsono = (Simpson(fun, a, b, n))
	
	! Cosmetics and printout
	!print*, 
	!print*, 'I valori calcolati sono... '	
	!print*, 
	!print*, 'REGOLA DEI TRAPEZI: '	, trapezio
	!print*, 'REGOLA DI SIMPSON: '	, simpsono
	
	open(unit=14, file='trap.dat')
	open(unit=15, file='simps.dat')
	
	do n = 1,90000,1000
		write(unit=14, fmt=*) (real(n)), (abs(Trapezi(fun, a, b, n))-0.4)
		write(unit=15, fmt=*) (real(n)), (abs(Simpson(fun, a, b, n))-0.4)
	end do
	
END PROGRAM INTEGRALS
