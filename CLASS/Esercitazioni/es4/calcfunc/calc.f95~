program calcfunc
	
	implicit none
	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(33)
	
	integer (kind = ik) :: intnum, i
	real (kind = rk), parameter :: pi = acos(-1.0_rk)
	real (kind = rk) :: x, res, linspace, y
	
	!print*, "Scrivere in numero N di intervallini in cui dividere l'intervallo. N = ..."

	!read*, intnum
	
	intnum = 100
	
	print*, "Scrivere l'ampiezza L dello spazio lineare in cui stampare i dati. L = ..."

	read*, linspace

	do i = 1,intnum,1
		x = 0.0_rk + (linspace*i/intnum)
		y = (sqrt((2_ik)/(pi)/x)*((3_ik/x**2_ik-1_ik)*sin(x)-3_ik/x*cos(x)))

		write(10,*)x,y

		end do	
	
end program calcfunc
