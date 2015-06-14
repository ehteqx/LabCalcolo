! ### MODULES ###
!Deprecated <>
module geom
	
	implicit none
	
	private ! Deprecated <remove this>

		type :: punto
    		real ::x,y
		end type punto

		type :: triangolo
    		type(punto) :: vertice_A
    		type(punto) :: vertice_B
    		type(punto) :: vertice_C
		end type triangolo
 
	contains

		function sum_points(p,q) result(r)
			type(punto), intent(in) :: p,q
			type(punto)             :: r
			
			r%x = p%x + q%x
			r%y = p%y + q%y
			
		end function sum_points

		function invert(p) result(r)
			type(punto), intent(in) :: p
			type(punto) :: r
			
			r%x = -p%x
			r%y = -p%y
			
		end function invert

		function scala(p, coeff) result(r)
			type(punto), intent(in) :: p
			real, intent(in) :: coeff
			type(punto) :: r
			
			r%x = (p%x)*coeff
			r%y = (p%y)*coeff
			
		end function scala

end module geom
! Deprecated </>

! ### ### ###

module geompro

	implicit none
	private     ! rende privati tutti i nomi non esplicitamente dichiarati pubblici

	public :: punto, triangolo, operator(+), operator(*)

		type :: punto
    		real ::x,y
    		
  		contains
    		procedure :: inv => invert
		end type punto
		
		type :: triangolo
    		type(punto) :: vertice_A
    		type(punto) :: vertice_B
    		type(punto) :: vertice_C
		end type triangolo

		interface operator (+)
    		module procedure sum_points
		end interface operator (+)
		
		interface operator (*)
    		module procedure scala
		end interface operator (*)
		
		interface operator (*)
    		module procedure scalacomm
		end interface operator (*)

	contains

		function sum_points(p,q) result(r)
		
			type(punto), intent(in) :: p,q
			type(punto)             :: r
			
			r%x = p%x + q%x
			r%y = p%y + q%y
			
		end function sum_points
		
		function scala(p, coeff) result(r)
			type(punto), intent(in) :: p
			real, intent(in) :: coeff
			type(punto) :: r
			
			r%x = (p%x)*coeff
			r%y = (p%y)*coeff
			
		end function scala
		
		function scalacomm(coeff, p) result(r)
			type(punto), intent(in) :: p
			real, intent(in) :: coeff
			type(punto) :: r
			
			r%x = (p%x)*coeff
			r%y = (p%y)*coeff
			
		end function scalacomm

		function invert(this) result(r)
		
			class(punto), intent(in) :: this
			type(punto) :: r
			
			r%x = -this%x
			r%y = -this%y
			
		end function invert

end module geompro

! ### END MODULES


! ### PROGRAM ###

program piano
	use geompro
	implicit none
	
	type(punto) :: A,B,C
	type(triangolo) :: T1, T2, T3

	print*,'Coordinate 2D primo vertice'
	read*,A
	
	print*,'Coordinate 2D secondo vertice'
	read*,B
	
	print*,'Coordinate 2D terzo vertice'
	read*,C
	
	T1 = triangolo(A,B,C)		! il nome del tipo dati "costruisce" una variabile 
								! dello stesso tipo a partire dai 3 campi

	T2%vertice_A = T1%vertice_A%inv()
	T2%vertice_B = T1%vertice_B%inv()
	T2%vertice_C = T1%vertice_C%inv()

	print*,'coppie di coordinate dei vertici del triangolo T2', T2
	
	T3%vertice_A = (0.5*(T2%vertice_A + T2%vertice_B))
	T3%vertice_B = (0.5*(T2%vertice_B + T2%vertice_C))
	T3%vertice_C = (0.5*(T2%vertice_C + T2%vertice_A))
	
! PRINTS
	print*, ''
	print*, 'PRELIMINARY CHECK'
	print*, ''

	print*, 'TRIANGOLO T1'
	print*,' vertice A: ',T1%vertice_A
	print*,' vertice B: ',T1%vertice_B
	print*,' vertice C: ',T1%vertice_C
	print*, ''
	print*, ''
	
	print*, 'TRIANGOLO T2'
	print*,' vertice A: ',T2%vertice_A
	print*,' vertice B: ',T2%vertice_B
	print*,' vertice C: ',T2%vertice_C
	print*, ''
	print*, ''
	
	print*, 'TRIANGOLO T3'
	print*,' vertice A: ',T3%vertice_A
	print*,' vertice B: ',T3%vertice_B
	print*,' vertice C: ',T3%vertice_C
	print*, ''
	print*, ''
	
	! FILE PRINTS
	
	open(unit=14, file='triangolo.dat')
	
	! 'TRIANGOLO T1'
	write(unit=14, fmt=*) T1%vertice_A
	write(unit=14, fmt=*) T1%vertice_B
	write(unit=14, fmt=*) T1%vertice_C
	write(unit=14, fmt=*) T1%vertice_A
	write(unit=14, fmt=*) 
	write(unit=14, fmt=*) 
		
	! 'TRIANGOLO T2'
	write(unit=14, fmt=*) T2%vertice_A
	write(unit=14, fmt=*) T2%vertice_B
	write(unit=14, fmt=*) T2%vertice_C
	write(unit=14, fmt=*) T2%vertice_A
	write(unit=14, fmt=*) 
	write(unit=14, fmt=*) 
		
	! 'TRIANGOLO T3'
	write(unit=14, fmt=*) T3%vertice_A
	write(unit=14, fmt=*) T3%vertice_B
	write(unit=14, fmt=*) T3%vertice_C
	write(unit=14, fmt=*) T3%vertice_A
	write(unit=14, fmt=*) 
	write(unit=14, fmt=*) 
		
end program piano

! ### END PROGRAM ###
