module geompro

	implicit none
	private     ! rende privati tutti i nomi non esplicitamente dichiarati pubblici

	public :: punto, operator(+)

		type :: punto
    		real ::x,y
    		
  		contains
    		procedure :: inv => invert
		end type punto

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


program pippo

	use geom
	
	implicit none
	type(punto) :: A,B,C
	
	read*,A
	print*,A%inv()
	
end program pippo
