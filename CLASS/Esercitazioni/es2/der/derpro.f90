program der
           implicit none
           real (kind = 10) :: x,xp,xm, f,fp, fm, deriv1,deriv2,deriv3, deltax,deltax0
           real (kind = 16) :: two = 2
           integer :: i
           x = 1.0
           deltax0 = 0.3
           do i = 1,20
               deltax = deltax0**i
               xp = x + deltax   
               xm = x - deltax
               f  = x**2
               fp = xp**2
               fm = xm**2
               deriv1 = (fp - f) / deltax
               deriv2 = (fp - f) / (xp - x)
               deriv3 = (fp - fm) / (two*deltax)

               print *, i,deltax, "DERIVES", deriv1,deriv2, deriv3
               
           end do
end program der
