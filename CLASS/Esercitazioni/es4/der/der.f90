program derive

        implicit none
	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(33)
           
           real (kind = rk) :: x,xp,xm, f,fp, fm, deriv1,deriv3, deltax, deltax0
           integer (kind = ik) ::i
           
           x = 1.0_rk
           deltax0 = 0.9_rk
           
           do i = 201_ik,1_ik,-1_ik
           
               deltax = deltax0**i
               xp = x + deltax   
               xm = x - deltax
               
               f  = exp(x)
               fp = exp(xp)
               fm = exp(xm)
               
               deriv1 = ( fp - f ) / deltax
               deriv3 = ( fp - fm ) / (2.0_rk*deltax)
               
               if ((fp-f)==0)cycle   ! l' istruzione cycle fa saltare al blocco di controllo del ciclo do
               
               write(unit=1,fmt=*) i, deltax, deriv1, deriv3, exp(1.0_rk)
               
           end do
           
end program derive
