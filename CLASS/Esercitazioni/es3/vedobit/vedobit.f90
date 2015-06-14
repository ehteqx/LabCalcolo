program vedobit

	implicit none
	
	integer :: x  ! modificare in integer  per vedere le sequenze di bit di un intero
	integer ::ivar,j,i
	
	print*,"immetti un numero con punto decimale (real fortran)"
	read*,x
	
	ivar=transfer(x,ivar)
	
	
	do j=bit_size(i)-1,0,-1        !  inizia a scrivere dal bit piu'  significativo 
                               !  (quello piu' a sinistra nella scrittura
   		if(btest(ivar,j)) then      !  usuale) al  meno significativo

        		write(unit=*,fmt='(a)',advance='no')'1'   ! il formato (a) chiede che il dato da scrivere
   		else                                           ! sia trattato come stringa di caratteri.
        		write(unit=*,fmt='(a)',advance='no')'0'   ! advance='no' sopprime l' inserimento automatico 
   		end if                                         ! del carattere di "a capo" quindi la prossima
	end do                                            ! istruzione di scrittura iniziera' sulla 

	print*,"numero immesso: ",x                       ! stessa riga
	
end program vedobit
