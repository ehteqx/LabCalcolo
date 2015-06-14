program query_kind
 
       implicit none
       
       integer :: i
       real    :: dum
       
       do i=1,35
          print*," per ",i," cifre significative: kind= ",selected_int_kind(i)," per interi ",selected_real_kind(i)," per reali"
       end do
       
       print*,"  ***   valori negativi  significano che NON esiste un kind in grado di assicurare la precisione richiesta ***"
       print*
       print*," kind per gli interi di defaulti : ",kind(i)  ! l' argomento e' una qualsiasi variabile intera
       print*," kind per i real  di default : ",kind(dum)    ! l' argomento e' una qualsiasi variabiel real
       print*
       print*," numero cifre significative (binarie) real default    : ",digits(1.4)
       print*," numero cifre significative (binarie) integer default : ",digits(2)
       print*," epsilon per il tipo dati della variabile dum: ",epsilon(dum)
       
end program query_kind
