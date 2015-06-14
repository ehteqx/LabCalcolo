program strano
          implicit none
          real :: a=1.0,b=0.0,c=-1.0
          print*," 1.0/0.0 = ",a/b
          print*," 0.0/0.0 = ",b/b
          print*,"-1.0/0.0 = ",c/b
          print '(es14.7)',10.0**(-42)        ! '(es14.7)' e' un esempio di "format"
          print '(es14.7)',10.0**(-42)*1.0e7  ! serve a controllare direttamente come
          print '(es14.7)',10.0**(-35)        ! un' espressione viene visualizzata  
          print '(es14.7)',10.0**(-100)       ! es sta per notazione scientifica 
       end program strano                     ! 14 indica il numero totale di caratteri,       
                                              ! 7 il numero di cifre dopo il punto decimale della mantissa
