program epsil
   implicit none
   integer, parameter :: rk=selected_real_kind(8) ! il valore di rk  garantisce almeno 5 cifre significative decimali di precisione
   real(kind=rk) :: a,b 

   a= 1+epsilon(1.0_rk)/2+epsilon(1.0_rk)/2
   b= epsilon(1.0_rk)/2+epsilon(1.0_rk)/2+1

   print*, b-a
end program  epsil
