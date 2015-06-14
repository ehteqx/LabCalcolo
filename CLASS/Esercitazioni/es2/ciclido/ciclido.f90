program cicli_do
implicit none
integer :: i,j,n_ciclo = 1    ! la variabile n_ciclo e' inizializzata a 1 oltre che dichiarata

print*," ciclo N.   ",n_ciclo     ! ciclo n. 1 ; scriviamo di quale
                                  ! ciclo si tratta per identificarlo in output
do i = 1,10,1
   print*,i
end do

n_ciclo = n_ciclo + 1             ! incrementiamo il numero di ciclo
print*," ciclo N.   ",n_ciclo     ! ciclo n. 2
do i = 1,10
   print*,i
end do

n_ciclo = n_ciclo + 1
print*," ciclo N.   ",n_ciclo     ! ciclo n. 3
do i = 1,10,-1
   print*,i
end do

n_ciclo = n_ciclo + 1
print*," ciclo N.   ",n_ciclo     ! ciclo n. 4
do i = 10,10,2
   print*,i
end do

n_ciclo = n_ciclo + 1
print*," ciclo N.   ",n_ciclo     ! ciclo n. 5
do i = 10,1,-1
   print*,i
end do

n_ciclo = n_ciclo + 1
print*," ciclo N.   ",n_ciclo     ! ciclo n. 6
do i = -10,10,3
   print*,i
end do

n_ciclo = n_ciclo + 1
print*," ciclo N.   ",n_ciclo     ! ciclo n. 7

do i = 1,3
   do j = 1,3
      print*,"i= ",i," j= ",j
   end do
end do

n_ciclo = n_ciclo + 1
print*," ciclo N.   ",n_ciclo     ! ciclo n. 8
do i = 1,3
   do j = i+1,3
      print*,"i= ",i," j= ",j
   end do
end do
end program cicli_do 
