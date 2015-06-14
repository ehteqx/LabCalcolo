program sumsq

implicit none

! Costants and Variables

integer :: i, intnum
real (kind = 8) :: asx

! Start main code here

print*, "Scrivere in numero N di intervallini in cui dividere l'intervallo. N = ..."

read*, intnum

do i = 1,intnum,1
	asx = -1.5 + (i/intnum*(3))
	
print*, intnum
print*, asx

end do

! Stop main code here

end program sumsq
