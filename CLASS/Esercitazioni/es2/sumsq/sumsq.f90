program sumsq

implicit none

! Costants and Variables

integer :: i, output = 0, limsup, chknum1

! Start main code here

print*, "Scrivere l' N-esimo quadrato limite della somma. N = ..."

read*, limsup

do i = 1,limsup,1
	output = (output + (i)**2)
	
end do

print*, output

chknum1 = (limsup*(limsup+1)*(2*limsup+1))/6

print*, chknum1

! Stop main code here

end program sumsq
