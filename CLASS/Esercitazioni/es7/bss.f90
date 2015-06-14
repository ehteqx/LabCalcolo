PROGRAM BUBBLESORT
	
! Preliminary Declarations

	implicit none	
	integer,parameter :: ik = selected_int_kind(18)
	
	integer (kind = ik) :: N, buffer, counter=0, i, testalloc
	integer (kind = ik), dimension(:), allocatable :: array
	logical :: SORTED
	
! User Input

	print*, 'Inserisci il NUMERO (intero) di elementi da ordinare... '
	read*, N
	
	allocate(array(N), stat=testalloc)											! Allocation Test
	if (testalloc /= 0) then
		stop
		print*, "Errore di allocazione!"
	end if
	
	print*, 'Inserisci gli ELEMENTI da ordinare (interi)... '
	read*, array	
	
! Algorithm

	SORTED = .FALSE.

	do while (SORTED .EQV. (.FALSE.))
	counter = counter+1
		
		do i = 1,N-1
			
			buffer = array(i)
			
			SORTED = .TRUE.
			
			if (buffer .GT. array(i+1)) then
			
				array(i) = array(i+1)
				array(i+1) = buffer
				SORTED = .FALSE.
				
			end if
			
		end do
		
		print*, counter
		
	end do

!Print Instruction
		
	print*, array

END PROGRAM BUBBLESORT
