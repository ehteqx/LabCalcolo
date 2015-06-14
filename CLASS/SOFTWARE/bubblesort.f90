PROGRAM BUBBLESORT
	
! Preliminary Declarations

	implicit none	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(32)
	
	integer (kind = ik) :: N, buffer, j, i, testalloc
	integer (kind = ik), dimension(:), allocatable :: array
	logical :: SORTED = .FALSE.
	
! User Input

	print*, 'Inserisci il NUMERO (intero) di elementi da ordinare... '
	read*, N
	
	allocate(array(N), stat=testalloc)											! Allocation Check
	if (testalloc /= 0) then
		stop
		print*, "Errore di allocazione!"
	end if
	
	print*, 'Inserisci gli ELEMENTI da ordinare (interi)... '
	read*, array	
	
! Algorithm

	ext: do i = 1,N
		
		do j = 1,(N-i)
			
			SORTED = .TRUE.
			
			buffer = array(j)
			
			if (buffer .GT. array(j+1)) then
				
				array(j) = array(j+1)
				array(j+1) = buffer
				
				SORTED = .FALSE.
										
			end if
			
			print*, SORTED
			if (SORTED) then
				exit ext
			end if
			
		end do
		
	end do ext

!Print Instruction
		
	print*, ''																	! Typesetting white line
	print*, array

END PROGRAM BUBBLESORT
