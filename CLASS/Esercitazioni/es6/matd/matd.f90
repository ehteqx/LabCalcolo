program matrices
	
	integer :: N=0, testalloc
	real, dimension(:), allocatable :: a
	real, dimension(:), allocatable :: b
	real, dimension(:), allocatable :: meta
	real :: pscal_aa=0.0, pscal_bb=0.0, pscal_ab=0.0, moda=0.0, modb=0.0, angolo=0.0
	integer :: ind1
	
	print*, 'Inserisci il numero di componenti di un vettore... '
	read*, N
	
	allocate(a(N), b(N), meta(N), stat=testalloc)
	
	if (testalloc /= 0) then
		stop
		print*, "Errore di allocazione!"
	end if
	
	meta=0.0
	
	print*, 'Inserisci le coordinate del primo vettore... '
	read*,a
	
	print*, 'Inserisci le coordinate del secondo vettore... '
	read*,b
	
	print*, 'PRODOTTI SCALARI'
	
	do ind1 = 1,N,1
		pscal_aa = pscal_aa + a(ind1)*a(ind1)
	end do
		
	do ind1 = 1,N,1
		pscal_bb = pscal_bb + b(ind1)*b(ind1)
	end do
	
	do ind1 = 1,N,1
		pscal_ab = pscal_ab + a(ind1)*b(ind1)
	end do
		
	print*, pscal_aa, pscal_bb, pscal_ab
	
	meta=0.0
	meta = a*a
	pscal_aa = sum(meta)
	
	meta=0.0
	meta = b*b
	pscal_bb = sum(meta)
	
	meta=0.0
	meta = a*b
	pscal_ab = sum(meta)
	
	print*, pscal_aa, pscal_bb, pscal_ab
	
	
	pscal_aa = dot_product(a,a)
	pscal_bb = dot_product(b,b)
	pscal_ab = dot_product(a,b)
	
	print*, pscal_aa, pscal_bb, pscal_ab
	
	
	print*,'MODULI'
	moda= sqrt(pscal_aa)
	modb= sqrt(pscal_bb)
	
	print*, 'Di a... ', moda
	print*, 'Di b... ', modb
	
	
	print*, 'ANGOLO tra a e b... '
	angolo = acos(pscal_ab/(moda*modb))	
	print*, angolo
	
end program matrices
