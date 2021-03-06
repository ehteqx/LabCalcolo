program matrices
	
	integer, parameter :: N=3
	real, dimension(N) :: a
	real, dimension(N) :: b
	real, dimension(N) :: meta=0.0
	real :: pscal_aa=0.0, pscal_bb=0.0, pscal_ab=0.0, moda=0.0, modb=0.0, angolo=0.0
	integer :: ind1
	
	print*, 'Inserisci le coordinate del primo vettore (10)... '
	read*,a
	
	print*, 'Inserisci le coordinate del secondo vettore (10)... '
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
