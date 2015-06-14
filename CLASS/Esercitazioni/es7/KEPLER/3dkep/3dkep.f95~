! ########## MODULES ##########

module gravitation
	
	implicit none
	integer,parameter :: kr=selected_real_kind(32), ki=selected_int_kind(15), nbody=2
	real(kind=kr),parameter :: G=6.673E-11_kr
	real(kind=kr),dimension(nbody) :: mass

	contains
	subroutine intgrav(pos,f,epot)
		real(kind=kr), intent(in), dimension(:,:) :: pos
		real(kind=kr), intent(out) :: epot
		real(kind=kr), intent(out), dimension(:,:) :: f
		real(kind=kr), dimension(size(pos,1)) :: posij
		real(kind=kr) :: rij
		integer ::i,j
		
		epot = 0
		f    = 0
		
		do i=1,nbody
			do j=1,nbody
				if(i == j) cycle
				posij  = pos(:,j)-pos(:,i)
				rij    = sqrt(dot_product(posij,posij))
				epot   = epot - (G*mass(i)*mass(j))/rij
				f(:,i) = f(:,i) + (G*mass(i)*mass(j)) * posij/rij**3
			end do
		end do
	
		epot = epot/2		! divisione per 2 perche' tutti i contributi di ciascuna
   							! stati contati due volte: (i,j) e (j,i)                                                            

 	end subroutine intgrav
end module gravitation


! ########## PROGRAM ##########

program g3dkep
	
	use gravitation
	implicit none
	real(kind=kr), dimension(3,nbody) :: pos, vel, f, vel_parziale, L
	real(kind=kr), dimension(3) :: Ltot
	real (kind = kr) :: dt, epot, ekin
	integer (kind = ki) :: nstep, it
	
	
	write(unit=*,fmt="(a)",advance="no")"Timestep: "
 	read*,dt   
 	
 	write(unit=*,fmt="(a)",advance="no")"Steps n.: "
 	read*,nstep    
	
	write(unit=*,fmt="(a)",advance="no")"Masses: "
 	read*,mass
 	
 	write(unit=*,fmt="(a)",advance="no")"pos(0): "
 	read*,pos
 	
 	write(unit=*,fmt="(a)",advance="no")"vel(0): "
 	read*,vel 	
	
	
	it=0_ki
	
	L(3,1) = mass(1)*((pos(1,1)*vel(2,1)) - (pos(2,1)*vel(1,1)))
	L(2,1) = mass(1)*((pos(3,1)*vel(1,1)) - (pos(1,1)*vel(3,1)))
	L(1,1) = mass(1)*((pos(2,1)*vel(3,1)) - (pos(3,1)*vel(1,1)))
	L(3,2) = mass(1)*((pos(1,2)*vel(2,2)) - (pos(2,2)*vel(1,2)))
	L(2,2) = mass(1)*((pos(3,2)*vel(1,2)) - (pos(1,2)*vel(3,2)))
	L(1,2) = mass(1)*((pos(2,2)*vel(3,2)) - (pos(3,2)*vel(1,2)))

	Ltot(1) = L(1,1) + L(1,2)
	Ltot(2) = L(2,1) + L(2,2)
	Ltot(3) = L(3,1) + L(3,2)	
	
 	write(unit=1,fmt=*)it,it*dt,pos,vel, Ltot					! INITIAL WRITE

	call intgrav(pos,f,epot)								! SUBROUTINE CALL

 	ekin =  0.5_kr*sum(spread(mass,1,3)*vel**2)
 	
 	write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot		! INTERMEDIATE WRITE 	
 	
	
	do it = 1,nstep
 	
    		pos = pos + vel * dt + 0.5* f/spread(mass,1,3) * dt**2
    		vel_parziale = vel + 0.5 * dt * f/spread(mass,1,3)    		! PARTIAL SPEED
    		
    		call intgrav(pos,f,epot)									! SUBROUTINE CALL
    		
    		vel = vel_parziale + 0.5_kr * dt * f/spread(mass,1,3)  		! FINAL SPEED
    		
    		L(3,1) = mass(1)*((pos(1,1)*vel(2,1)) - (pos(2,1)*vel(1,1)))
			L(2,1) = mass(1)*((pos(3,1)*vel(1,1)) - (pos(1,1)*vel(3,1)))
			L(1,1) = mass(1)*((pos(2,1)*vel(3,1)) - (pos(3,1)*vel(1,1)))
			L(3,2) = mass(1)*((pos(1,2)*vel(2,2)) - (pos(2,2)*vel(1,2)))
			L(2,2) = mass(1)*((pos(3,2)*vel(1,2)) - (pos(1,2)*vel(3,2)))
			L(1,2) = mass(1)*((pos(2,2)*vel(3,2)) - (pos(3,2)*vel(1,2)))

			Ltot(1) = L(1,1) + L(1,2)
			Ltot(2) = L(2,1) + L(2,2)
			Ltot(3) = L(3,1) + L(3,2)	
    		
    		write(unit=1,fmt=*)it,it*dt,pos,vel, Ltot					
    		ekin = 0.5_kr*sum(spread(mass,1,3)*vel**2)
    		write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot
    		
 	end do
 	
end program g3dkep
