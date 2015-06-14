program harm
 
 	implicit none
	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(32)

 	real (kind = rk)    :: massa=1.0_rk, kappa=1.0_rk 	! valori di default per massa e cost. elastica
 	real (kind = rk)   :: dt, ekin, epot			! OPTIMAL TIMESTEP: 0.015 (Recommended)
 	real (kind = rk)    :: pos, vel, vel_parziale, f
 	real (kind = rk)    :: B, posan_0, posan, time
 	integer (kind = ik) :: nstep, it, cyc
 	
 	write(unit=*,fmt="(a)",advance="no")"delta t : "        ! il formato (a) chiede che il dato sia trattato come 
 	read*,dt   
 	                                        		! caratteri e advance="no"  sopprime l'  inserimento del 
 	write(unit=*,fmt="(a)",advance="no")"n.step: "          ! caratter "a capo"  alla fine della linea per cui 
 	read*,nstep    
 	                                    			! la prossima operazione di lettura/scrittura inziera'
 	write(unit=*,fmt="(a)",advance="no")"massa: "           ! sula stessa riga di schermo di quella corrente
 	read*,massa
 	
 	write(unit=*,fmt="(a)",advance="no")"kappa: "
 	read*,kappa
 	
 	write(unit=*,fmt="(a)",advance="no")"pos(0): "
 	read*,pos
 	
 	write(unit=*,fmt="(a)",advance="no")"vel(0): "
 	read*,vel
 	
 	posan_0 = pos
 	B = (vel/(sqrt(kappa/massa)))
 	
 	do cyc = 0,nstep,1
 	
		time = 0.0_rk + ((nstep*dt)*cyc/nstep)
		posan = (((posan_0)*cos(sqrt(kappa/massa)*time)) + B*(cos(sqrt(kappa/massa)*time)))
		
		write(unit=3_ik,fmt=*)time,posan

	end do
	 	

 	it=0_ik
 	        						        ! step 0 : valori iniziali
 	write(unit=1_ik,fmt=*)it,it*dt,pos,vel
 	
 	epot =  0.5_rk * kappa * pos**2.0_rk
 	f    = - kappa * pos
 	ekin =  0.5_rk * massa * vel**2.0_rk
 	
 	
 	write(unit=2_ik,fmt=*)it,dt*it,ekin,epot,ekin+epot

 	do it = 1,nstep
 	
    		pos = pos + vel * dt + 0.5_rk* f/massa * dt**2.0
    		vel_parziale = vel + 0.5_rk * dt * f/massa             !  prima parte della formula per le velocita'
    		f    = - kappa * pos
    		epot =  0.5_rk * kappa * pos**2.0_rk
    		vel = vel_parziale + 0.5_rk * dt * f/massa             !  la formula per le velocita' viene completata qui
    		
    		write(unit=1_ik,fmt=*)it,it*dt,pos,vel
    		ekin = 0.5_rk * massa * vel**2.0_rk
    		write(unit=2_ik,fmt=*)it,it*dt,ekin,epot,ekin+epot
    		
 	end do
 	
end program harm
