program harmsq
 
 	implicit none
	
	integer,parameter :: ik = selected_int_kind(18)
	integer,parameter :: rk = selected_real_kind(32)
	
	real (kind = rk) :: massa=1.0_rk, dt
	real (kind = rk), dimension(2) :: kappa, pos, vel, vel_parziale, f, omega
	real (kind = rk), dimension(2) :: ekinc, epotc
	real (kind = rk) :: epot, ekin, ftot, post, velt
	integer (kind = ik) :: it, nstep
	! OPTIMAL TIMESTEP: 0.015 (Recommended)
	
 	
 	write(unit=*,fmt="(a)",advance="no")"delta t : "  	! il formato (a) chiede che il dato sia trattato come 
 	read*,dt                                   			! caratteri e advance="no"  sopprime l'inserimento del 
 	write(unit=*,fmt="(a)",advance="no")"n.step: "      ! carattere "a capo"  alla fine della linea. 
 	read*,nstep    
 	                                    				! La prossima operazione di lettura/scrittura inziera'
 	write(unit=*,fmt="(a)",advance="no")"massa: "       ! sula stessa riga di schermo di quella corrente.
 	read*,massa
 	 	
 	
 	!VALORI LUNGO X -----------------------------------------
 	write(unit=*,fmt="(a)",advance="no")"omega_x: "
 	read*,omega(1)
 	kappa(1) = ((omega(1))**2)
 	
 	write(unit=*,fmt="(a)",advance="no")"pos(0)_x: "
 	read*,pos(1)
 	
 	write(unit=*,fmt="(a)",advance="no")"vel(0)_x: "
 	read*,vel(1)
 	
 	!VALORI LUNGO Y -----------------------------------------
 	write(unit=*,fmt="(a)",advance="no")"omega_y: "
 	read*,omega(2)
 	kappa(2) = ((omega(2))**2)
 	
 	write(unit=*,fmt="(a)",advance="no")"pos(0)_y: "
 	read*,pos(2)
 	
 	write(unit=*,fmt="(a)",advance="no")"vel(0)_y: "
 	read*,vel(2)

		
 	!---------------------------------------------------------------------------
 	! STEP LUNGO COMPONENTI !
 	!---------------------------------------------------------------------------
 	
 	! - Componente X
 	
 	it=0_ik
 	
 	write(unit=1_ik,fmt=*)it,it*dt,pos(1),vel(1)		! valori lungo x INIZIALI
 	 	
 	epotc(1) =  0.5_rk * kappa(1) * pos(1)**2.0_rk
 	f(1)    = - kappa(1) * pos(1)
 	ekinc(1) =  0.5_rk * massa * vel(1)**2.0_rk
 	
 	
 	write(unit=2_ik,fmt=*)it,dt*it,ekinc(1),epotc(1),ekinc(1)+epotc(1)	! valori lungo x INTERMEDI
 	
 	
 	! - Componente Y
 	
 	write(unit=3_ik,fmt=*)it,it*dt,pos(2),vel(2)		! valori lungo y INIZIALI
 	 	
 	epotc(2) =  0.5_rk * kappa(2) * pos(2)**2.0_rk
 	f(2)    = - kappa(2) * pos(2)
 	ekinc(2) =  0.5_rk * massa * vel(2)**2.0_rk
 	
 	
 	write(unit=4_ik,fmt=*)it,dt*it,ekinc(2),epotc(2),ekinc(2)+epotc(2)	! valori lungo y INTERMEDI
 	
!------------------------------------------------------------------------------- 	 	
 	
 	! - LUNGO TUTTE LE COMPONENTI (START)
 	post = sqrt((pos(1))**2 + ((pos(2))**2))
 	velt = sqrt((vel(1))**2 + ((vel(2))**2))
 	ftot = sqrt((f(1))**2 + ((f(2))**2))
 	
 	write(unit=7_ik,fmt=*)it,it*dt,post,velt
 	write(unit=99_ik,fmt=*)it,dt*it,pos(1),pos(2)
 	! - LUNGO TUTTE LE COMPONENTI (STOP)
 	
 	! - LUNGO TUTTE LE COMPONENTI (START)
 	ekin = ekinc(1) + ekinc(2)
 	epot = epotc(1) + epotc(2)
 	
 	write(unit=8_ik,fmt=*)it,dt*it,ekin,epot,ekin+epot
 	! - LUNGO TUTTE LE COMPONENTI (STOP) 	
 	
!------------------------------------------------------------------------------- 	
 	
 	! CICLO DI INTEGRAZIONE (X)
 	
 	do it = 1,nstep
 	
    		pos(1) = pos(1) + vel(1) * dt + 0.5_rk* f(1)/massa * dt**2.0
    		vel_parziale(1) = vel(1) + 0.5_rk * dt * f(1)/massa             !  prima parte della formula per le velocita'
    		f(1)    = - kappa(1) * pos(1)
    		epotc(1) =  0.5_rk * kappa(1) * pos(1)**2.0_rk
    		vel(1) = vel_parziale(1) + 0.5_rk * dt * f(1)/massa           !  la formula per le velocita' viene completata qui
    		
    		write(unit=1_ik,fmt=*)it,it*dt,pos(1),vel(1)						! valori lungo x FINALI
    		
    		 	! CICLO DI INTEGRAZIONE (Y)
 	
 	
    		pos(2) = pos(2) + vel(2) * dt + 0.5_rk* f(2)/massa * dt**2.0
    		vel_parziale(2) = vel(2) + 0.5_rk * dt * f(2)/massa             !  prima parte della formula per le velocita'
    		f(2)    = - kappa(2) * pos(2)
    		epotc(2) =  0.5_rk * kappa(2) * pos(2)**2.0_rk
    		vel(2) = vel_parziale(2) + 0.5_rk * dt * f(2)/massa          !  la formula per le velocita' viene completata qui
    		
    		
    		write(unit=3_ik,fmt=*)it,it*dt,pos(2),vel(2)						! valori lungo y FINALI
    		
    		! - LUNGO TUTTE LE COMPONENTI (START)
 	post = sqrt((pos(1))**2 + ((pos(2))**2))
 	velt = sqrt((vel(1))**2 + ((vel(2))**2))
 	ftot = sqrt((f(1))**2 + ((f(2))**2))
 	
 	write(unit=7_ik,fmt=*)it,it*dt,post,velt
 	write(unit=99_ik,fmt=*)it,dt*it,pos(1),pos(2)
 	! - LUNGO TUTTE LE COMPONENTI (STOP)
    		
    		
    		
    		ekinc(1) = 0.5_rk * massa * vel(1)**2.0_rk
    		write(unit=2_ik,fmt=*)it,dt*it,ekinc(1),epotc(1),ekinc(1)+epotc(1)	! valori lungo x FINALI
    		
    		ekinc(2) = 0.5_rk * massa * vel(2)**2.0_rk
    		write(unit=4_ik,fmt=*)it,dt*it,ekinc(2),epotc(2),ekinc(2)+epotc(2)	! valori lungo y FINALI 	
 	
 	! - LUNGO TUTTE LE COMPONENTI (START)
 	ekin = ekinc(1) + ekinc(2)
 	epot = epotc(1) + epotc(2)
 	
 	write(unit=8_ik,fmt=*)it,dt*it,ekin,epot,ekin+epot
 	! - LUNGO TUTTE LE COMPONENTI (STOP)

 	end do
 	 	
end program harmsq
