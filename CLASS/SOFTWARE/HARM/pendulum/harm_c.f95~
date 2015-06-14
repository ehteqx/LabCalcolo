program harm_c
 
 	implicit none
	
	integer,parameter :: ik = selected_int_kind(8)
	integer,parameter :: rk = selected_real_kind(8)

 	real (kind = rk)    :: lunghezza=1.0_rk, agrav=9.81_rk 	! valori di default per lunghezza del pendolo e ac. di gravita'
 	real (kind = rk)   :: dt, ekin, epot			! OPTIMAL TIMESTEP: ????? (Recommended)
 	real (kind = rk)    :: agl, vang, vang_parziale, mixedt, massa
 	integer (kind = ik) :: nstep, it
 	
 	write(unit=*,fmt="(a)",advance="no")"delta t : "        ! il formato (a) chiede che il dato sia trattato come 
 	read*,dt   
 	                                        		! caratteri e advance="no"  sopprime l'  inserimento del 
 	write(unit=*,fmt="(a)",advance="no")"n.step: "          ! caratter "a capo"  alla fine della linea per cui 
 	read*,nstep    
 	 	
 	write(unit=*,fmt="(a)",advance="no")"massa : "        	! il formato (a) chiede che il dato sia trattato come 
 	read*,massa   
 	                                    			! la prossima operazione di lettura/scrittura inziera'
 	write(unit=*,fmt="(a)",advance="no")"lunghezza: "       ! sulla stessa riga di schermo di quella corrente
 	read*,lunghezza
 	
 	write(unit=*,fmt="(a)",advance="no")"agrav: "
 	read*,agrav
 	
 	write(unit=*,fmt="(a)",advance="no")"agl(0): "
 	read*,agl
 	
 	write(unit=*,fmt="(a)",advance="no")"vang(0): "
 	read*,vang
 	
 	it=0_ik
 	
 	write(unit=1,fmt=*)it,it*dt,agl,vang
 	
 	epot =  lunghezza*massa*agrav*(1_ik-cos(agl))
 	mixedt = - agrav * sin(agl)
 	ekin =  (0.5_rk)*massa*(lunghezza*vang)**2
 	

 	write(unit=2,fmt=*)it,dt*it,ekin,epot,ekin+epot

 	do it = 1,nstep
 	
    		agl = agl + vang * dt + 0.5_rk * mixedt/lunghezza * dt**2
    		vang_parziale = vang + 0.5_rk * dt * mixedt/lunghezza       !  prima parte della formula per le velocita'
    		mixedt    = - agrav * sin(agl)
    		epot =  lunghezza*massa*agrav*(1_ik-cos(agl))
    		vang = vang_parziale + 0.5_rk * dt * mixedt/lunghezza       !  la formula per le velocita' viene completata qui
    		
    		write(unit=1,fmt=*)it,it*dt,agl,vang
    		ekin = (0.5_rk)*massa*(lunghezza*vang)**2
    		write(unit=2,fmt=*)it,it*dt,ekin,epot,ekin+epot
    		
 	end do
 	
end program harm_c
