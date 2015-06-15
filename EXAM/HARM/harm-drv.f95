! HARM-DRV - v. 0.1 alpha
! A simple computational model for a Driven Harmonic Oscillator
! (using the Euler-Cromer algorithm).
! (C) Emanuele Ballarin - 15/06/2015
!###############################################################################

PROGRAM HARM-DRV

	implicit none
	
! # PARAMETERS # 
	
	integer, parameter 	:: ik = selected_int_kind(8)
	integer, parameter 	:: rk = selected_real_kind(8)
	real (kind = rk), parameter		:: gravity = 9.80665_rk	! As recommended in CODATA 2010
	
	real (kind = rk), parameter		:: om = 1.0_rk			! Given value
	real (kind = rk), parameter		:: a = 0.5_rk			! Given value (0.0 for conservative system)
	real (kind = rk), parameter		:: f = 1.15_rk			! Given value (0.0 for conservative system)
	real (kind = rk), parameter		:: w = 0.6_rk			! Given value

	real (kind = rk), parameter		:: length = 1.0_rk		! "Standard" pendulum lenght
	real (kind = rk), parameter		:: mass = 1.0_rk		! "Standard" pendulum mass

! # VARIABLES # 
	
	real (kind = rk)	:: theta, omega, alpha 	! Angular position, velocity, acceleration
	real (kind = rk)	:: dt, time = 0.0_rk 	! Timestep (OPTIMAL: x.xxx), elapsed time
	real (kind = rk)	:: ekin, epot 			! Kinetic energy, potential energy
	
	integer (kind = ik) :: nstep, it			! Number of iterations, counter
	
! # SETTINGS # 
	
	write(unit=*,fmt="(a)",advance="no")"Timestep value : "				! Data as characters and
 	read*, dt															! suppressed newline command

 	write(unit=*,fmt="(a)",advance="no")"Number of steps: "				! Data as characters and
 	read*, nstep														! suppressed newline command    

 	write(unit=*,fmt="(a)",advance="no")"Initial angular position: "	! Data as characters and
 	read*, theta														! suppressed newline command

 	write(unit=*,fmt="(a)",advance="no")"Initial angular velocity: "	! Data as characters and
 	read*, omega														! suppressed newline command











END PROGRAM HARM-DRV
