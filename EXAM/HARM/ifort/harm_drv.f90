! HARM_DRV - v. 1.6
! A simple computational model for a Driven Harmonic Oscillator (using the Euler-Cromer algorithm).
! (C) Emanuele Ballarin (ehteqx@gmail.com) -- 20/06/2015 # 18.52
!###################################################################################################

PROGRAM HARM_DRV

	implicit none

! # PARAMETERS #

	integer, parameter 	:: ik = selected_int_kind(18)		! MAX: 38
	integer, parameter 	:: rk = selected_real_kind(33)		! MAX: 33
	real (kind = rk), parameter		:: gravity = 9.80665_rk	! As recommended in CODATA 2010
	real (kind = rk), parameter		:: pi = acos(-1.0_rk)	! Pi

	real (kind = rk), parameter		:: om = 1.0_rk			! Given value
	real (kind = rk), parameter		:: a = 0.5_rk			! Given value
!	real (kind = rk), parameter		:: a = 0.0_rk			! Given value (0.0 for conservative system)
	real (kind = rk), parameter		:: f = 1.15_rk			! Given value
!	real (kind = rk), parameter		:: f = 0.0_rk			! Given value (0.0 for conservative system)
	real (kind = rk), parameter		:: w = 0.6_rk			! Given value

	real (kind = rk), parameter		:: length = (gravity/(om**2))		! Pendulum lenght (from given parameters)
	real (kind = rk), parameter		:: mass = 1.0_rk					! "Standard" pendulum mass

! # VARIABLES #

	real (kind = rk)	:: theta, omega, alpha 	! Angular position, velocity, acceleration
	real (kind = rk)	:: dt, time = 0.0_rk 	! Timestep (OPTIMAL: 0.00015), elapsed time (initial: 0.0)
	real (kind = rk)	:: ekin, epot, energy 	! Kinetic energy, potential energy, total energy

	integer (kind = ik) :: nstep, it = 0_ik		! Number of iterations, counter (initial: 0)

! # SOME COSMETICS #

	print*, ' '
	print*, '######################################################################'
	print*, '                          HARM_DRV - v. 1.6                           '
	print*, '     A simple computational model for a Driven Harmonic Oscillator    '
	print*, '                  (using the Euler-Cromer algorithm)                  '
	print*, '                                                                      '
	print*, '                 Copyright (C) 2015 Emanuele Ballarin                 '
	print*, '                                                                      '
	print*, 'HARM_DRV is free software, covered by the GNU General Public License v3,'
	print*, 'and comes with ABSOLUTELY NO WARRANTY WHATSOEVER.                     '
	print*, 'For more information about the license: https://www.gnu.org/licenses/ '
	print*, '######################################################################'
	print*, ' '

! # SETTINGS #

	dt = -1.0_rk				! Necessary for the 'do while' statement to be false
	do while (dt .LE. 0.0_rk)
		write(unit=*,fmt="(a)",advance="no")"Timestep value (must be positive; Recommended: 0.00015): "	! Data as characters and
 		read*, dt																						! suppress newline command
 	end do

 	nstep = -1.0_ik				! Necessary for the 'do while' statement to be false
 	do while (nstep .LE. 0.0_rk)
 		write(unit=*,fmt="(a)",advance="no")"Number of steps (must be integer, positive; Standard: 800000): "	! Data as characters and
 		read*, nstep																							! suppress newline command
 	end do

 	write(unit=*,fmt="(a)",advance="no")"Initial angular position: "				! Data as characters and
 	read*, theta																	! suppress newline command

 	write(unit=*,fmt="(a)",advance="no")"Initial angular velocity: "				! Data as characters and
 	read*, omega																	! suppress newline command

	print*, ' '								! Some info for the user
	print*, 'Computation started...'		! Some info for the user

! # EXTRA CALCULATIONS #

	time = it*dt		! Time

	alpha = (-((om)**2)*(sin(theta))) - (a*(omega)) + (f*(cos(w*time))) ! Angular acceleration (equation given)

	epot = (length*mass*gravity*(1.0_rk - cos(theta)))		! Potential energy

	ekin = ((0.5_rk)*mass*((length*omega)**2))				! Kinetic energy

	energy = (epot + ekin)

! # WRITING TO FILE #

	! Creating files
		open(unit=1, file='motion.dat')
		open(unit=2, file='energy.dat')
		open(unit=3, file='traject.dat')

	! Writing motion data
		write(unit=1,fmt=*)it, time, theta, omega, alpha

	! Writing energy data
		write(unit=2,fmt=*)it, time, ekin, epot, energy

	! Writing trajectory data
		write(unit=3,fmt=*)it, time, (length*sin(theta)), (length*(1.0_rk - cos(theta)))		! x and y coordinates

! # INTEGRATION (USING THE EULER-CROMER ALGORITHM) #

	do it = 1,nstep,1

	! Obtaining the new values for angular velocity and position
		omega = (omega + (dt*alpha))		! The angular velocity is integrated using the ang. acceleration
		theta = (theta + (dt*omega))		! The angular position is integrated using the ang. velocity

	! Calculating the new values of the other variables
		time = it*dt		! Time

		alpha = (-((om)**2)*(sin(theta))) - (a*(omega)) + (f*(cos(w*time))) ! Angular acceleration (given equation)

		epot = (length*mass*gravity*(1.0_rk - cos(theta)))		! Potential energy

		ekin = ((0.5_rk)*mass*((length*omega)**2))				! Kinetic energy

		energy = (epot + ekin)

	! Writing motion data
		write(unit=1,fmt=*)it, time, theta, omega, alpha

	! Writing energy data
		write(unit=2,fmt=*)it, time, ekin, epot, energy

	! Writing trajectory data
		write(unit=3,fmt=*)it, time, (length*sin(theta)), (length*(1.0_rk - cos(theta)))

	end do

	print*, ' '								! Some info for the user
	print*, 'Computation completed!'		! Some info for the user
	print*, ' '

END PROGRAM HARM_DRV
