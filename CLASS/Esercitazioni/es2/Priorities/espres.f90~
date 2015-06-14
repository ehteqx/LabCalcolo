program espres
  implicit none
  real      :: ra,rb,rc,rd,re,rf,rg,rh,ri,rs
  integer   :: ip,iq,ir,is,iu,iw,ix,iy,ij
  logical   :: les1,les2,les3

  ra = 2
  rb = 1
  rc = 3.0
  ip = 2
  iq = 3
  rd = rb/ra + rc
  re = rb/rc + ra
  rf = ip/iq
  rg = ip/iq*iq/ip 
  ir = ra/rc*rc/ra 
  iu = (ra/rc)*(rc/ra) 
  iw = 8**(1/3) 
  rh = 4.7 
  ri = 10.2 
  ix = rh 
  iy = ri 
  ij = -4.7 
  les1 = ij < -4.0
  les2 = .true. .and. (3*(1/3) == 1)      
! il risultato di un' operazione di AND logico 
! e' vero se e solo se sono veri tutti e due gli operandi

  les3 = (1<2) .or. (3<=3) .and. (-5>-4.0) ! chi ha precedenza tra un .or.  e un .and.  ?

  print *," in questo programma nomi che iniziano per r indicano variabili reali"
  print *," nomi che iniziano per i indicano variabili intere, con l logiche"
  print*," ra = 2: ra = ", ra
  print*," rb = 1: rb = ", rb
  print*," rc = 3.0: rc = ", rc
  print*," ip = 2: ip = ", ip
  print*," iq = 3: iq = ", iq

  print *," rd = rb/ra + rc = ",rd 
  print *," re = rb/rc + ra = ",re 
  print *," rf = ip/iq = ",rf 
  print *," rg = ip/iq*iq/ip = ",rg 
  print *," ir = ra/rc*rc/ra = ",ir 
  print *," iu = (ra/rc)*(rc/ra) = ",iu 
  print *," is = ",is , " rs = ", rs, & 
         " (variabili dichiarate ma non definite nel programma) "
  print *," iw = 8**(1/3) = ",iw 
  print *," ix = rh: ix = ",ix 
  print *," iy = ri: iy = ",iy 
  print *," ij = -4.7:  ij = ",ij 
  print *,"2**3**2 = ", 2**3**2," quale potenza viene calcolata per prima ?"

  print *,"les1 =  ij < -4.0  :  = ", les1
  print *,"les2 =  .true. .and. (3*(1/3) == 1)  :  = ",les2
  print *,"les3 = (1<2) .or. (3<=3) .and. (-5>-4.0)  :  = ",les3

end program espres
