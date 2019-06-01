program suite
  implicit none

  integer::i,N
  real:: u1,u0,u2
  write(*,"('Veuillez indiquer le rang N:')",advance='no')
  read(*,*)N
  write(*,*)

  u0=3
  u1=sqrt(3.0)

  do i=0,N-2
     u2=0.5*u1+sqrt(1+cos(u0))
     u0=u1
     u1=u2
     write(*,"('  Iteration  ',i3,'  :',F5.2)")i+1,u2
  end do

  write(*,*)
  write(*,"(' Le ',i3,'-eme terme vaut :   ',F5.2,/)")N,u2

  stop "Fin recurrence ..."
end program suite
  
