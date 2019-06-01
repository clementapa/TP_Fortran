program anti_trans
  use module_matrice
  implicit none

  integer,dimension(:,:),allocatable::matrice1,matrice2
  integer::N,malloc,i,j
  N=3
  allocate(matrice1(N,N),stat=malloc)
  if(malloc>0)then
     stop "erreur"
  end if
  
  do i=1,N
     do j=1,N
        matrice1(i,j)=floor(rand()*50+1)
     end do
  end do
  
  call afficher(matrice1,N,N)
  matrice2=anti_transpose(matrice1,N)
  call afficher(matrice2,N,N)
  
  stop "FIN programme"
end program anti_trans
