module module_matrice
  implicit none

  contains
    subroutine afficher(matrice,nb_col,nb_ligne)
      integer,intent(in)::nb_col,nb_ligne
      integer,dimension(nb_ligne,nb_col),intent(in)::matrice
      integer::i,j

      write(*,"(/,'Voici les entrées de la matrice :')")
      do i=1,nb_ligne
         do j=1,nb_col
            write(*,"(i4,'   ')",advance='no')matrice(i,j)
         end do
         write(*,*)
      end do
      return
    end subroutine afficher

    function creer_matrice(nb_ligne,nb_col)
      integer,intent(in)::nb_ligne,nb_col
      integer,dimension(:,:),allocatable::creer_matrice
      integer::i,j
      
      allocate(creer_matrice(nb_ligne,nb_col))

      write(*,"(/,'Merci de saisir (ligne par ligne) les entrées de la matrice :')")
      do i=1,nb_ligne
         write(*,"('Ligne : ',i1)")i
         do j=1,nb_col
            write(*,"('  col ',i1,' : ')",advance='no')j
            read *,creer_matrice(i,j)
         end do
      end do
      return
    end function creer_matrice

    function anti_transpose(matrice,N)
      integer,dimension(:,:),intent(in)::matrice
      integer,intent(in)::N
      integer,dimension(:,:),allocatable::anti_transpose
      integer::i,j
      allocate(anti_transpose(N,N))
      
      do i = 1,N
         do j =1,N
            anti_transpose(i,j)=matrice(mod(i+1,N),mod(j+1,N))
         end do
      end do
      
      return
    end function anti_transpose
  end module module_matrice
