!
! (c) Hacene Ouzia, UPMC France 
!
program listes
  use ModListe
  implicit none

  type(Donnee) :: D1,D2,D3,D4
  type(Liste) :: lst

  call system("clear")

  call setData()

 
  write(*,"(/,7x, 'TP-5 :: Tester Program liste ... ',/)")
 

  ! 1. CreerListe
  call creerListe(lst)
  ! 2. Ajouter des elements à la liste ... 
  call ajouter(lst,D1)
  call ajouter(lst,D2)
  call ajouter(lst,D3)
  call ajouter(lst,D4)
  ! 3. Afficher la liste courante 
  call printListe(lst)
  ! 4. Est-ce D1 est dans la liste ?
  if(chercherListe(lst,D1))then
     write(*,"('True')")
  else
     write(*,"('False')")
  end if
  
  ! 5. Est-ce D4 est dans la liste ?
  if(chercherListe(lst,D4))then
     write(*,"('True')")
  else
     write(*,"('False')")
  end if
  ! 6. Supprimer D2 de la liste 
  call remove(lst,D1)
  ! 7. Supprimer D4 de la liste 
  call remove(lst,D4)
  ! 8. Afficher la liste 
  call printListe(lst)
  ! 9. Vider la liste 
  call supprimer(lst)
  
  write(*,*)
  stop "Listes chainees"
contains

  subroutine setData()

    call setNumero(D1,1)
    call setNumero(D2,2)
    call setNumero(D3,3)
    call setNumero(D4,2)

    call setPoids(D1, 2.75)
    call setPoids(D2, 4.76)
    call setPoids(D3, 6.89)
    call setPoids(D4, 3.67)

    return 
  end subroutine setData

end program listes
