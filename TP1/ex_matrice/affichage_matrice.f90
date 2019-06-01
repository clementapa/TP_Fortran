program affichage_matrice
  use module_matrice
  
  implicit none
  
  integer::nb_col,nb_ligne
  integer,dimension(:,:),allocatable::matrice
  
  write(*,"('Taille de la matrice:')")
  write(*,"('Nombre de lignes  :  ')",advance='no')
  read *, nb_ligne
  write(*,"('Nombre de colonnes  :  ')",advance='no')
  read *, nb_col

  matrice=creer_matrice(nb_col,nb_ligne)
  call afficher(matrice,nb_col,nb_ligne)
  
  deallocate(matrice)
  
  stop "Fin de affiche_matrice"
end program affichage_matrice
