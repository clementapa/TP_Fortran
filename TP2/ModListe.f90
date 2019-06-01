!
! (c) Hacene Ouzia, UPMC France 
!
module ModListe
  use ModElement

  implicit none

  type Liste
     integer, private :: size
     type(Element), pointer, private :: head
  end type Liste

  interface creerListe
     module procedure creer_liste_vide, creer_liste_element, creer_liste_donnee, creer_liste_liste
  end interface creerListe

  interface chercherListe
     module procedure localiser_donnee, contient_donnee
  end interface chercherListe

  private :: setSize
  private :: localiser_donnee, contient_donnee
  private :: creer_liste_vide, creer_liste_element, creer_liste_donnee, creer_liste_liste

contains 

  subroutine todo(msg)
    implicit none
    character(len=*), intent(in) :: msg

    write(*,"(/,'Not implemened yet --> procdeure : ')", advance='no'); write(*,*) msg
    stop "You have a todo to do ... "
    return 
  end subroutine todo
  !<><><><><><><><><><><><><><><><><><><><>
  !<  CONSTRUCTORS
  !<><><><><><><><><><><><><><><><><><><><>
  function vide(argListe)
    implicit none
    type(Liste), intent(in) :: argListe
    logical :: vide
    if ( getSize(argListe)==0 ) then
       vide = .TRUE.
    else
       vide = .FALSE.
    end if
    return
  end function vide

  subroutine creer_liste_vide(argListe)
    implicit none 
    type(Liste) :: argListe

    call setSize(argListe,0)
    call nullifyHead(argListe)

    return 
  end subroutine creer_liste_vide

  subroutine creer_liste_element(argList, elem)
    type(Liste) :: argList
    type(Element),pointer, intent(in) :: elem

    call setSize(argList,1)
    call setHead(argList,elem)

    return
  end subroutine creer_liste_element

  subroutine creer_liste_donnee(argList,newData)
    type(Liste) :: argList
    type(Donnee), intent(in) :: newData

    call setSize(argList,1)
    call creerElement(getHead(argList),newData)

    return
  end subroutine creer_liste_donnee

  subroutine creer_liste_liste(argList, lst)
    type(Liste) :: argList
    type(Liste), pointer, intent(in) :: lst

    call setSize(argList,getSize(lst))
    call setHead(argList,getHead(lst))

    return
  end subroutine creer_liste_liste

  !<><><><><><><><><><><><><><><><><><><><>
  !<  METHOD: ADD
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine ajouter(argList,newData) 
    implicit none 
    type(Donnee),intent (in) :: newData
    type(Liste) :: argList
    type(Element),pointer :: temp,elem

    call creerElement(elem,newData)
    if(vide(argList))then
       call setHead(argList,elem)
       call setSize(argList,1)
    else
       temp=>getHead(argList)
       do while(possedeFils(temp))
          call elementSuivant(temp,temp)
       end do
       call setFils(temp,elem)
       call setSize(argList,getSize(argList)+1)
    end if
    return 
  end subroutine ajouter

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: DESTROY
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine supprimer(argList)
    implicit none 
    type(Liste) :: argList
    type(Element), pointer :: nextElem, currentElem

    if(vide(argList))then
       write(*,"('Liste vide')")
    else   
       currentElem=>getHead(argList)
       do while(possedeFils(currentELem))
          call elementSuivant(currentElem,nextElem)
          nullify(currentElem)
          currentElem=>nextElem
       end do
       nullify(currentElem)
    end if
    call nullifyhead(argList)
    call setSize(argList,0)
    return
  end subroutine supprimer

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: PRINT
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine printListe(argList)
    implicit none 
    type(Liste), intent(in) :: argList
    type(Element), pointer :: currentElem

    if(vide(argList))then
       write(*,"('Liste vide')")
    else   
       currentElem=>getHead(argList)
       do while(possedeFils(currentELem))
          call printElement(currentElem)
          call elementSuivant(currentElem,currentElem)
       end do
       call printElement(currentElem)
    end if
    return
  end subroutine printListe

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: FIRST
  !<><><><><><><><><><><><><><><><><><><><>
  function first(arglist)
    implicit none
    type(Liste), intent(in) :: argList
    type(Element), pointer :: first

    first => getHead(argList)

    return
  end function first

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: FIND
  !<><><><><><><><><><><><><><><><><><><><>
  function contient_donnee(argList, info)
    implicit none
    type(Donnee), intent (in) :: info
    type(Liste) :: argList
    logical :: contient_donnee
    
    type(Element), pointer :: currentElem
    contient_donnee=.false.
    
    if(vide(argList))then
       write(*,"('Liste vide')")
    else   
       currentElem=>getHead(argList)
       do while(possedeFils(currentELem))
          if(getDonnee(currentElem).EQ.info)then
             contient_donnee=.true.
             return
          end if
          call elementSuivant(currentElem,currentElem)
       end do
       if(getDonnee(currentElem).EQ.info)then
          contient_donnee=.true.
          return
       end if
    end if
    
    return 
  end function contient_donnee

  function localiser_donnee(argList,info,prev,elem)
    implicit none
    type(Liste) :: arglist
    type(Donnee), intent(in) :: info
    type(Element), pointer :: elem, next, prev

    logical :: localiser_donnee

    call todo("localiser_donnee")

    return 
  end function localiser_donnee

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHOD: REMOVE
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine remove(argList, info)
    type(Liste) :: argList
    type(Donnee) :: info
    type(Element), pointer :: elem, prev

    if(vide(argList))then
       write(*,"('Liste vide')")
    else
       if(getDonnee(getHead(argList)).EQ.info)then
          prev=>getHead(argList)
          elem=>getFils(prev)
          nullify(prev)
          call setSize(arglist,getSize(arglist)-1)
          call setHead(argList,elem)
       else
          write(*,"('la')")
          elem=>getHead(argList)
          do while(possedeFils(elem).and..not.(getDonnee(getFils(elem)).EQ.info))
             call elementSuivant(elem,elem)
             write(*,"('la')")
          end do
          if(getDonnee(getFils(elem)).EQ.info)then
             write(*,"('la')")
             prev=>elem
             call elementSuivant(elem,elem)
             call setFils(prev,getFils(elem))
             nullify(elem)
             return
          end if
       end if
    end if

    return    
  end subroutine remove

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: GETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  function getSize(argListe)
    implicit none
    type(Liste), intent(in) :: argListe
    integer :: getSize

    getSize = argListe%size
    return 
  end function getSize

  function getHead(argListe)
    type(Liste), intent(in) :: argListe
    type(Element), pointer :: getHead

    getHead => argListe % head
    return 
  end function getHead

  !<><><><><><><><><><><><><><><><><><><><>
  !< METHODS: SETTERS
  !<><><><><><><><><><><><><><><><><><><><>
  subroutine setSize(argListe,valeur)
    implicit none
    integer, intent(in) :: valeur
    type(Liste) :: argListe

    argListe%size = valeur

    return
  end subroutine setSize

  subroutine setHead(argListe, Elem)
    implicit none
    type(Liste) :: argListe
    type(Element), pointer, intent(in) :: Elem

    if( .NOT. associated( Elem ) ) then 
       call ElementPointerNotAssociatedError("setHead")
    else
       argListe % head => Elem
    end if

    return
  end subroutine setHead

  subroutine nullifyHead(argList)
    implicit none
    type(Liste) :: argList

    nullify(argList % head)

    return
  end subroutine nullifyHead

end module ModListe
