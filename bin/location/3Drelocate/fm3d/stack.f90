module stack

  type Tnode
     integer                              :: value
     type(Tnode),pointer                  :: next
  end type Tnode

  type(Tnode),pointer                     :: head,t  
   
  type Tnode2
     integer                              :: value1,value2
     type(Tnode2),pointer                 :: next
  end type Tnode2

  type(Tnode2),pointer                    :: head2,t2  

end module stack

  subroutine stackinit
    use stack
    allocate(head)
    nullify(head%next) ; head%value = 0
  end subroutine stackinit

  subroutine push(p)
    use stack
    integer :: p
    allocate(t)
    t%next => head%next ; t%value = p
    head%next => t
    nullify(t)
  end subroutine push

  subroutine pop(x)
    use stack
    integer :: x
    t => head%next ; head%next => t%next
    x = t%value
    deallocate(t)
  end subroutine pop

  subroutine stackempty(i)
    use stack
    integer  :: i
    i=0
    if (.not.associated(head%next)) i=1
  end subroutine stackempty

  subroutine stackflush
    use stack
    deallocate(head)
  end subroutine stackflush


  subroutine stackpairinit
    use stack
    allocate(head2)
    nullify(head2%next) ; head2%value1 = 0; head2%value2 = 0
  end subroutine stackpairinit

  subroutine pushpair(p1,p2)
    use stack
    integer :: p1,p2
    allocate(t2)
    t2%next => head2%next ; t2%value1 = p1 ; t2%value2 = p2
    head2%next => t2
    nullify(t2)
  end subroutine pushpair

  subroutine poppair(x1,x2)
    use stack
    integer :: x1,x2
    t2 => head2%next ; head2%next => t2%next
    x1 = t2%value1 ; x2 = t2%value2 
    deallocate(t2)
  end subroutine poppair

  subroutine stackpairempty(i)
    use stack
    integer  :: i
    i=0
    if (.not.associated(head2%next)) i=1
  end subroutine stackpairempty

  subroutine stackpairflush
    use stack
    deallocate(head2)
  end subroutine stackpairflush

