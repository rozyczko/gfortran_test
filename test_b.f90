program test_b

    implicit none

    type tipo_A
        integer, dimension(:), allocatable :: arr
    end type

    type, extends(tipo_A) :: tipo_B
        integer :: m
    end type

    type tipo_L
        integer :: n
        class(tipo_A), dimension(:), allocatable :: r
    end type

    integer :: i,n
    type(tipo_L) :: l

    do i = 1 , 2
        write(*,'(a,i1)') 'Iteration ',i
        write(*,'(4x,a)') ' => Calling init_tipo_L'
        call init_tipo_L(100,l,'b')
        write(*,'(4x,a)') ' => Calling modify_tipo_L'
        call modify_tipo_L(50,l)
    end do

    contains

    subroutine init_tipo_L(n,l,tipo)

        ! Arguments
        integer, intent(in) :: n
        type(tipo_L), intent(inout) :: l
        character(len=*), intent(in) :: tipo

        ! Local
        integer :: ierr
        type(tipo_A) :: src_a
        type(tipo_B) :: src_b

        write(*,'(4x,a)') 'Entering init_tipo_L'
        if (allocated(l%r)) then
            write(*,'(8x,a)') 'l%r was allocated. Deallocating it...'
            deallocate(l%r,stat=ierr)
            write(*,'(8x,a,i1)') 'Deallocating stat = ',ierr
        end if
        l%n = n
        select case (tipo)
        case('b')
            write(*,'(8x,a)') 'Allocating r with type b'
            allocate(l%r(n), source=src_b)
        case('a')
            write(*,'(8x,a)') 'Allocating r with type a'
            allocate(l%r(n), source=src_a)
        end select
        write(*,'(4x,a)') 'Exiting init_tipo_L'

    end subroutine init_tipo_L

    subroutine modify_tipo_L(n,l)

        ! Arguments
        integer, intent(in) :: n
        type(tipo_L), intent(inout) :: l

        ! Local
        integer :: ierr

        write(*,'(4x,a)') 'Entering modify_tipo_L'
        select type (arr => l%r)
            type is (tipo_A)
                write(*,'(4x,a)') 'Calling init_tipo_L with type = tipo_A'
                call init_tipo_L(n,l,'a')
            type is (tipo_B)
                write(*,'(4x,a)') 'Calling init_tipo_L with type = tipo_B'
                call init_tipo_L(n,l,'b')
            write(*,'(4x,a)') 'Exiting modify_tipo_L'
        end select

    end subroutine modify_tipo_L

end program