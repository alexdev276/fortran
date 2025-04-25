module funcs1
    implicit none
    integer, dimension(10, 10) :: arr
    common /matrix/ arr
    
contains
    subroutine ap(n, m)
        integer, intent(in) :: n, m
        integer :: i, j
        do i = 1, n
            print *, (arr(i, j), j = 1, m) 
        end do
    end subroutine

    
end module funcs1


program task5
    use funcs1
    implicit none

    integer :: n, m, i, j

    print *, 'enter array size into a line separated by space' 
    read *, n, m

    do i = 1, n
        print *, "enter array elements of string", i,  "into a line separated by spaces"
        read *, (arr(i, j), j = 1, m)
    end do

    print *, 'your array:'
    call ap(n, m)

end program task5

block data init
    implicit none
    integer, dimension(10, 10) :: arr
    common /matrix/ arr
    data arr / 1, 2, 98 * 0 /
end block data init

