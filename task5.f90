module funcs1
    implicit none
    integer, dimension(10, 10) :: arr
    common /matrix/ arr
    
contains
    subroutine pprint(n, m)
        integer, intent(in) :: n, m
        integer :: i, j
        do i = 1, n
            print *, (arr(i, j), j = 1, m) 
        end do
    end subroutine

    function sum_vec(a1, a2, lenarr) result(res)
        integer, intent(in) :: a1(:), a2(:)
        integer, intent(in) :: lenarr
        integer, dimension(lenarr) :: res
        integer :: i

        do i = 1, lenarr
            res(i) = a1(i) + a2(i)
        end do
    end function sum_vec

    subroutine sum_stolb(m, a1, a2, a3)
        integer, intent(in) :: m, a1, a2, a3
        arr(:, a3) = sum_vec(arr(:, a1), arr(:, a2), m)
    end subroutine sum_stolb

end module funcs1


program task5
    use funcs1
    implicit none

    integer :: n, m, i, j

    print *, '= enter matrix size into a line separated by space' 
    read *, n, m

    do i = 1, n
        print *, "= enter matrix elements of string", i,  "into a line separated by spaces"
        read *, (arr(i, j), j = 1, m)
    end do

    print *, '=== your matrix:'
    call pprint(n, m)

    call sum_stolb(m, 1, 2, 3)

    print *, '=== the final matrix:'

    call pprint(n, m)

end program task5

block data init
    implicit none
    integer, dimension(10, 10) :: arr
    common /matrix/ arr
    data arr / 100 * 0 /
end block data init

