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

    

    subroutine apo()
        integer :: qq(90), mas(3), q(7)
        common /matrix/ qq, mas, q
        mas(1) = 1000
        mas(2) = 1000
        mas(3) = 1000

    end subroutine

    

end module funcs1


program task5
    use funcs1
    implicit none

    integer :: n, m, i, j, r1, r2, a1, a2

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

    print *, '== 10th stolbec'


    r1 = 1
    r2 = 2
    a1 = 1
    a2 = 1
    if (arr(r1, a1) > arr(r2, a2)) then
        r1 = 2
        r2 = 1
    end if
    do i = 1, n
        do j = 1, m
            if (arr(i, j) < arr(r1, a1)) then
                r2 = r1
                a2 = a1
                r1 = i
                a1 = j
            else if (arr(i, j) > arr(r1, a1) .and. arr(i, j) < arr(r2, a2)) then
                r2 = i
                a2 = j
            end if
        end do
    end do
    
    print *, '== indexes of minimal elements:'
    print *, a1, a2

    print *, 'dop zadanie'
    call apo()
    print *, arr(1, 10), arr(2, 10), arr(3, 10)
    
end program task5

block data init
    implicit none
    integer, dimension(10, 10) :: arr
    common /matrix/ arr
    data arr / 100 * 0 /
end block data init

