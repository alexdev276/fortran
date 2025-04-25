module funcs
contains
    subroutine pprint(arr, lenarr)
        implicit none
        integer, intent(in) :: arr(:), lenarr
        print *, arr(1:lenarr)
    end subroutine pprint

    function create(lenarr) result(arr)
        implicit none
        integer, intent(in) :: lenarr
        integer, dimension(lenarr) :: arr

        integer :: i


        print *, 'enter array elements into a line separated by spaces'
        read *, (arr(i), i = 1, lenarr)
    end function create

    function dotask(arr, lenarr) result (resarr)
        implicit none
        integer, intent(in) :: arr(:), lenarr
        integer, dimension(lenarr) :: resarr
    
        integer :: mid, r1, r2, temp

        mid = lenarr / 2
        r1 = search_max(arr, 1, mid)
        
        if (mod(lenarr, 2) == 0) then
            r2 = search_max(arr, mid + 1, lenarr)
        else
            r2 = search_max(arr, mid + 2, lenarr)
        end if

        resarr = arr(:)
        temp = resarr(r1)
        resarr(r1) = resarr(r2)
        resarr(r2) = temp

    end function dotask

    function search_max(arr, left, right) result(res)
        integer, intent(in) :: arr(:), left, right
        integer :: res

        integer :: i, m
        m = 0

        do i = left, right
            if (arr(i) > m) then
                m = arr(i)
                res = i
            end if
        end do

    end function search_max
end module funcs

program task4

    use funcs

    implicit none
    integer :: comm, lenarr, temp_comm
    logical :: f, arrcreated
    integer, dimension(10000) :: arr, temp_arr

    arrcreated = .false.
    f = .true.

    do while (f .eqv. .true.)
        print *, '0 - exit program'
        print *, '1 - create array'
        print *, '2 - print array'
        print *, '3 - search max elements in left and right parts of array and switch their'
        read *, comm
        select case (comm)
        case (0)
            f = .false.
        case (1)
            arrcreated = .true.
            print *, 'enter the length of array'
            read *, lenarr
            arr = create(lenarr)
        case (2)
            if (arrcreated .eqv. .true.) then
                call pprint(arr, lenarr)
            else 
                print *, 'array has not created yet'
            end if
        case (3)
            if (arrcreated .eqv. .true.) then
                print *, 'do you want change the main array?'
                print *, 'enter 1 if yes, or 0 if not'

                read *, temp_comm

                if (temp_comm == 1) then
                    arr = dotask(arr, lenarr)
                    call pprint(arr, lenarr)
                else
                    temp_arr = dotask(arr, lenarr)
                    call pprint(temp_arr, lenarr)
                end if
            else 
                print *, 'array has not created yet'
            end if
        end select
    end do    
end program task4

