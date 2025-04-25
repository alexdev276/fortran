program str
    implicit none

    character(len=30) :: text = '   awdaw  ads    sad', norm_str
    integer :: trimmed_len, text_len, i
    logical :: flag = .false.

    text_len = len(text)
    trimmed_len = len_trim(text)
    print *, text, trimmed_len
    text = adjustr(text)
    text = adjustl(text)
    norm_str = ''
    do i = 1, trimmed_len
        if (i == 1 .and. text(i:i) /= ' ') then
            norm_str = trim(norm_str) // text(i:i)
        else if (i > 1 .and. text(i:i) /= ' ') then
            if (flag) then
                norm_str = trim(norm_str) // ' ' // text(i:i)
                flag = .false.
            else
                norm_str = trim(norm_str) // text(i:i)
            end if
            
        else
            flag = .true.
        end if
    end do

    print *, '!'//norm_str//'!'

end program str
