program test
    
    implicit none

    integer  :: comm, temp_comm

    comm = 1
    
    do while (comm /= 0)

        print *, '                              |'
        print *, '         _____________________|___________________'
        print *, '         |                    |                  |'
        print *, '        (0)                  (1)                (2)'
        print *, 'завершение работы       найти интеграл      найти корень'
        

        
        read *, comm


        if (comm == 1) then
            
            call iopprint()

            read *, temp_comm


            if (temp_comm == 1) then
                call integral_1()
            else if (temp_comm == 2) then
                call integral_2()
            end if

        else if (comm == 2) then
            call iopprint()

            read *, temp_comm

            if (temp_comm == 1) then
                call sqrt_1()
            else if (temp_comm == 2) then
                call sqrt_2()
            end if
        end if
    end do
contains 
    subroutine main()
        
    end subroutine main


    subroutine iopprint()
        print *, '                  |'
        print *, '                  |'
        print *, '                 / \'
        print *, '                /   \'
        print *, '       ________/     \________'
        print *, '       |                     |'
        print *, '      (1)                   (2)'
        print *, ' первый способ         второй способ'
    end subroutine iopprint

    subroutine integral_1()
        implicit none
        real :: a, b, h, sum, x, square
        integer :: i, n
    
        10 print *, 'Введите нижнюю границу интегрирования:'
        read *, a
        print *, 'Введите верхнюю границу интегрирования:'
        read *, b
        print *, 'Введите количество отрезков:'
        read *, n
    
        if (n == 0) then 
            print *, ' количество отрезков не должно быть равно 0'
            goto 10
        end if
        h = (b - a) / n
        sum = f(a) + f(b)
        square = f(a) + f(b)
        do i = 1, n - 1
            x = a + i * h
            sum = sum + 2 * f(x)
            square = square + abs(2 * f(x))
        end do
    
        sum = sum * h / 2
        square = square * h / 2
    
        print *, 'Значение интеграла:', sum
        print *, 'Площадь: ', square
    end subroutine integral_1

    

    subroutine integral_2()
        implicit none
        real :: a, b, h, sum, x, square
        integer :: i, n

        
        20 print *, 'Введите нижнюю границу интегрирования:'
        read *, a
        print *, 'Введите верхнюю границу интегрирования:'
        read *, b
        print *, 'Введите количество отрезков (четное число):'
        read *, n

        if (n == 0) then 
            print *, ' количество отрезков не должно быть равно 0'
            goto 20
        end if


        if (mod(n, 2) /= 0) then
            print *, 'Количество отрезков должно быть четным!'
            goto 20
        end if

        h = (b - a) / n

        sum = f(a) + f(b)
        square = f(a) + f(b)
        do i = 1, n - 1
            x = a + i * h
            if (mod(i, 2) == 0) then
                sum = sum + 2 * f(x)
                square = square + abs(2 * f(x))
            else
                sum = sum + 4 * f(x)
                square = square + abs(4 * f(x))
            end if
        end do

        sum = sum * h / 3
        square = square * h / 3

        print *, 'Значение интеграла:', sum
        print *, 'Площадь: ', square


    end subroutine integral_2
    
    subroutine sqrt_1()
        implicit none
        real :: number, low, high, mid, tochn

        30 print *, 'Введите число:'
        read *, number

        if (number < 0) then
            print *, '!!!!! Число должно быть неотрицательным !!!!!'
            goto 30
        end if 

        low = 0.0
        high = number
        tochn = 1e-5


        do while ((high - low) > tochn)
            mid = (low + high) / 2.0
            if (mid*mid > number) then
                high = mid
            else
                low = mid
            endif
        enddo

        print *, 'Квадратный корень числа ', number, ' равен ', mid
    end subroutine sqrt_1
    
    subroutine sqrt_2()
        implicit none

        ! Определение переменных
        real :: number, x, x_prev, tochn
        integer :: iter

        ! Ввод числа, для которого ищем квадратный корень
        40 print *, 'Введите положительное число:'
        read *, number

        if (number < 0) then
            print *, '!!!!! Число должно быть неотрицательным !!!!!'
            goto 40
        end if 

        ! Начальные условия для метода Ньютона
        x = number / 2.0  ! Начальная оценка
        tochn = 1e-5   ! Точность вычислений
        iter = 0           ! Счётчик итераций

        ! Алгоритм метода Ньютона
        do while (abs(x - x_prev) > tochn)
            x_prev = x
            x = (x + number/x) / 2.0
            iter = iter + 1
        enddo

        ! Вывод результата
        print *, 'Квадратный корень числа ', number, ' равен ', x
    end subroutine sqrt_2

    function f(x)
        implicit none
        real :: f, x
        f = sin(x)  ! Пример функции 
    end function f

end program test
