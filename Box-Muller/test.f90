program main
    implicit none
    integer :: i, N = 100000
    double precision, parameter :: PI = 3.141592654D0 ! 円周率
    double precision :: rand(4), w(3)

    open(0, file='Normal_Distribution.dat')

    do i = 1, N
        call random_number(rand)
        w(1) = sqrt(-2.0D0 * log(rand(1))) * cos(2.0D0 * PI * rand(2))
        w(2) = sqrt(-2.0D0 * log(rand(1))) * sin(2.0D0 * PI * rand(2))
        w(3) = sqrt(-2.0D0 * log(rand(3))) * cos(2.0D0 * PI * rand(4))

        write(0,'(E15.7)') w(1)
        write(0,'(E15.7)') w(2)
        write(0,'(E15.7)') w(3)
    end do
end program main