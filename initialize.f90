subroutine initialize
    use variables
    use parameters
    implicit none
    integer :: num = 0, i, j, k, kind
    double precision :: ran, r1, r2, cr = 1.00D-6
    double precision, dimension(3) :: init_xyz
    
    double precision, dimension(3) :: v

    do i = 1,3
        write(DAT_PERIODIC,*) SSIZE(i)
    end do
    
    ! 初期配置
    do kind = 1, 3 ! 分子の種類
        do k = 1, xyz(kind, Z) ! z
            init_xyz(Z) = OFST(kind, Z) + dble(k-1)*STDIST(kind)/2.0D0
            do i=1, xyz(kind, X) ! x
                init_xyz(X) = OFST(kind, X) + dble(i-1)*STDIST(kind)/2.0D0
                do j=1, xyz(kind, Y) ! y
                    ! iとkの偶奇が一致する
                    if(mod(k,2) == mod(i,2)) then
                        init_xyz(Y) = OFST(kind, Y) + dble(j-1)*STDIST(kind) + STDIST(kind)/2.0D0
                    else
                        init_xyz(Y) = OFST(kind, Y) + dble(j-1)*STDIST(kind)
                    endif
                    num = num + 1
                    pos(kind, num, :) = init_xyz(:)
                end do
            end do
        end do
        num = 0
        init_xyz(:) = 0
    end do

    ! Arに初期速度を与える
    do i=1, N(AR)
        read(DAT_RANDOM1,*)ran
        r1 = PI*ran
        read(DAT_RANDOM0,*)ran
        r2 = 2.000D0*PI*ran
        v(1) = dsin(r1)*dcos(r2)*cr
        v(2) = dsin(r1)*dsin(r2)*cr
        v(3) = dcos(r1)*cr
        vel(AR, i, :) = v(:)
    end do

end subroutine initialize