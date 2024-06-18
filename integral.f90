subroutine integral
    use variables
    use parameters
    implicit none
    double precision :: rand(4), w(3), F_f(3), F_r(3)
    integer :: i, kind

    call calc_potential ! ポテンシャル計算
    call calc_kinetic ! 運動エネルギー計算

    ! 上部Pt 界面
    do i = 1, xyz(U_PT, X)*xyz(U_PT, Y)
        vel(U_PT, i, :) = vel(U_PT, i, :) + acc(U_PT, i, :)*DT
        pos(U_PT, i, :) = pos(U_PT, i, :) + vel(U_PT, i, :)*DT
    end do
    ! 下部Pt 界面
    do i = xyz(L_PT, X)*xyz(L_PT, Y)*(xyz(L_PT, Z)-1)+1, N(L_PT)
        vel(L_PT, i, :) = vel(L_PT, i, :) + acc(L_PT, i, :)*DT
        pos(L_PT, i, :) = pos(L_PT, i, :) + vel(L_PT, i, :)*DT
    end do
    ! Ar
    do i = 1, N(AR)
        vel(AR, i, :) = vel(AR, i, :) + acc(AR, i, :)*DT
        pos(AR, i, :) = pos(AR, i, :) + vel(AR, i, :)*DT
    end do

    ! 温度制御層 (Langevin法)
    do kind = 1, 2
        do i = xyz(kind, X)*xyz(kind, Y)+1, xyz(kind, X)*xyz(kind, Y)*(xyz(kind, Z)-1)
            ! Box-Muller法
            call random_number(rand)
            w(X) = sqrt(-2.0D0 * log(rand(1))) * cos(2.0D0 * PI * rand(2))
            w(Y) = sqrt(-2.0D0 * log(rand(1))) * sin(2.0D0 * PI * rand(2))
            w(Z) = sqrt(-2.0D0 * log(rand(3))) * cos(2.0D0 * PI * rand(4))

            F_f(:) = DAMPCOEF(kind) * vel(kind, i, :)
            F_r(:) = RANDCOEF(kind) * w(:)

            acc(kind, i, :) = acc(kind, i, :) + (F_f(:) + F_r(:)) / MASS(kind)
            vel(kind, i, :) = vel(kind, i, :) + acc(kind, i, :)*DT
            pos(kind, i, :) = pos(kind, i, :) + vel(kind, i, :)*DT
        end do
    end do
end subroutine integral