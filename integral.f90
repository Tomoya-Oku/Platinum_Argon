subroutine integral
    use variables
    use parameters
    use functions
    implicit none
    double precision :: rand(4), w(3)
    integer :: kind, i

    call calc_potential ! ポテンシャル計算

    ! 更新
    do kind = 1, 3
        do i = 1, N(kind)
            if (isPhantom(kind, i)) then
                ! Box-Muller法
                call random_number(rand)
                w(X) = dsqrt(-2.0D0 * log(rand(1))) * cos(2.0D0 * PI * rand(2))
                w(Y) = dsqrt(-2.0D0 * log(rand(1))) * sin(2.0D0 * PI * rand(2))
                w(Z) = dsqrt(-2.0D0 * log(rand(3))) * cos(2.0D0 * PI * rand(4))

                F_D(kind, i, :) = DAMPCOEF(kind) * vel(kind, i, :) ! 時刻tでのダンパ力
                F_R(kind, i, :) = RANDCOEF(kind) * w(:) ! 時刻tでのランダム力

                acc(kind, i, :) = acc(kind, i, :) + (F_D(kind, i, :) + F_R(kind, i, :)) * MASS_inv(kind) ! 時刻tでの加速度
            end if
                
            if (isNotFixed(kind, i)) then
                velene(kind, i, :) = vel(kind, i, :) + acc(kind, i, :) * 0.5D0 * DT ! 時刻t+DTでの速度
                vel(kind, i, :) = vel(kind, i, :) + acc(kind, i, :)*DT ! v(t+DT/2) = v(t-DT/2) + a(t)DT
                pos(kind, i, :) = pos(kind, i, :) + vel(kind, i, :)*DT ! r(t+DT) = r(t) + v(t+DT/2)DT
            end if
        end do
    end do

end subroutine integral