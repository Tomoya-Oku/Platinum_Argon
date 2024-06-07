! 並進速度の補正
subroutine correct_trvelocity
    use variables
    use parameters
    implicit none
    double precision, dimension(3) :: trvel = 0.00000D0

    trvel(1) = sum(vel(AR, :, 1))
    trvel(2) = sum(vel(AR, :, 2))
    trvel(3) = sum(vel(AR, :, 3))

    trvel(:) = trvel(:) / N(AR)

    vel(AR, :, 1) = vel(AR, :, 1) - trvel(1)
    vel(AR, :, 2) = vel(AR, :, 2) - trvel(2)
    vel(AR, :, 3) = vel(AR, :, 3) - trvel(3)
end subroutine correct_trvelocity

! 重心の補正
subroutine correct_cogravity
    use variables
    use parameters
    implicit none
    double precision, dimension(3) :: cms = 0.0000D0, tcms = 0.0000D0

    cms(:) = SSIZE(:) / 2.0D0

    tcms(X) = sum(pos(AR, :, X))
    tcms(Y) = sum(pos(AR, :, Y))
    tcms(Z) = sum(pos(AR, :, Z))

    tcms(:) = cms(:) - tcms(:) / N(AR)
   
    pos(AR, :, X) = pos(AR, :, X) + tcms(X)
    pos(AR, :, Y) = pos(AR, :, Y) + tcms(Y)
    pos(AR, :, Z) = pos(AR, :, Z) + tcms(Z)

end subroutine correct_cogravity

! 速度スケーリング法
subroutine velocity_scaling
    use variables
    use parameters
    implicit none
    double precision :: vel2, vel2_sum, vel2_mean, baiss
    integer :: i

    vel2_sum = 0.00D0 ! すべての分子の速度の2乗和
    do i = 1, N(AR)
        vel2 = vel(AR, i, 1)*vel(AR, i, 1) + vel(AR, i, 2)*vel(AR, i, 2) + vel(AR, i, 3)*vel(AR, i, 3)
        vel2_sum = vel2_sum + vel2
    end do
    vel2_mean = vel2_sum / dble(N(AR)) / 1.000D+16 ! 速度2乗の平均・後のために有次元化
    baiss = dsqrt(3.00D0 * BOLTZMANN * ATEMP_AR / MASS(AR) / vel2_mean)
    do i = 1, N(AR)
        vel(AR, i, :) = vel(AR, i, :) * baiss
    end do

end subroutine velocity_scaling