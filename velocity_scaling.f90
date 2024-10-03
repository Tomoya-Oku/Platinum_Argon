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