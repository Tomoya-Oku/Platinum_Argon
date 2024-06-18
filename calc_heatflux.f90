subroutine calc_tempDistribution
    use variables
    use parameters
    implicit none
    integer :: i, j, count = 0
    double precision :: z_min, z_max, vel2_sum = 0.0D0

    do i = 1, PARTITION
        z_max = STDIST_Pt + LAYER * PARTITION - (i-1) * LAYER
        z_min = STDIST_Pt + LAYER * PARTITION - (i-1) * LAYER - LAYER

        ! write(6,*) "z_max, z_min", z_max, z_min

        ! 指定層にある粒子について速度の2乗和を計算
        do j = 1, N(AR)
            if ((z_min <= pos(AR, j, Z)) .and. (pos(AR, j, Z) < z_max)) then
                vel2_sum = vel2_sum + sum(vel(AR, j, :)*vel(AR, j, :))
                count = count + 1
            end if
        end do

        ! 指定層にある粒子について運動エネルギーを計算+有次元化
        kin_layer(i) = 0.500D0 * MASS(AR) * vel2_sum / 1.00D16

        ! 指定層にある粒子について温度を計算
        temp_layer(i) = 2.0D0 * kin_layer(i) / (3.0D0 * count * BOLTZMANN)
        count = 0
        vel2_sum = 0.0D0
    end do
end subroutine calc_tempDistribution

! subroutine calc_heatflux
!     use variables, only: nowstp
!     use parameters
!     implicit none


! end subroutine calc_heatflux