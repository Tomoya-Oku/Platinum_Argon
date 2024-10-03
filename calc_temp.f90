subroutine calc_temp
    use variables
    use parameters
    implicit none

    integer :: l

    temp(U_PT:L_PT) = 2.0D0 * energy(U_PT:L_PT, KINETIC) / (3.0D0 * N_LAYER*(xyz(U_PT, Z)-1) * BOLTZMANN) ! 固定層を除いて計算
    temp(AR) = 2.0D0 * energy(AR, KINETIC) / (3.0D0 * N(AR) * BOLTZMANN)

    do l = 1, INTERFACE_LAYER
        temp_interface(:, l) = 2.0D0 * kin_interface_sum(:, l) / (3.0D0 * N_LAYER * BOLTZMANN)
    end do
    
    do l = 1, PHANTOM_LAYER
        temp_phantom(:, l) = 2.0D0 * kin_phantom_sum(:, l) / (3.0D0 * N_LAYER * BOLTZMANN)
    end do
    
end subroutine calc_temp

subroutine calc_tempDistribution
    use variables
    use parameters
    implicit none

    integer :: i, j
    integer :: count = 0 ! 指定層の中にある粒子数
    double precision :: z_min, z_max
    double precision :: vel2_sum = 0.0D0 ! 速度の2乗和

    do i = 1, PARTITION
        z_max = z_interface_lower + LAYER * i
        z_min = z_interface_lower + LAYER * (i-1)
        z_mid(i) = z_min + LAYER / 2 ! 層の代表z座標

        ! 指定層にある粒子について速度の2乗和を計算
        do j = 1, N(AR)
            if ((z_min <= pos(AR, j, Z)) .and. (pos(AR, j, Z) < z_max)) then
                vel2_sum = vel2_sum + DOT_PRODUCT(velene(AR, j, :), velene(AR, j, :))
                count = count + 1
            end if
        end do

        ! 指定層にある粒子について運動エネルギーを計算+有次元化
        kin_layer(i) = 0.500D0 * MASS(AR) * vel2_sum * 1.00D-16

        ! 指定層にある粒子について温度を計算
        temp_layer(i) = 2.0D0 * kin_layer(i) / (3.0D0 * count * BOLTZMANN)

        count = 0
        vel2_sum = 0.0D0
    end do
end subroutine calc_tempDistribution