subroutine calc_energy
    use variables
    use parameters
    implicit none
    integer :: i, kind

    energy(:, :) = 0.00D0

    ! エネルギーの合計計算
    do kind = 1, 3
        do i = 1, N(kind)
            energy(kind, POTENTIAL) = energy(kind, POTENTIAL) + pot(kind, i)
            energy(kind, KINETIC) = energy(kind, KINETIC) + kin(kind, i)
        end do
    end do

    kin_interface_sum(U_PT) = sum(kin_interface(U_PT, :)) / 1.00D16
    kin_interface_sum(L_PT) = sum(kin_interface(L_PT, :)) / 1.00D16
    kin_phantom_sum(U_PT) = sum(kin_phantom(U_PT, :)) / 1.00D16
    kin_phantom_sum(L_PT) = sum(kin_phantom(L_PT, :)) / 1.00D16

    ! グループ別全E = グループ別全ポテンシャル + グループ別全運動エネルギー
    energy(:, TOTAL) = energy(:, POTENTIAL) + energy(:, KINETIC)

    ! 全E = 全ポテンシャル + 全運動エネルギー
    energy(ALL, :) = energy(U_PT, :) + energy(L_PT, :) + energy(AR, :)

    ! 有次元化
    energy(:, :)  = energy(:, :) / 1.00D16

    ! 温度計算
    temp(U_PT:L_PT) = 2.0D0 * energy(U_PT:L_PT, KINETIC) / (3.0D0 * N_LAYER(:)*2 * BOLTZMANN) ! 固定層を除いて計算
    temp(AR) = 2.0D0 * energy(AR, KINETIC) / (3.0D0 * N(AR) * BOLTZMANN)
    temp_interface(:) = 2.0D0 * kin_interface_sum(:) / (3.0D0 * N_LAYER(:) * BOLTZMANN)
    temp_phantom(:) = 2.0D0 * kin_phantom_sum(:) / (3.0D0 * N_LAYER(:) * BOLTZMANN)
end subroutine calc_energy