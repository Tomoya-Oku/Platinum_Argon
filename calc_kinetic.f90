subroutine calc_kinetic
    use variables
    use parameters
    use functions
    implicit none
    integer :: i, kind, l = 1
    double precision :: vel2

    ! 初期化
    kin(:, :) = 0.0000D0

    ! 運動エネルギー計算
    do kind = 1, 3
        do i = 1, N(kind)
            vel2 = sum(velene(kind, i, :)*velene(kind, i, :))
            kin(kind, i) = 0.500D0 * MASS(kind) * vel2

            ! 上部Pt
            if (kind == U_PT) then
                ! 界面
                if (isInterface(U_PT, i)) then
                    do l = 1, INTERFACE_LAYER
                        if (N_LAYER*(l-1)+1 <= i .and. i <= N_LAYER*(l)) then
                            kin_interface_sum(U_PT, l) = kin_interface_sum(U_PT, l) + kin(U_PT, i)
                        end if
                    end do
                ! Phantom層
                else if (isPhantom(U_PT, i)) then
                    do l = 1, PHANTOM_LAYER
                        if (N_LAYER*(INTERFACE_LAYER+l-1)+1 <= i .and. i <= N_LAYER*(INTERFACE_LAYER+l)) then
                            kin_phantom_sum(U_PT, l) = kin_phantom_sum(U_PT, l) + kin(U_PT, i)
                        end if
                    end do
                end if
            ! 下部Pt
            else if (kind == L_PT) then
                ! 界面
                if (isInterface(L_PT, i)) then
                    do l = 1, INTERFACE_LAYER
                        if (N_LAYER*(PHANTOM_LAYER+l)+1 <= i .and. i <= N_LAYER*(PHANTOM_LAYER+l+1)) then
                            kin_interface_sum(L_PT, l) = kin_interface_sum(L_PT, l) + kin(L_PT, i)
                        end if
                    end do
                ! Phantom層
                else if (isPhantom(L_PT, i)) then
                    do l = 1, PHANTOM_LAYER
                        if ((N_LAYER*l)+1 <= i .and. i <= N_LAYER*(l+1)) then
                            kin_phantom_sum(L_PT, l) = kin_phantom_sum(L_PT, l) + kin(L_PT, i)
                        end if
                    end do
                end if
            end if
        end do
    end do

end subroutine calc_kinetic