subroutine calc_kinetic
    use variables
    use parameters
    implicit none
    integer :: i, kind, num = 1
    double precision :: vel2

    ! 初期化
    kin(:, :) = 0.0000D0

    ! 運動エネルギー計算
    do kind = 1, 3
        do i = 1, N(kind)
            vel2 = vel(kind, i, 1)*vel(kind, i, 1) + vel(kind, i, 2)*vel(kind, i, 2) + vel(kind, i, 3)*vel(kind, i, 3)
            kin(kind, i) = 0.500D0 * MASS(kind) * vel2

            ! 界面のみ
            if (kind == U_PT .and. i <= xyz(U_PT, X)*xyz(U_PT, Y)) then
                kin_interface(U_PT, i) = kin(U_PT, i)
            else if (kind == L_PT .and. i > xyz(L_PT, X)*xyz(L_PT, Y)*(xyz(L_PT, Z)-1)) then
                kin_interface(L_PT, num) = kin(L_PT, i)
                num = num + 1
            end if
        end do
        num = 1
    end do

end subroutine calc_kinetic