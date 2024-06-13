subroutine calc_kinetic
    use variables
    use parameters
    implicit none
    integer :: i, kind, j = 1, k = 1
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
                kin_interface(U_PT, j) = kin(U_PT, i)
                j = j + 1
            else if (kind == L_PT .and. i > xyz(L_PT, X)*xyz(L_PT, Y)*(xyz(L_PT, Z)-1)) then
                kin_interface(L_PT, j) = kin(L_PT, i)
                j = j + 1
            end if

            ! Phantom面のみ
            if (kind == U_PT .and. xyz(U_PT, X)*xyz(U_PT, Y)+1 <= i .and. i <= xyz(U_PT, X)*xyz(U_PT, Y)*(xyz(U_PT, z)-1)) then
                kin_phantom(U_PT, k) = kin(U_PT, i)
                k = k + 1
            else if (kind == L_PT .and. xyz(L_PT, X)*xyz(L_PT, Y)+1 <= i .and. i <= xyz(L_PT, X)*xyz(L_PT, Y)*(xyz(L_PT, z)-1)) then
                kin_phantom(L_PT, k) = kin(L_PT, i)
                k = k + 1
            end if
        end do
        j = 1
        k = 1
    end do

end subroutine calc_kinetic