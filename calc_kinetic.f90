subroutine calc_kinetic
    use variables
    use parameters
    implicit none
    integer :: i, kind
    double precision :: vel2

    ! 初期化
    kin(:, :) = 0.0000D0

    ! 運動エネルギー計算
    do kind = 1, 3
        do i = 1, N(kind)
            vel2 = vel(kind, i, 1)*vel(kind, i, 1) + vel(kind, i, 2)*vel(kind, i, 2) + vel(kind, i, 3)*vel(kind, i, 3)
            kin(kind, i) = 0.500D0 * MASS(kind) * vel2
        end do
    end do
end subroutine calc_kinetic