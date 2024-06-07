subroutine calc_LeapFlog
    use variables
    use parameters
    implicit none
    integer :: i

    call calc_potential ! ポテンシャル計算
    call calc_kinetic ! 運動エネルギー計算

    ! 上部Pt 固定層以外
    do i = 1, xyz(U_PT, X)*xyz(U_PT, Y)*(xyz(U_PT, Z)-1)
        vel(U_PT, i, :) = vel(U_PT, i, :) + acc(U_PT, i, :)*DT
        pos(U_PT, i, :) = pos(U_PT, i, :) + vel(U_PT, i, :)*DT
    end do
    ! 下部Pt 固定層以外
    do i = xyz(L_PT, X)*xyz(L_PT, Y)+1, N(L_PT)
        vel(L_PT, i, :) = vel(L_PT, i, :) + acc(L_PT, i, :)*DT
        pos(L_PT, i, :) = pos(L_PT, i, :) + vel(L_PT, i, :)*DT
    end do
    ! Ar
    do i = 1, N(AR)
        vel(AR, i, :) = vel(AR, i, :) + acc(AR, i, :)*DT
        pos(AR, i, :) = pos(AR, i, :) + vel(AR, i, :)*DT
    end do
end subroutine calc_LeapFlog