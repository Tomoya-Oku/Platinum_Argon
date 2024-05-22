subroutine calc_LeapFlog
    use variables
    use parameters
    implicit none
    integer :: i
    double precision :: vene
    double precision, dimension(3) :: venes

    ! 初期化
    u_Pt_kin(:) = 0.0000D0
    l_Pt_kin(:) = 0.0000D0
    Ar_kin(:) = 0.0000D0

    call calc_potential ! ポテンシャル計算

    ! 運動エネルギー計算
    do i = 1, u_Pt_N
        venes(:) = u_Pt_vel(i,:) + u_Pt_acc(i,:)*0.500D0*dt
        vene = venes(1)*venes(1) + venes(2)*venes(2) + venes(3)*venes(3)
        u_Pt_kin(i) = 0.500D0 * Pt_M * vene
    end do
    do i = 1, l_Pt_N
        venes(:) = l_Pt_vel(i,:) + l_Pt_acc(i,:)*0.500D0*dt
        vene = venes(1)*venes(1) + venes(2)*venes(2) + venes(3)*venes(3)
        l_Pt_kin(i) = 0.500D0 * Pt_M * vene
    end do
    do i = 1, Ar_N
        venes(:) = Ar_vel(i,:) + Ar_acc(i,:)*0.500D0*dt
        vene = venes(1)*venes(1) + venes(2)*venes(2) + venes(3)*venes(3)
        Ar_kin(i) = 0.500D0 * Ar_M * vene
    end do

    ! 力から加速度を算出
    do i = 1, u_Pt_N
        u_Pt_acc(i,:) = u_Pt_for(i,:)
    end do
    do i = 1, l_Pt_N
        l_Pt_acc(i,:) = l_Pt_for(i,:)
    end do
    do i = 1, Ar_N
        Ar_acc(i,:) = Ar_for(i,:)
    end do

    ! 蛙飛び法
    do i = 1, u_Pt_N
        u_Pt_vel(i,:) = u_Pt_vel(i,:) + u_Pt_acc(i,:)*dt
        u_Pt_pos(i,:) = u_Pt_pos(i,:) + u_Pt_vel(i,:)*dt
    end do
    do i = 1, l_Pt_N
        l_Pt_vel(i,:) = l_Pt_vel(i,:) + l_Pt_acc(i,:)*dt
        l_Pt_pos(i,:) = l_Pt_pos(i,:) + l_Pt_vel(i,:)*dt
    end do
    do i = 1, Ar_N
        Ar_vel(i,:) = Ar_vel(i,:) + Ar_acc(i,:)*dt
        Ar_pos(i,:) = Ar_pos(i,:) + Ar_vel(i,:)*dt
    end do
end subroutine calc_LeapFlog