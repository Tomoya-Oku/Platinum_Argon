subroutine correct_trspeed
    use variables
    use parameters
    implicit none
    double precision, dimension(3) :: u_Pt_trv = 0.00000D0
    double precision, dimension(3) :: l_Pt_trv = 0.00000D0
    double precision, dimension(3) :: Ar_trv = 0.00000D0
    integer :: i

    do i = 1, u_Pt_N
        u_Pt_trv(:) = u_Pt_trv(:) + u_Pt_vel(i,:)
    end do

    do i = 1, l_Pt_N
        l_Pt_trv(:) = l_Pt_trv(:) + l_Pt_vel(i,:)
    end do

    do i = 1, Ar_N
        Ar_trv(:) = Ar_trv(:) + Ar_vel(i,:)
    end do

    u_Pt_trv(:) = u_Pt_trv(:) / u_Pt_N
    l_Pt_trv(:) = l_Pt_trv(:) / l_Pt_N
    Ar_trv(:) = Ar_trv(:) / Ar_N

    do i = 1, u_Pt_N
        u_Pt_vel(i,:) = u_Pt_vel(i,:) - u_Pt_trv(:)
    end do

    do i = 1, l_Pt_N
        l_Pt_vel(i,:) = l_Pt_vel(i,:) - l_Pt_trv(:)
    end do

    do i = 1, Ar_N
        Ar_vel(i,:) = Ar_vel(i,:) - Ar_trv(:)
    end do

end subroutine correct_trspeed

! 速度スケーリング法
subroutine velocity_scaling
    use variables
    use parameters
    implicit none
    double precision :: vel2, vel2_sum, vel2_mean, baiss
    integer :: i

    ! 上部Pt
    vel2_sum = 0.00D0
    do i = 1, u_Pt_N
        vel2 = u_Pt_vel(i,1)*u_Pt_vel(i,1) + u_Pt_vel(i,2)*u_Pt_vel(i,2) + u_Pt_vel(i,3)*u_Pt_vel(i,3)
        vel2_sum = vel2_sum + vel2
    end do
    vel2_mean = vel2_sum / dble(u_Pt_N) / 1.000D+16 ! 後のために有次元化
    baiss = dsqrt(3.00D0 * BOLTZMANN * u_Pt_atemp / Pt_M / vel2_mean)
    do i = 1, u_Pt_N
        u_Pt_vel(i, :) = u_Pt_vel(i, :) * baiss
    end do

    ! 下部Pt
    vel2_sum = 0.00D0
    do i = 1, l_Pt_N
        vel2 = l_Pt_vel(i,1)*l_Pt_vel(i,1) + l_Pt_vel(i,2)*l_Pt_vel(i,2) + l_Pt_vel(i,3)*l_Pt_vel(i,3)
        vel2_sum = vel2_sum + vel2
    end do
    vel2_mean = vel2_sum / dble(l_Pt_N) / 1.000D+16 ! 後のために有次元化
    baiss = dsqrt(3.00D0 * BOLTZMANN * l_Pt_atemp / Pt_M / vel2_mean)
    do i = 1, l_Pt_N
        l_Pt_vel(i, :) = l_Pt_vel(i, :) * baiss
    end do

    ! Ar
    vel2_sum = 0.00D0
    do i = 1, Ar_N
        vel2 = Ar_vel(i,1)*Ar_vel(i,1) + Ar_vel(i,2)*Ar_vel(i,2) + Ar_vel(i,3)*Ar_vel(i,3)
        vel2_sum = vel2_sum + vel2
    end do
    vel2_mean = vel2_sum / dble(Ar_N) / 1.000D+16 ! 後のために有次元化
    baiss = dsqrt(3.00D0 * BOLTZMANN * Ar_atemp / Ar_M / vel2_mean)
    do i = 1, Ar_N
        Ar_vel(i, :) = Ar_vel(i, :) * baiss
    end do

end subroutine velocity_scaling

subroutine correct_cogravity
    use variables
    use parameters
    implicit none
    double precision, dimension(3) :: cms, tcms
    integer :: i

    cms(:) = ssize(:) / 2.0D0
    tcms(:) = 0.0000D0

    do i = 1, u_Pt_N
        tcms(:) = tcms(:) + u_Pt_pos(i,:)
    end do

    do i = 1, l_Pt_N
        tcms(:) = tcms(:) + l_Pt_pos(i,:)
    end do

    do i = 1, Ar_N
        tcms(:) = tcms(:) + Ar_pos(i,:)
    end do

    tcms(:) = cms(:) - tcms(:) / dble(u_Pt_N+l_Pt_N+Ar_N)
   
    do i = 1, u_Pt_N
        u_Pt_pos(i,:) = u_Pt_pos(i,:) + tcms(:)
    end do

    do i = 1, l_Pt_N
        l_Pt_pos(i,:) = l_Pt_pos(i,:) + tcms(:)
    end do

    do i = 1, Ar_N
        Ar_pos(i,:) = Ar_pos(i,:) + tcms(:)
    end do

end subroutine correct_cogravity