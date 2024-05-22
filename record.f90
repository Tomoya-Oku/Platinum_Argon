subroutine record_posvel
    use variables
    use parameters
    implicit none
    integer :: i

    ! 位置の記録 -> posit.dat
    do i = 1, u_Pt_N
        write(3, '(I6, 3E15.7)') i, u_Pt_pos(i,1), u_Pt_pos(i,2), u_Pt_pos(i,3)
    end do
    do i = 1, l_Pt_N
        write(3, '(I6, 3E15.7)') i, l_Pt_pos(i,1), l_Pt_pos(i,2), l_Pt_pos(i,3)
    end do
    do i = 1, Ar_N
        write(3, '(I6, 3E15.7)') i, Ar_pos(i,1), Ar_pos(i,2), Ar_pos(i,3)
    end do

    ! 速度の記録 -> velocity.dat
    do i = 1, u_Pt_N
        write(4, '(I6, 3E15.7)') i, u_Pt_vel(i,1), u_Pt_vel(i,2), u_Pt_vel(i,3)
    end do
    do i = 1, l_Pt_N
        write(4, '(I6, 3E15.7)') i, l_Pt_vel(i,1), l_Pt_vel(i,2), l_Pt_vel(i,3)
    end do
    do i = 1, Ar_N
        write(4, '(I6, 3E15.7)') i, Ar_vel(i,1), Ar_vel(i,2), Ar_vel(i,3)
    end do
end subroutine record_posvel

subroutine record_energy
    use variables
    use parameters
    implicit none
    double precision :: u_Pt_totpot, u_Pt_totkin, u_Pt_temp
    double precision :: l_Pt_totpot, l_Pt_totkin, l_Pt_temp
    double precision :: Ar_totpot, Ar_totkin, Ar_temp
    double precision :: totene, totpot, totkin
    integer :: i

    ! エネルギーの合計計算
    do i = 1, u_Pt_N
        u_Pt_totpot = u_Pt_totpot + u_Pt_pot(i)
        u_Pt_totkin = u_Pt_totkin + u_Pt_kin(i)
    end do
    do i = 1, l_Pt_N
        l_Pt_totpot = l_Pt_totpot + l_Pt_pot(i)
        l_Pt_totkin = l_Pt_totkin + l_Pt_kin(i)
    end do
    do i = 1, Ar_N
        Ar_totpot = Ar_totpot + Ar_pot(i)
        Ar_totkin = Ar_totkin + Ar_kin(i)
    end do

    totpot = (u_Pt_totpot + l_Pt_totpot + Ar_totpot) / 1.00D16
    totkin = (u_Pt_totkin + l_Pt_totkin + Ar_totkin) / 1.00D16
    totene = totpot + totene

    ! 温度計算
    u_Pt_temp = 2.0D0 * u_Pt_totkin / (3.0D0 * dble(u_Pt_N) * BOLTZMANN)
    l_Pt_temp = 2.0D0 * l_Pt_totkin / (3.0D0 * dble(l_Pt_N) * BOLTZMANN)
    Ar_temp = 2.0D0 * Ar_totkin / (3.0D0 * dble(Ar_N) * BOLTZMANN)

    write(7, '(4E15.7)') totene, totpot, totkin, u_Pt_temp, l_Pt_temp, Ar_temp
end subroutine record_energy