subroutine record_posvel
    use variables
    use parameters
    implicit none
    integer :: i, j

    ! 位置の記録 -> posit.dat
    do i = 1, u_Pt_N
        write(3, '(I6, 3E15.7)') i, u_Pt_pos(i,1), u_Pt_pos(i,2), u_Pt_pos(i,3)
    end do
    do i = 1, l_Pt_N
        j = u_Pt_N+i
        write(3, '(I6, 3E15.7)') j, l_Pt_pos(i,1), l_Pt_pos(i,2), l_Pt_pos(i,3)
    end do
    do i = 1, Ar_N
        j = u_Pt_N+l_Pt_N+i
        write(3, '(I6, 3E15.7)') j, Ar_pos(i,1), Ar_pos(i,2), Ar_pos(i,3)
    end do

    ! 速度の記録 -> velocity.dat
    do i = 1, u_Pt_N
        write(4, '(I6, 3E15.7)') i, u_Pt_vel(i,1), u_Pt_vel(i,2), u_Pt_vel(i,3)
    end do
    do i = 1, l_Pt_N
        j = u_Pt_N+i
        write(4, '(I6, 3E15.7)') j, l_Pt_vel(i,1), l_Pt_vel(i,2), l_Pt_vel(i,3)
    end do
    do i = 1, Ar_N
        j = u_Pt_N+l_Pt_N+i
        write(4, '(I6, 3E15.7)') j, Ar_vel(i,1), Ar_vel(i,2), Ar_vel(i,3)
    end do
end subroutine record_posvel

subroutine record_energy
    use variables
    use parameters
    implicit none
    double precision :: u_Pt_totene, u_Pt_totpot, u_Pt_totkin, u_Pt_temp
    double precision :: l_Pt_totene, l_Pt_totpot, l_Pt_totkin, l_Pt_temp
    double precision :: Ar_totene, Ar_totpot, Ar_totkin, Ar_temp
    double precision :: totene, totpot, totkin
    integer :: i

    u_Pt_totpot = 0.00D0
    u_Pt_totkin = 0.00D0
    u_Pt_totene = 0.00D0

    l_Pt_totpot = 0.00D0
    l_Pt_totkin = 0.00D0
    l_Pt_totene = 0.00D0

    Ar_totpot = 0.00D0
    Ar_totkin = 0.00D0
    Ar_totene = 0.00D0

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

    u_Pt_totene = u_Pt_totpot + u_Pt_totkin
    l_Pt_totene = l_Pt_totpot + l_Pt_totkin
    Ar_totene = Ar_totpot + Ar_totkin

    u_Pt_totpot = u_Pt_totpot / 1.00D16
    u_Pt_totkin = u_Pt_totkin / 1.00D16
    u_Pt_totene = u_Pt_totene / 1.00D16

    l_Pt_totpot = l_Pt_totpot / 1.00D16
    l_Pt_totkin = l_Pt_totkin / 1.00D16
    l_Pt_totene = l_Pt_totene / 1.00D16

    Ar_totpot = Ar_totpot / 1.00D16
    Ar_totkin = Ar_totkin / 1.00D16
    Ar_totene = Ar_totene / 1.00D16

    totpot = u_Pt_totpot + l_Pt_totpot + Ar_totpot
    totkin = u_Pt_totkin + l_Pt_totkin + Ar_totkin
    totene = u_Pt_totene + l_Pt_totene + Ar_totene

    ! 温度計算
    u_Pt_temp = 2.0D0 * u_Pt_totkin / (3.0D0 * dble(u_Pt_N) * BOLTZMANN)
    l_Pt_temp = 2.0D0 * l_Pt_totkin / (3.0D0 * dble(l_Pt_N) * BOLTZMANN)
    Ar_temp = 2.0D0 * Ar_totkin / (3.0D0 * dble(Ar_N) * BOLTZMANN)

    !write(7, '(6E15.7)') totene, totpot, totkin, u_Pt_temp, l_Pt_temp, Ar_temp
    write(7, '(6E15.7)') totene, totpot, totkin, u_Pt_temp, l_Pt_temp, Ar_temp
    write(8, '(6E15.7)') u_Pt_totene, u_Pt_totpot, u_Pt_totkin
    write(10, '(6E15.7)') l_Pt_totene, l_Pt_totpot, l_Pt_totkin
    write(11, '(6E15.7)') Ar_totene, Ar_totpot, Ar_totkin

end subroutine record_energy

subroutine record_finposvel
    use variables
    use parameters
    implicit none
    integer :: i, j

    do i = 1, u_Pt_N
        write(9, '(I6, 6E15.7)') i, u_Pt_pos(i,1), u_Pt_pos(i,2), u_Pt_pos(i,3), u_Pt_vel(i,1), u_Pt_vel(i,2), u_Pt_vel(i,3)
    end do
    do i = 1, l_Pt_N
        j = u_Pt_N+i
        write(9, '(I6, 6E15.7)') j, l_Pt_pos(i,1), l_Pt_pos(i,2), l_Pt_pos(i,3), l_Pt_vel(i,1), l_Pt_vel(i,2), l_Pt_vel(i,3)
    end do
    do i = 1, Ar_N
        j = u_Pt_N+l_Pt_N+i
        write(9, '(I6, 6E15.7)') j, Ar_pos(i,1), Ar_pos(i,2), Ar_pos(i,3), Ar_vel(i,1), Ar_vel(i,2), Ar_vel(i,3)
    end do
end subroutine record_finposvel