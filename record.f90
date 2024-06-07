! subroutine record_header
!     use parameters
!     implicit none

!     write(DAT_VELOCITY, '(A6, 1X, A15, 1X, A15, 1X, A15)') 'No.', 'x', 'y', 'z'
!     write(DAT_ENERGY, '(A15, 1X, A15, 1X, A15, 1X, A15, 1X, A15, 1X, A15, 1X, A15, 1X, A15, 1X, A15)') &
!     'U_Pt Total Energy', 'U_Pt Potential', 'U_Pt Kinetic Energy', 'L_Pt Total Energy', 'L_Pt Potential', 'L_Pt Kinetic Energy', 'Ar Total Energy', 'Ar Potential', 'Ar Kinetic Energy'
!     write(DAT_TEMP, '(A15, 1X, A15, 1X, A15)') 'U_Pt', 'L_Pt', 'Ar'

! end subroutine record_header

subroutine record_posvel
    use variables
    use parameters
    implicit none
    integer :: i, kind, num

    num = 1

    ! 位置の記録 -> posit.dat
    do kind = 1, 3
        do i = 1, N(kind)
            write(DAT_POSIT, '(I6, 3E15.7)') num, pos(kind, i, X), pos(kind, i, Y), pos(kind, i, Z)
            num = num + 1
        end do
    end do

    ! num = 1

    ! 速度の記録 -> velocity.dat
    ! do kind = 1, 3
    !     do i = 1, N(kind)
    !         write(DAT_VELOCITY, '(I6, 3E15.7)') num, vel(kind, i, X), vel(kind, i, Y), vel(kind, i, Z)
    !         num = num + 1
    !     end do
    ! end do

    ! num = 1

    ! 加速度の記録 -> acceleration.dat
    ! do kind = 1, 3
    !     do i = 1, N(kind)
    !         write(DAT_ACCELERATION, '(I6, 3E15.7)') num, acc(kind, i, X), acc(kind, i, Y), acc(kind, i, Z)
    !         num = num + 1
    !     end do
    ! end do

end subroutine record_posvel

subroutine record_energy
    use variables
    use parameters
    implicit none

    ! 分子の種類
    ! total_enegy, total_potential, total_kinetic
    double precision :: energy(4, 3)
    ! temperature
    double precision :: temperature(3)
    integer :: i, kind

    energy(:, :) = 0.00D0

    ! エネルギーの合計計算
    do kind = 1, 3
        do i = 1, N(kind)
            energy(kind, POTENTIAL) = energy(kind, POTENTIAL) + pot(kind, i)
            energy(kind, KINETIC) = energy(kind, KINETIC) + kin(kind, i)
        end do
    end do

    ! グループ別全E = グループ別全ポテンシャル + グループ別全運動エネルギー
    energy(:, TOTAL) = energy(:, POTENTIAL) + energy(:, KINETIC)

    ! 全E = 全ポテンシャル + 全運動エネルギー
    energy(ALL, :) = energy(U_PT, :) + energy(L_PT, :) + energy(AR, :)

    ! 有次元化
    energy(:, :)  = energy(:, :) / 1.00D16

    ! 温度計算
    temperature(:) = 2.0D0 * energy(1:3, KINETIC) / (3.0D0 * dble(N(1:3)) * BOLTZMANN)

    write(DAT_ENERGY, '(9E15.7)') energy(AR, 1), energy(AR, 2), energy(AR, 3)
    write(DAT_TEMP, '(3E15.7)') temperature(U_PT), temperature(L_PT), temperature(AR)

end subroutine record_energy

subroutine record_finposvel
    use variables
    use parameters
    implicit none
    integer :: i, k, num

    do k = 1, 3
        do i = 1, N(k)
            write(DAT_PERIODIC, '(i6, 6E15.7)') num, pos(k,i,X), pos(k,i,Y), pos(k,i,Z), vel(k,i,X), vel(k,i,Y), vel(k,i,Z)
            num = num + 1
        end do
    end do

end subroutine record_finposvel