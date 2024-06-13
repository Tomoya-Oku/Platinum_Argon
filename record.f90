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

    num = 1

    !速度の記録 -> velocity.dat
    do kind = 1, 3
        do i = 1, N(kind)
            write(DAT_VELOCITY, '(I6, 3E15.7)') num, vel(kind, i, X), vel(kind, i, Y), vel(kind, i, Z)
            num = num + 1
        end do
    end do

    num = 1

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

    call calc_energy

    write(DAT_ENERGY, '(3E15.7)') energy(ALL, TOTAL), energy(ALL, POTENTIAL), energy(ALL, KINETIC)
    write(DAT_TEMP, '(3E15.7)') temp(U_PT), temp(L_PT), temp(AR)
    write(DAT_TEMP_INTERFACE, '(2E15.7)') temp_interface(U_PT), temp_interface(L_PT)
    write(DAT_TEMP_PHANTOM, '(2E15.7)') temp_phantom(U_PT), temp_phantom(L_PT)

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