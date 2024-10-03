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
            write(DAT_POSIT, '(I8, 3E15.7)') num, pos(kind, i, X), pos(kind, i, Y), pos(kind, i, Z)
            num = num + 1
        end do
    end do

    num = 1

    ! 速度の記録 -> velocity.dat
    do kind = 1, 3
        do i = 1, N(kind)
            write(DAT_VELOCITY, '(I8, 3E15.7)') num, vel(kind, i, X), vel(kind, i, Y), vel(kind, i, Z)
            num = num + 1
        end do
    end do

    num = 1

    ! 計測用速度の記録 -> velene.dat
    do kind = 1, 3
        do i = 1, N(kind)
            write(DAT_VELENE, '(I8, 3E15.7)') num, velene(kind, i, X), velene(kind, i, Y), velene(kind, i, Z)
            num = num + 1
        end do
    end do

end subroutine record_posvel

subroutine record_force
    use variables
    use parameters
    implicit none
    integer :: i, kind

    do kind = U_PT, L_PT
        do i = 1, N(kind)
            write(DAT_DFORCE, '(2I8, 3E15.7)') nowstp, i, F_D(kind, i, X), F_D(kind, i, Y), F_D(kind, i, Z)
            write(DAT_RFORCE, '(2I8, 3E15.7)') nowstp, i, F_R(kind, i, X), F_R(kind, i, Y), F_R(kind, i, Z)
        end do
    end do

end subroutine record_force

subroutine record_energy
    use variables
    use parameters
    implicit none

    write(DAT_ENERGY, '(I8, 3E15.7)') nowstp, energy(ALL, TOTAL), energy(ALL, POTENTIAL), energy(ALL, KINETIC)
end subroutine record_energy

subroutine record_temp
    use variables
    use parameters
    implicit none

    write(DAT_TEMP, '(I8, 3E15.7)') nowstp, temp(U_PT), temp(L_PT), temp(AR)
    ! write(DAT_TEMP_INTERFACE, '(2E15.7)') temp_interface(U_PT), temp_interface(L_PT)
    ! write(DAT_TEMP_PHANTOM, '(2E15.7)') temp_phantom(U_PT), temp_phantom(L_PT)
end subroutine record_temp

subroutine record_tempDistribution
    use variables
    use parameters
    implicit none
    integer :: l, i

    ! 下部Pt, Phantom層
    do l = 1, PHANTOM_LAYER
        i = l
        write(DAT_TEMP_DISTRIBUTION, '(2I8, 2E15.7)') nowstp, l, OFST(L_PT, Z) + i*STDIST(L_PT)*0.5D0 , temp_phantom(L_PT, i)
    end do

    ! 下部Pt, 界面
    do l = PHANTOM_LAYER+1, PHANTOM_LAYER+INTERFACE_LAYER
        i = l - PHANTOM_LAYER
        write(DAT_TEMP_DISTRIBUTION, '(2I8, 2E15.7)') nowstp, l, l*STDIST(L_PT)*0.5D0, temp_interface(L_PT, i) 
    end do
    
    ! Ar(分割)
    do l = PHANTOM_LAYER+INTERFACE_LAYER+1, PHANTOM_LAYER+INTERFACE_LAYER+PARTITION
        i = l - (PHANTOM_LAYER+INTERFACE_LAYER)
        write(DAT_TEMP_DISTRIBUTION, '(2I8, 2E15.7)') nowstp, l, z_mid(i), temp_layer(i)
    end do

    ! 上部Pt, 界面
    do l = PHANTOM_LAYER+INTERFACE_LAYER+PARTITION+1, PHANTOM_LAYER+2*INTERFACE_LAYER+PARTITION
        i = l - (PHANTOM_LAYER+INTERFACE_LAYER+PARTITION)
        write(DAT_TEMP_DISTRIBUTION, '(2I8, 2E15.7)') nowstp, l, OFST(U_PT, Z)+(i-1)*STDIST(U_PT)*0.5D0, temp_interface(U_PT, i)
    end do

    ! 上部Pt, Phantom層
    do l = PHANTOM_LAYER+2*INTERFACE_LAYER+PARTITION+1, 2*PHANTOM_LAYER+2*INTERFACE_LAYER+PARTITION
        i = l - (PHANTOM_LAYER+2*INTERFACE_LAYER+PARTITION)
        write(DAT_TEMP_DISTRIBUTION, '(2I8, 2E15.7)') nowstp, l, OFST(U_PT, Z)+(INTERFACE_LAYER+i-1)*STDIST(U_PT)*0.5D0, &
        temp_phantom(U_PT, i)
    end do 
    
end subroutine record_tempDistribution

subroutine record_pressure
    use variables
    use parameters
    implicit none

    write(DAT_PRESSURE, '(I8, 2E15.7)') nowstp, pressure

end subroutine record_pressure

subroutine record_heatflux
    use variables
    use parameters
    implicit none

    write(DAT_HEATFLUX, "(I8, 4E15.7)") nowstp, heatflux_interface(U_PT), heatflux_interface(L_PT), &
    heatflux_phantom(U_PT), heatflux_phantom(L_PT)

end subroutine record_heatflux

subroutine record_Q
    use variables
    use parameters
    implicit none

    write(DAT_Q, "(I8, 4E15.7)") nowstp, Q_interface(U_PT), Q_interface(L_PT), Q_phantom(U_PT), Q_phantom(L_PT)

end subroutine record_Q

subroutine record_finposvel
    use variables
    use parameters
    implicit none
    integer :: i, k, num

    do k = 1, 3
        do i = 1, N(k)
            write(DAT_PERIODIC, '(I8, 6E15.7)') num, pos(k,i,X), pos(k,i,Y), pos(k,i,Z), vel(k,i,X), vel(k,i,Y), vel(k,i,Z)
            num = num + 1
        end do
    end do

end subroutine record_finposvel