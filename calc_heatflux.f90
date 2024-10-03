subroutine calc_heatflux
    use variables
    use parameters
    use functions
    implicit none

    integer :: i, j, kind
    double precision :: Fv(2) = 0.0D0, F_Dv(2) = 0.0D0, F_Rv(2) = 0.0D0

    do j = 1, N(AR)
        do kind = U_PT, L_PT
            do i = INTERFACE_START(kind), INTERFACE_END(kind)
                Fv(kind) = Fv(kind) + DOT_PRODUCT(for(kind, AR, i, j, :), velene(kind, i, :))
            end do

            ! do i = 1, N(kind)
            !     if (isInterface(kind, i)) then
            !         Fv(kind) = Fv(kind) + DOT_PRODUCT(for(kind, AR, i, j, :), velene(kind, i, :))
            !     end if
            ! end do
        end do
    end do

    do kind = U_PT, L_PT
        do i = PHANTOM_START(kind), PHANTOM_END(kind)
            F_Dv(kind) = F_Dv(kind) + DOT_PRODUCT(F_D(kind, i, :), velene(kind, i, :))
            F_Rv(kind) = F_Rv(kind) + DOT_PRODUCT(F_R(kind, i, :), velene(kind, i, :))
        end do

        ! do i = 1, N(kind)
        !     if (isPhantom(kind, i)) then
        !         F_Dv(kind) = F_Dv(kind) + DOT_PRODUCT(F_D(kind, i, :), velene(kind, i, :))
        !         F_Rv(kind) = F_Rv(kind) + DOT_PRODUCT(F_R(kind, i, :), velene(kind, i, :))
        !     end if
        ! end do
    end do

    ! interface
    heatflux_interface(:) = Fv(:) / A
    Q_interface(:) = Q_interface(:) + heatflux_interface(:) * DT * 1.0D-4 ! 有次元化[J/m^2]

    ! phantom
    heatflux_phantom(:) = (F_Dv(:) + F_Rv(:)) / A
    Q_phantom(:) = Q_phantom(:) + heatflux_phantom(:) * DT  * 1.0D-4 ! 有次元化[J/m^2]

end subroutine calc_heatflux