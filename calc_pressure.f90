subroutine calc_pressure
    use variables
    use parameters
    use functions
    implicit none

    integer :: i, j, kind
    F_sum = 0.0

    do i = 1, N(AR)
        do kind = U_PT, L_PT
            do j = 1, N(kind)
                if (isInterface(kind, j)) then
                    ! i番目のArがj番目のPtから受ける力
                    F_sum = F_sum + for(AR, kind, i, j, Z)
                end if
            end do
        end do
    end do

    pressure = F_sum / A * 1.0D14 ! 有次元化[Pa]
end subroutine