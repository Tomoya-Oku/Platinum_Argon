subroutine calc_bound
    use variables
    use parameters
    implicit none
    integer :: i, j

    do i = 1, u_Pt_N
        do j = 1, 3
            if(u_Pt_pos(i, j) < 0.00D0) then
                u_Pt_pos(i, j) = u_Pt_pos(i, j) + syul(j)
            else if(u_Pt_pos(i, j) > syul(j)) then
                u_Pt_pos(i, j) = u_Pt_pos(i, j) - syul(j)
            endif
        end do
    end do

    do i = 1, l_Pt_N
        do j = 1, 3
            if(l_Pt_pos(i, j) < 0.00D0) then
                l_Pt_pos(i, j) = l_Pt_pos(i, j) + syul(j)
            else if(l_Pt_pos(i, j) > syul(j)) then
                l_Pt_pos(i, j) = l_Pt_pos(i, j) - syul(j)
            endif
        end do
    end do

    do i = 1, Ar_N
        do j = 1, 3
            if(Ar_pos(i, j) < 0.00D0) then
                Ar_pos(i, j) = Ar_pos(i, j) + syul(j)
            else if(Ar_pos(i, j) > syul(j)) then
                Ar_pos(i, j) = Ar_pos(i, j) - syul(j)
            endif
        end do
    end do
end subroutine calc_bound