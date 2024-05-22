subroutine calc_potential
    use variables
    use parameters
    implicit none
    integer :: i1, i2, j
    double precision :: dist, ppp, force
    double precision :: dit2, dit4, dit6, dit8, dit12, dit14
    double precision, dimension(3) :: divs, forces

    u_Pt_for(:,:) = 0.0000D0
    u_Pt_pot(:) = 0.0000D0
    l_Pt_for(:,:) = 0.0000D0
    l_Pt_pot(:) = 0.0000D0
    Ar_for(:,:) = 0.0000D0
    Ar_pot(:) = 0.0000D0

    ! 上部Ptが受ける力
    do i1 = 1, u_Pt_N
        ! 上部Ptが上部Ptから受ける力
        LP1:do i2 = i1+1, u_Pt_N
                divs(:) = u_Pt_pos(i1, :) - u_Pt_pos(i2, :)

                do j = 1,3
                    if (divs(j) < -Pt_Pt_cutoff(j)) then
                        divs(j) = divs(j) + syul(j)
                    else if(divs(j) > Pt_Pt_cutoff(j)) then
                        divs(j) = divs(j) - syul(j)
                    endif
                end do

                do j = 1,3
                    divs(j) = divs(j) / Pt_Pt_sig
                    if (divs(j) > cutoff) cycle LP1
                    if (divs(j) < -cutoff) cycle LP1
                end do

                dit2 = divs(1)*divs(1) + divs(2)*divs(2) + divs(3)*divs(3)
                dist = dsqrt(dit2)
                if (dist > cutoff) cycle

                dit4 = dit2*dit2
                dit6 = dit4*dit2
                dit8 = dit4*dit4
                dit12 = dit6*dit6
                dit14 = dit8*dit6

                ppp = 4.00D0*Pt_Pt_eps*(1.00D0/dit12-1.00D0/dit6)
                force = Pt_Pt_cforce*(-2.00D0/dit14+1.00D0/dit8)

                forces(:) = -force * divs(:) / Pt_M
                u_Pt_for(i1, :) = u_Pt_for(i1, :) + forces(:)
                u_Pt_for(i2, :) = u_Pt_for(i2, :) - forces(:)

                u_Pt_pot(i1) = u_Pt_pot(i1) + ppp*0.500D0
                u_Pt_pot(i2) = u_Pt_pot(i2) + ppp*0.500D0
            end do LP1
        ! 上部Ptが下部Ptから受ける力
        LP2:do i2 = 1, l_Pt_N
                divs(:) = u_Pt_pos(i1, :) - l_Pt_pos(i2, :)

                do j = 1,3
                    if (divs(j) < -Pt_Pt_cutoff(j)) then
                        divs(j) = divs(j) + syul(j)
                    else if(divs(j) > Pt_Pt_cutoff(j)) then
                        divs(j) = divs(j) - syul(j)
                    endif
                end do

                do j = 1,3
                    divs(j) = divs(j) / Pt_Pt_sig
                    if (divs(j) > cutoff) cycle LP2
                    if (divs(j) < -cutoff) cycle LP2
                end do

                dit2 = divs(1)*divs(1) + divs(2)*divs(2) + divs(3)*divs(3)
                dist = dsqrt(dit2)
                if (dist > cutoff) cycle

                dit4 = dit2*dit2
                dit6 = dit4*dit2
                dit8 = dit4*dit4
                dit12 = dit6*dit6
                dit14 = dit8*dit6

                ppp = 4.00D0*Pt_Pt_eps*(1.00D0/dit12-1.00D0/dit6)
                force = Pt_Pt_cforce*(-2.00D0/dit14+1.00D0/dit8)

                forces(:) = -force * divs(:) / Pt_M
                u_Pt_for(i1, :) = u_Pt_for(i1, :) + forces(:)
                l_Pt_for(i2, :) = l_Pt_for(i2, :) - forces(:)

                u_Pt_pot(i1) = u_Pt_pot(i1) + ppp*0.500D0
                l_Pt_pot(i2) = l_Pt_pot(i2) + ppp*0.500D0
            end do LP2

        ! 上部PtがArから受ける力
        LP3:do i2 = 1, Ar_N
                divs(:) = u_Pt_pos(i1, :) - Ar_pos(i2, :)

                do j = 1,3
                    if (divs(j) < -Pt_Ar_cutoff(j)) then
                        divs(j) = divs(j) + syul(j)
                    else if(divs(j) > Pt_Ar_cutoff(j)) then
                        divs(j) = divs(j) - syul(j)
                    endif
                end do

                do j = 1,3
                    divs(j) = divs(j) / Pt_Ar_sig
                    if (divs(j) > cutoff) cycle LP3
                    if (divs(j) < -cutoff) cycle LP3
                end do

                dit2 = divs(1)*divs(1) + divs(2)*divs(2) + divs(3)*divs(3)
                dist = dsqrt(dit2)
                if (dist > cutoff) cycle

                dit4 = dit2*dit2
                dit6 = dit4*dit2
                dit8 = dit4*dit4
                dit12 = dit6*dit6
                dit14 = dit8*dit6

                ppp = 4.00D0*Pt_Ar_eps*(1.00D0/dit12-1.00D0/dit6)
                force = Pt_Ar_cforce*(-2.00D0/dit14+1.00D0/dit8)

                forces(:) = -force * divs(:) / Pt_M
                u_Pt_for(i1, :) = u_Pt_for(i1, :) + forces(:)
                Ar_for(i2, :) = Ar_for(i2, :) - forces(:)

                u_Pt_pot(i1) = u_Pt_pot(i1) + ppp*0.500D0
                Ar_pot(i2) = Ar_pot(i2) + ppp*0.500D0
            end do LP3
    end do

    ! 下部Ptが受ける力
    do i1 = 1, l_Pt_N
        ! 下部Ptが下部Ptから受ける力
        LP4:do i2 = i1+1, l_Pt_N
                divs(:) = l_Pt_pos(i1, :) - l_Pt_pos(i2, :)

                do j = 1,3
                    if (divs(j) < -Pt_Pt_cutoff(j)) then
                        divs(j) = divs(j) + syul(j)
                    else if(divs(j) > Pt_Pt_cutoff(j)) then
                        divs(j) = divs(j) - syul(j)
                    endif
                end do

                do j = 1,3
                    divs(j) = divs(j) / Pt_Pt_sig
                    if (divs(j) > cutoff) cycle LP4
                    if (divs(j) < -cutoff) cycle LP4
                end do

                dit2 = divs(1)*divs(1) + divs(2)*divs(2) + divs(3)*divs(3)
                dist = dsqrt(dit2)
                if (dist > cutoff) cycle

                dit4 = dit2*dit2
                dit6 = dit4*dit2
                dit8 = dit4*dit4
                dit12 = dit6*dit6
                dit14 = dit8*dit6

                ppp = 4.00D0*Pt_Pt_eps*(1.00D0/dit12-1.00D0/dit6)
                force = Pt_Pt_cforce*(-2.00D0/dit14+1.00D0/dit8)

                forces(:) = -force * divs(:) / Pt_M
                l_Pt_for(i1, :) = l_Pt_for(i1, :) + forces(:)
                l_Pt_for(i2, :) = l_Pt_for(i2, :) - forces(:)

                l_Pt_pot(i1) = l_Pt_pot(i1) + ppp*0.500D0
                l_Pt_pot(i2) = l_Pt_pot(i2) + ppp*0.500D0
            end do LP4
        ! 下部PtがArから受ける力
        LP5:do i2 = 1, Ar_N
                divs(:) = l_Pt_pos(i1, :) - Ar_pos(i2, :)

                do j = 1,3
                    if (divs(j) < -Pt_Ar_cutoff(j)) then
                        divs(j) = divs(j) + syul(j)
                    else if(divs(j) > Pt_Ar_cutoff(j)) then
                        divs(j) = divs(j) - syul(j)
                    endif
                end do

                do j = 1,3
                    divs(j) = divs(j) / Pt_Ar_sig
                    if (divs(j) > cutoff) cycle LP5
                    if (divs(j) < -cutoff) cycle LP5
                end do

                dit2 = divs(1)*divs(1) + divs(2)*divs(2) + divs(3)*divs(3)
                dist = dsqrt(dit2)
                if (dist > cutoff) cycle

                dit4 = dit2*dit2
                dit6 = dit4*dit2
                dit8 = dit4*dit4
                dit12 = dit6*dit6
                dit14 = dit8*dit6

                ppp = 4.00D0*Pt_Ar_eps*(1.00D0/dit12-1.00D0/dit6)
                force = Pt_Ar_cforce*(-2.00D0/dit14+1.00D0/dit8)

                forces(:) = -force * divs(:) / Pt_M
                l_Pt_for(i1, :) = l_Pt_for(i1, :) + forces(:)
                Ar_for(i2, :) = Ar_for(i2, :) - forces(:)

                l_Pt_pot(i1) = l_Pt_pot(i1) + ppp*0.500D0
                Ar_pot(i2) = Ar_pot(i2) + ppp*0.500D0
            end do LP5
    end do

    ! Arが受ける力
    do i1 = 1, Ar_N
        ! ArがArから受ける力
        LP6:do i2 = i1+1, Ar_N
                divs(:) = Ar_pos(i1, :) - Ar_pos(i2, :)

                do j = 1,3
                    if (divs(j) < -Ar_Ar_cutoff(j)) then
                        divs(j) = divs(j) + syul(j)
                    else if(divs(j) > Ar_Ar_cutoff(j)) then
                        divs(j) = divs(j) - syul(j)
                    endif
                end do

                do j = 1,3
                    divs(j) = divs(j) / Ar_Ar_sig
                    if (divs(j) > cutoff) cycle LP6
                    if (divs(j) < -cutoff) cycle LP6
                end do

                dit2 = divs(1)*divs(1) + divs(2)*divs(2) + divs(3)*divs(3)
                dist = dsqrt(dit2)
                if (dist > cutoff) cycle

                dit4 = dit2*dit2
                dit6 = dit4*dit2
                dit8 = dit4*dit4
                dit12 = dit6*dit6
                dit14 = dit8*dit6

                ppp = 4.00D0*Ar_Ar_eps*(1.00D0/dit12-1.00D0/dit6)
                force = Ar_Ar_cforce*(-2.00D0/dit14+1.00D0/dit8)

                forces(:) = -force * divs(:) / Ar_M
                Ar_for(i1, :) = Ar_for(i1, :) + forces(:)
                Ar_for(i2, :) = Ar_for(i2, :) - forces(:)

                Ar_pot(i1) = Ar_pot(i1) + ppp*0.500D0
                Ar_pot(i2) = Ar_pot(i2) + ppp*0.500D0
            end do LP6
    end do
end subroutine calc_potential