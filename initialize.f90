subroutine initialize
    use variables
    use parameters
    implicit none
    integer :: num, i, j, k
    double precision :: stdist, ran, alpha, beta, cr
    double precision, dimension(3) :: xyz
    double precision, dimension(3) :: ofst
    double precision, dimension(3) :: v

    Pt_Pt_cforce = 24.00D0 *  Pt_Pt_eps /  Pt_Pt_sig
    Pt_Ar_cforce = 24.00D0 *  Pt_Ar_eps /  Pt_Ar_sig
    Ar_Ar_cforce = 24.00D0 *  Ar_Ar_eps /  Ar_Ar_sig

    syul(:) = ssize(:)

    do i = 1,3
        write(9,*) syul(i)
    end do

    num = 0
    Pt_Pt_cutoff(:) = syul(:) - cutoff * Pt_Pt_sig
    Pt_Ar_cutoff(:) = syul(:) - cutoff * Pt_Pt_sig
    Ar_Ar_cutoff(:) = syul(:) - cutoff * Pt_Pt_sig
    stdist = 8.00D0
    ofst(3) = 32
    
    ! 上部Ptを配置
    do k=1,4
        xyz(3) = ofst(3) + dble(k-1)*stdist/2.0D0
        do i=1,8
            xyz(1) = ofst(1) + dble(i-1)*stdist/2.0D0
            do j=1,8
                ! iとkの偶奇が一致する
                if(mod(k,2) == mod(i,2)) then
                    xyz(2) = ofst(2) + dble(j-1)*stdist
                else
                    xyz(2) = ofst(2) + dble(j-1)*stdist + stdist/2.0D0
                endif
                num = num + 1
                u_Pt_pos(num, :) = xyz(:)
            end do
        end do
    end do

    num = 0

    ! 下部Ptを配置
    do k=1,4
        xyz(3) = -ofst(3) + dble(k-1)*stdist/2.0D0
        do i=1,8
            xyz(1) = ofst(1) + dble(i-1)*stdist/2.0D0
            do j=1,8
                ! iとkの偶奇が一致する
                if(mod(k,2) == mod(i,2)) then
                    xyz(2) = ofst(2) + dble(j-1)*stdist
                else
                    xyz(2) = ofst(2) + dble(j-1)*stdist + stdist/2.0D0
                endif
                num = num + 1
                l_Pt_pos(num, :) = xyz(:)
            end do
        end do
    end do

    num = 0

    ! 下部Ptを配置
    do k=1,8
        xyz(3) = ofst(3) + dble(k-1)*stdist/2.0D0
        do i=1,8
            xyz(1) = ofst(1) + dble(i-1)*stdist/2.0D0
            do j=1,8
                ! iとkの偶奇が一致する
                if(mod(k,2) == mod(i,2)) then
                    xyz(2) = ofst(2) + dble(j-1)*stdist
                else
                    xyz(2) = ofst(2) + dble(j-1)*stdist + stdist/2.0D0
                endif
                num = num + 1
                Ar_pos(num, :) = xyz(:)
            end do
        end do
    end do

    cr = 1.00D-6

    do i=1, Ar_N
        read(1,*)ran
        alpha = pi*ran
        read(2,*)ran
        beta = 2.000D0*pi*ran
        v(1) = dsin(alpha)*dcos(beta)*cr
        v(2) = dsin(alpha)*dsin(beta)*cr
        v(3) = dcos(alpha)*cr
        Ar_vel(i, :) = v(:)
    end do

end subroutine initialize