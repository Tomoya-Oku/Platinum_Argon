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
    stdist = 4.00
    ofst(1) = 1.00
    ofst(2) = 1.00
    ofst(3) = 59 ! 上下のPt層を中心から話す距離
    
    ! 上部Ptを配置
    do k=1,3 ! z
        xyz(3) = ofst(3) + dble(k-1)*2.0D0
        do i=1,16 ! x
            xyz(1) = ofst(1) + dble(i-1)*stdist/2.0D0
            do j=1,8 ! y
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
    do k=1,3 ! z
        xyz(3) = 1 + dble(k-1)*2.0D0
        do i=1,16 ! x
            xyz(1) = ofst(1) + dble(i-1)*stdist/2.0D0
            do j=1,8 ! y
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
    xyz(:) = 0

    ! Arを配置
    do k=1,8 ! z
        xyz(3) = dble(k-1)*2.0D0
        do i=1,16 ! x
            xyz(1) = dble(i-1)*2.0D0
            do j=1,8 ! y
                ! iとkの偶奇が一致する
                if(mod(k,2) == mod(i,2)) then
                    xyz(2) = dble(j-1)*stdist
                else
                    xyz(2) = dble(j-1)*stdist + stdist/2.0D0
                endif
                num = num + 1
                Ar_pos(num, :) = xyz(:)
            end do
        end do
    end do

    cr = 1.00D-6

    ! 速度の初期化
    do i=1, u_Pt_N
        read(1,*)ran
        alpha = PI*ran
        read(2,*)ran
        beta = 2.000D0*PI*ran
        v(1) = dsin(alpha)*dcos(beta)*cr
        v(2) = dsin(alpha)*dsin(beta)*cr
        v(3) = dcos(alpha)*cr
        u_Pt_vel(i, :) = v(:)
    end do
    do i=1, l_Pt_N
        read(1,*)ran
        alpha = PI*ran
        read(2,*)ran
        beta = 2.000D0*PI*ran
        v(1) = dsin(alpha)*dcos(beta)*cr
        v(2) = dsin(alpha)*dsin(beta)*cr
        v(3) = dcos(alpha)*cr
        l_Pt_vel(i, :) = v(:)
    end do
    do i=1, Ar_N
        read(1,*)ran
        alpha = PI*ran
        read(2,*)ran
        beta = 2.000D0*PI*ran
        v(1) = dsin(alpha)*dcos(beta)*cr
        v(2) = dsin(alpha)*dsin(beta)*cr
        v(3) = dcos(alpha)*cr
        Ar_vel(i, :) = v(:)
    end do

end subroutine initialize