subroutine initialize
    use variables
    use parameters
    use functions
    implicit none
    integer :: unit
    integer :: num = 0, i, j, k, kind
    double precision :: ran, r1, r2, cr = 1.00D-6
    double precision, dimension(3) :: init_xyz
    double precision, dimension(3) :: w

    ! メモリ動的割り当て
    allocate(pos(3, N_Max, 3))
    allocate(vel(3, N_Max, 3))
    allocate(acc(3, N_Max, 3))
    allocate(for(3, 3, N_Max, N_Max, 3))
    allocate(pot(3, N_Max))
    allocate(kin(3, N_Max))
    allocate(velene(3, N_Max, 3))
    allocate(vel_temp(3, N_Max, 3))

    ! 初期化
    vel(:, :, :) = 0
    velene(:, :, :) = 0
    vel_temp(:, :, :) = 0

    ! ターミナルおよびcondition.datに条件を出力
    do unit = 6, 7
        write(unit, *) "Step"
        write(unit, *) " -TOTAL STEP:", TOTALSTEP
        write(unit, *) " -NVT STEP:", NVTSTEP
        write(unit, *) " -CALCULATION STEP:", CALCSTEP
        write(unit, *) " -dt:", DT

        write(unit, *) "System"
        write(unit, *) " -Size (x,y,z) [nm]:", SSIZE(x) / 10, SSIZE(y) / 10, SSIZE(z) / 10
        write(unit, *) " -Area of cross section [nm^2]:", A / 1.0D2
        write(unit, *) " -Volume (except walls) [nm^3]:", V / 1.0D3
        write(unit, *) " -The number of TOP Pt:", xyz(U_PT, X), "x", xyz(U_PT, Y), "x", xyz(U_PT, Z), "=", N(U_PT)
        write(unit, *) " -The number of BOTTOM Pt:", xyz(L_PT, X), "x", xyz(L_PT, Y), "x", xyz(L_PT, Z), "=", N(L_PT)
        write(unit, *) " -The number of Ar:", xyz(AR, X), "x", xyz(AR, Y), "x", xyz(AR, Z), "=", N(AR)

        write(unit, *) "L-J Potential"
        write(unit, *) " -ALPHA(Pt-Ar):", ALPHA_PtAr
        write(unit, *) " -SIGMA(Pt-Pt):", SIG_PtPt
        write(unit, *) " -EPSILON(Pt-Pt):", EPS_PtPt
        write(unit, *) " -SIGMA(Ar-Ar):", SIG_ArAr
        write(unit, *) " -EPSILON(Ar-Ar):", EPS_ArAr
        write(unit, *) " -SIGMA(Pt-Ar) based on LB:", SIG_PtAr
        write(unit, *) " -EPSILON(Pt-Ar) based on LB:", EPS_PtAr

        write(unit, *) "Velocity Scaling"
        write(unit, *) " -Aim temperature of Ar:", ATEMP_AR

        write(unit, *) "Langevin Thermostat"
        write(unit, *) " -Aim temperature of TOP Pt & BOTTOM Pt:", ATEMP(U_PT), ATEMP(L_PT)
        write(unit, *) " -Debye temperature:", DEBYE_TEMP
        write(unit, *) " -Debye frequency:", DEBYE_FREQUENCY
        write(unit, *) " -GAMMA(dumper coeffecient):", GAMMA
        write(unit, *) " -DAMPCOEF(TOP, BOTTOM):", DAMPCOEF(U_PT), DAMPCOEF(L_PT)
        write(unit, *) " -RANDCOEF(TOP, BOTTOM):", RANDCOEF(U_PT), RANDCOEF(L_PT)

        write(unit, *) "Properties of molecules"
        write(unit, *) " -MASS(Pt, Ar) [10^-25kg]:", MASS_Pt, MASS_Ar
        write(unit, *) " -STABLE DISTANCE(Pt, Ar) [nm]:", STDIST_Pt/10.0D0, STDIST_Ar/10.0D0

        write(unit, *) "Temperature distribution"
        write(unit, *) " -The number of layers:", PARTITION
        write(unit, *) " -Layer thickness:", LAYER
    end do

    close(DAT_COND)

    do i = 1, 3
        write(DAT_PERIODIC,*) SSIZE(i)
    end do

    ! do kind = 1, 3
    !     do i = 1, N(kind)
    !         if (isPhantom(kind, i)) then
    !             write(6, *) "kind=", kind, ", i=", i, " is Phantom Layer."
    !         else if (isInterface(kind, i)) then
    !             write(6, *) "kind=", kind, ", i=", i, " is Interface Layer."
    !         else if (isNotFixed(kind, i) .eqv. .FALSE.) then
    !             write(6, *) "kind=", kind, ", i=", i, " is fixed Layer."
    !         end if
    !     end do
    ! end do
    
    ! 初期配置
    do kind = 1, 3 ! 分子の種類
        do k = 1, xyz(kind, Z) ! z
            init_xyz(Z) = OFST(kind, Z) + dble(k-1)*STDIST(kind)/2.0D0
            do i = 1, xyz(kind, X) ! x
                init_xyz(X) = OFST(kind, X) + dble(i-1)*STDIST(kind)/2.0D0
                do j = 1, xyz(kind, Y) ! y
                    ! iとkの偶奇が一致する
                    if(mod(k,2) == mod(i,2)) then
                        init_xyz(Y) = OFST(kind, Y) + dble(j-1)*STDIST(kind) + STDIST(kind)/2.0D0
                    else
                        init_xyz(Y) = OFST(kind, Y) + dble(j-1)*STDIST(kind)
                    end if
                    num = num + 1
                    pos(kind, num, :) = init_xyz(:)
                end do
            end do
        end do
        num = 0
        init_xyz(:) = 0
    end do

    ! Arに初期速度を与える
    do i = 1, N(AR)
        read(DAT_RANDOM1,*)ran
        r1 = PI*ran
        read(DAT_RANDOM0,*)ran
        r2 = 2.000D0*PI*ran
        w(1) = dsin(r1)*dcos(r2)*cr
        w(2) = dsin(r1)*dsin(r2)*cr
        w(3) = dcos(r1)*cr
        vel(AR, i, :) = w(:)
    end do

    write(6,*) ""
    write(6,*) "Initializing has finished."

end subroutine initialize