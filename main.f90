program main
    use variables, only: nowstp
    use parameters
    implicit none
    integer :: i

    ! 読み込み用乱数ファイル
    open(DAT_RANDOM1, file='Random/random1.dat')
    open(DAT_RANDOM0, file='Random/random0.dat')

    ! 各分子の位置・速度・加速度データの出力
    open(DAT_POSIT, file='Data/posit.dat')
    open(DAT_VELOCITY, file='Data/velocity.dat')
    !open(DAT_ACCELERATION, file='Data/acceleration.dat')

    ! 系のエネルギーデータの出力
    open(DAT_ENERGY, file='Data/energy.dat')

    ! 系の周期長さの出力
    open(DAT_PERIODIC, file='Data/periodic.dat')

    ! 系の温度
    open(DAT_TEMP, file='Data/temperature.dat')
    ! 界面温度
    open(DAT_TEMP_INTERFACE, file='Data/temp_interface.dat')
    ! Phantom層温度
    open(DAT_TEMP_PHANTOM, file='Data/temp_phantom.dat')
    
    ! 温度分布
    open(DAT_TEMP_DISTRIBUTION, file='Data/temp_layer.dat')

    ! ログ
    open(DAT_LOG, file='Data/log.dat')
    
    call initialize ! 各分子の初期位置，初期速度などの設定
    call vel_scaling ! 速度スケーリング法
    call boundary ! 境界条件
    call record_posvel
    call record_energy
    call record_tempDistribution

    write(6,*) "MAXSTEP, NVTSTEP, DT", MAXSTEP, NVTSTEP, DT
    write(6,*) "SYSTEM SIZE (x, y, z)", SSIZE(x), SSIZE(y), SSIZE(z)
    write(6,*) "Upper Pt: ", N_uP
    write(6,*) "Lower Pt: ", N_lP
    write(6,*) "Ar: ", N_Ar
    write(6,*) "STDIST_Pt, STDIST_Ar", STDIST_Pt, STDIST_Ar

    write(6,*) "LAYER", LAYER

    ! write(6,*) "OFST(U_PT, X)", OFST(U_PT, X)
    ! write(6,*) "OFST(U_PT, Y)", OFST(U_PT, Y)
    ! write(6,*) "OFST(U_PT, Z)", OFST(U_PT, Z)

    ! write(6,*) "OFST(L_PT, X)", OFST(L_PT, X)
    ! write(6,*) "OFST(L_PT, Y)", OFST(L_PT, Y)
    ! write(6,*) "OFST(L_PT, Z)", OFST(L_PT, Z)

    ! write(6,*) "OFST(AR, X)", OFST(AR, X)
    ! write(6,*) "OFST(AR, Y)", OFST(AR, Y)
    ! write(6,*) "OFST(AR, Z)", OFST(AR, Z)

    do i = 1, MAXSTEP
        nowstp = i
        ! ステップ数が500の倍数のとき
        if (mod(nowstp, 500) == 0) then
            write(6,*) nowstp, "/", MAXSTEP ! ターミナルに進捗を出力
        endif
        ! NVT一定
        if (nowstp <= NVTSTEP) then
            ! ステップ数が100の倍数のとき
            if (mod(nowstp,100) == 0) then
                call vel_scaling ! 速度スケーリング法
            endif
        endif
    
        call integral ! 各分子に働く力，速度，位置の分子動力学計算
        call boundary ! 境界条件
        
        ! ステップ数が100の倍数+1のとき
        if(mod(nowstp, 100) == 1) then
            call record_posvel
            call record_energy
            call record_tempDistribution
        endif
    end do

    call record_finposvel

end program main