program main
    use variables, only: nowstp
    use parameters
    implicit none
    integer :: i

    ! 読み込み用乱数ファイル
        open(DAT_RANDOM1, file='random1.dat')
        open(DAT_RANDOM0, file='random0.dat')
        open(DAT_POSIT, file='posit.dat')
    ! 各分子の速度データの出力
        open(DAT_VELOCITY, file='velocity.dat')
    ! 系のエネルギーデータの出力
        open(DAT_ENERGY_AR, file='energy_Ar.dat')
        open(DAT_ENERGY_TOTAL, file='energy_Total.dat')
    ! 系の周期長さの出力
        open(DAT_PERIODIC, file='periodic.dat')
    ! 系の温度
        open(DAT_TEMP, file='temperature.dat')
    ! ログ
    !   open(DAT_LOG, file='log.dat')
    ! 加速度データ
    !   open(DAT_ACCELERATION, file='acceleration.dat')
    ! Langevin層温度
        open(DAT_TEMP_INTERFACE, file='temp_interface.dat')

    nowstp = 0
    
    call initialize ! 各分子の初期位置，初期速度などの設定
    !call record_header ! ヘッダーを書き込み
    !call correct_trvelocity ! 系内の全分子の並進速度の補正
    call velocity_scaling ! 速度スケーリング法
    !call correct_cogravity ! 系内の全分子の重心の補正
    call calc_bound
    call record_posvel
    call record_energy

    write(6,*) "MAXSTEP, NVTSTEP, DT", MAXSTEP, NVTSTEP, DT
    write(6,*) "N_Ar", N_Ar
    write(6,*) "STDIST_Pt, STDIST_Ar", STDIST_Pt, STDIST_Ar

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
                !call correct_trvelocity ! 並進速度の補正
                call velocity_scaling ! 速度スケーリング法
                !call correct_cogravity ! 重心の補正
            endif
        endif
    
        call calc_LeapFlog ! 各分子に働く力，速度，位置の分子動力学計算
        call calc_bound ! 境界条件の付与
        
        ! ステップ数が100の倍数+1のとき
        if(mod(nowstp, 100) == 1) then
            call record_posvel
            call record_energy
        endif
    end do

    call record_finposvel

end program main