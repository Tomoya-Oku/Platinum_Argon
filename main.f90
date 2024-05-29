program main
    use variables, only: nowstp
    use parameters
    implicit none
    integer :: i

    ! 読み込み用乱数ファイル
        open(1,file='random1.dat')
        open(2,file='random0.dat')
        open(3,file='posit.dat')
    ! 各分子の速度データの出力
        open(4,file='velocity.dat')
    ! 系のエネルギーデータの出力
        open(7,file='energy.dat')
        open(8,file='energy_u_Pt.dat')
        open(10,file='energy_l_Pt.dat')
        open(11,file='energy_Ar.dat')
    ! 系の周期長さの出力
        open(9,file='period_length.dat')

    nowstp = 0
    
    call initialize ! 各分子の初期位置，初期速度などの設定
    call correct_trspeed ! 系内の全分子の並進速度の補正
    call velocity_scaling ! 速度スケーリング法
    call correct_cogravity ! 系内の全分子の重心の補正
    call record_posvel ! データの出力１
    call record_energy ! データの出力２

    do i = 1, maxstep
        nowstp = i
        ! ステップ数が500の倍数のとき
        if (mod(nowstp, 500) == 0) then
            write(6,*) nowstp, "/", maxstep ! ターミナルに進捗を出力
        endif
        ! NVT一定
        if (nowstp <= stpstep) then
            ! ステップ数が100の倍数のとき
            if (mod(nowstp,100) == 0) then
                call correct_trspeed ! 並進速度の補正
                call velocity_scaling ! 速度スケーリング法
                !call correct_cogravity ! 重心の補正
            endif
        endif
    
        call calc_LeapFlog ! 各分子に働く力，速度，位置の分子動力学計算
        call calc_bound ! 境界条件の付与
        
        ! ステップ数が100の倍数+1のとき
        if(mod(nowstp, 100) == 1) then
            call record_posvel ! データの出力１
            call record_energy ! データの出力２
        endif
    end do

    call record_finposvel ! データの出力３

end program main