module variables
    use parameters
    implicit none

    integer :: nowstp = 0 ! 現在のステップ数

    ! 座標(種類，番号, 座標)
    ! 種類: 1: 上部Pt，2: 下部Pt，3: Ar
    double precision, dimension(3, N_Max, 3) :: pos
    
    ! 速度(種類，番号, 座標)
    ! 種類: 1: 上部Pt，2: 下部Pt，3: Ar
    double precision, dimension(3, N_Max, 3) :: vel

    ! 加速度(種類，番号, 座標)
    ! 種類: 1: 上部Pt，2: 下部Pt，3: Ar
    double precision, dimension(3, N_Max, 3) :: acc

    ! ポテンシャル(種類，番号)
    ! 種類: 1: 上部Pt，2: 下部Pt，3: Ar
    double precision, dimension(3, N_Max) :: pot

    ! 運動エネルギー(種類，番号)
    ! 種類: 1: 上部Pt，2: 下部Pt，3: Ar
    double precision, dimension(3, N_Max) :: kin

    ! 界面粒子の運動エネルギー
    double precision :: kin_interface(2, xyz_uP(X)*xyz_uP(Y))
    double precision :: kin_interface_sum(2)

    ! Phantom層粒子の運動エネルギー
    double precision :: kin_phantom(2, xyz_uP(X)*xyz_uP(Y))
    double precision :: kin_phantom_sum(2)

    ! 温度分布関連
    double precision :: kin_layer(PARTITION)
    double precision :: temp_layer(PARTITION)

    ! 分子の種類
    ! total_enegy, total_potential, total_kinetic
    double precision :: energy(4, 3)
    ! temperature
    double precision :: temp(3), temp_interface(2), temp_phantom(2)

end module variables