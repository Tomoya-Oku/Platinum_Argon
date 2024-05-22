module parameters
    implicit none

    ! 無次元化について
    ! M[kg]: 10^-26 -> 1
    ! L [m]: 10^-10 -> 1
    ! T [s]: 10^-15 -> 1
    ! E [J]: 10^-16 -> 1

    ! 実験パラメータ
    double precision, parameter :: u_Pt_atemp = 50.0D0 ! 上部Pt目標温度[K]
    double precision, parameter :: l_Pt_atemp = 50.0D0 ! 下部Pt目標温度[K]
    double precision, parameter :: Ar_atemp = 200.0D0 ! Ar目標温度[K]
    double precision, parameter :: cutoff = 3.000D0 ! カットオフ長さ/σ
    double precision, parameter :: dt = 5.00D0 ! 無次元時間ステップ(無次元)
    integer, parameter :: maxstep = 20000 ! 最大ステップ数
    integer, parameter :: stpstep = 10000 ! 温度補正を止めるステップ

    ! 分子数
    integer, parameter :: u_Pt_N = 256 ! 上部Pt分子数
    integer, parameter :: l_Pt_N = 256 ! 下部Pt分子数
    integer, parameter :: Ar_N = 512   ! Ar分子数

    ! スケール
    double precision, parameter :: ssize(3) = [double precision :: 64.0D0, 64.0D0, 128.0D0] ! 系の大きさ
    double precision, parameter :: u_Pt_thickness = 16.0D0 ! 上部Pt層厚み
    double precision, parameter :: l_Pt_thickness = 16.0D0 ! 下部Pt層厚み

    ! 分子固有定数
    ! double precision, parameter :: Pt_Mmol = 195.084D-3 ! Ptのモル質量[kg/mol]
    ! double precision, parameter :: Ar_Mmol = 39.9500D-3 ! Arのモル質量[kg/mol]
    double precision, parameter :: Pt_M = 32.395 ! Pt分子の質量(無次元)
    double precision, parameter :: Ar_M = 6.6340 ! Ar分子の質量(無次元)

    ! L-Jパラメータ (+Lorentz-Berthelot則)
    double precision, parameter :: Pt_Pt_sig = 2.475D0  ! Pt-Pt間σ(無次元)
    double precision, parameter :: Pt_Pt_eps = 83.31D-5 ! Pt-Pt間ε(無次元)
    double precision, parameter :: Pt_Ar_sig = 2.938D0  ! Pt-Ar間σ(無次元)
    double precision, parameter :: Pt_Ar_eps = 11.78D-5 ! Pt-Ar間ε(無次元)
    double precision, parameter :: Ar_Ar_sig = 3.400D0  ! Ar-Ar間σ(無次元)
    double precision, parameter :: Ar_Ar_eps = 1.666D-5 ! Ar-Ar間ε(無次元)

    ! 不変定数
    double precision, parameter :: AVOGADRO = 6.022D+23 !アボガドロ数
    double precision, parameter :: BOLTZMANN = 1.3806662D-23 !ボルツマン定数
    double precision, parameter :: PI = 3.141592654D0 !円周率

end module parameters

module variables
    use parameters
    implicit none

    integer :: nowstp ! 現在のステップ数

    double precision :: Pt_Pt_cforce, Pt_Ar_cforce, Ar_Ar_cforce !力の計算の係数
    double precision, dimension(3) :: Pt_Pt_cutoff, Pt_Ar_cutoff, Ar_Ar_cutoff ! ポテンシャルのカットオフ長さx,y,z方向
    double precision, dimension(3) :: syul ! ポテンシャルのカットオフ長さx,y,z方向，x,y,z方向の周期長さ

    ! 位置
    double precision, dimension(u_Pt_N, 3) :: u_Pt_pos ! 上部Ptの座標(番号, 座標)
    double precision, dimension(l_Pt_N, 3) :: l_Pt_pos ! 下部Ptの座標(番号, 座標)
    double precision, dimension(Ar_N, 3) :: Ar_pos ! Arの座標(番号, 座標)
    
    ! 速度
    double precision, dimension(u_Pt_N, 3) :: u_Pt_vel ! 上部Ptの速度(番号, 座標)
    double precision, dimension(l_Pt_N, 3) :: l_Pt_vel ! 下部Ptの速度(番号, 座標)
    double precision, dimension(Ar_N, 3) :: Ar_vel ! Arの速度(番号, 座標)

    ! 加速度
    double precision, dimension(u_Pt_N, 3) :: u_Pt_acc ! 上部Ptの加速度(番号, 座標)
    double precision, dimension(l_Pt_N, 3) :: l_Pt_acc ! 下部Ptの加速度(番号, 座標)
    double precision, dimension(Ar_N, 3) :: Ar_acc ! Arの加速度(番号, 座標)

    ! 力
    double precision, dimension(u_Pt_N, 3) :: u_Pt_for ! 上部Ptに働く力(番号, 座標)
    double precision, dimension(l_Pt_N, 3) :: l_Pt_for ! 下部Ptに働く力(番号, 座標)
    double precision, dimension(Ar_N, 3) :: Ar_for ! Arに働く力(番号, 座標)

    ! ポテンシャル
    double precision, dimension(u_Pt_N) :: u_Pt_pot
    double precision, dimension(l_Pt_N) :: l_Pt_pot
    double precision, dimension(Ar_N) :: Ar_pot

    ! 運動エネルギー
    double precision, dimension(u_Pt_N) :: u_Pt_kin
    double precision, dimension(l_Pt_N) :: l_Pt_kin
    double precision, dimension(Ar_N) :: Ar_kin

end module variables