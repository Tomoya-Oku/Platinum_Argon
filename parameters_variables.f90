module parameters
    implicit none

    ! 分子グループ
    enum, bind(c)
        enumerator :: U_PT = 1, L_PT = 2, AR = 3, ALL = 4
    end enum

    ! 座標
    enum, bind(c)
        enumerator :: X = 1, Y = 2, Z = 3
    end enum

    ! エネルギー種別
    enum, bind(c)
        enumerator :: TOTAL = 1, POTENTIAL = 2, KINETIC = 3
    end enum

    !! 無次元化について
    ! M[kg]: 10^-26 -> 1
    ! L [m]: 10^-10 -> 1
    ! T [s]: 10^-15 -> 1
    ! E [J]: 10^-16 -> 1

    ! 不変定数
    double precision, parameter :: AVOGADRO = 6.022D+23 ! Avogadro数
    double precision, parameter :: BOLTZMANN = 1.3806662D-23 ! Boltzmann定数
    double precision, parameter :: PI = 3.141592654D0 ! 円周率

    ! 実験パラメータ
    integer, parameter :: xyz_uP(3) = [integer :: 16, 8, 3]
    integer, parameter :: xyz_lP(3) = [integer :: 16, 8, 3]
    integer, parameter :: xyz_Ar(3) = [integer :: 16, 8, 8]
    integer, parameter :: xyz(3, 3) = reshape([ &
       xyz_uP(X), xyz_lP(X), xyz_Ar(X), &
       xyz_uP(Y), xyz_lP(Y), xyz_Ar(Y), &
       xyz_uP(Z), xyz_lP(Z), xyz_Ar(Z) &
       ], [3, 3])

    integer, parameter :: N_uP = xyz_uP(1) * xyz_uP(2) * xyz_uP(3) ! 上部Pt 分子数
    integer, parameter :: N_lP = xyz_lP(1) * xyz_lP(2) * xyz_lP(3) ! 下部Pt 分子数
    integer, parameter :: N_Ar = xyz_Ar(1) * xyz_Ar(2) * xyz_Ar(3) ! Ar分子数
    integer, parameter :: N_Max = max(N_uP, N_lP, N_Ar)
    integer, parameter :: N_All = N_uP + N_lP + N_Ar ! すべての分子数
    integer, parameter :: N(4) = [N_uP, N_lP, N_Ar, N_All]

    double precision, parameter :: CUTOFFperSIG = 3.000D0 ! カットオフ長さ/σ
    double precision, parameter :: DT = 1.00D0 ! 無次元時間ステップ(無次元)
    integer, parameter :: MAXSTEP = 40000 ! 最大ステップ数
    integer, parameter :: NVTSTEP = 20000 ! 温度補正を止めるステップ

    ! 系のスケール
    double precision, parameter :: SSIZE(3) = [double precision :: 32.0D0, 32.0D0, 64.0D0]

    ! 分子固有定数
    ! double precision, parameter :: MOLMASS_Pt = 195.084D-3 ! Ptのモル質量[kg/mol]
    ! double precision, parameter :: MOLMASS_Ar = 39.9500D-3 ! Arのモル質量[kg/mol]
    double precision, parameter :: MASS_Pt = 32.395 ! Pt分子の質量(無次元)
    double precision, parameter :: MASS_Ar = 6.6340 ! Ar分子の質量(無次元)
    double precision, parameter :: MASS(3) = [MASS_Pt, MASS_Pt, MASS_Ar]

    !! L-Jパラメータ (+Lorentz-Berthelot則)
    double precision, parameter :: ALPHA_PtPt = 1.0D0 ! Pt-Pt間相互作用強さ
    double precision, parameter :: ALPHA_PtAr = 0.5D0 ! Pt-Pt間相互作用強さ
    double precision, parameter :: ALPHA_ArAr = 1.0D0 ! Pt-Pt間相互作用強さ
    double precision, parameter :: SIG_PtPt = 2.475D0  ! Pt-Pt間σ(無次元)
    double precision, parameter :: EPS_PtPt = 83.31D-5 ! Pt-Pt間ε(無次元)
    double precision, parameter :: SIG_PtAr = 2.938D0  ! Pt-Ar間σ(無次元)
    double precision, parameter :: EPS_PtAr = 11.78D-5 ! Pt-Ar間ε(無次元)
    double precision, parameter :: SIG_ArAr = 3.400D0  ! Ar-Ar間σ(無次元)
    double precision, parameter :: EPS_ArAr = 1.666D-5 ! Ar-Ar間ε(無次元)
    double precision, parameter :: ALPHA(3,3) = reshape([ &
    ALPHA_PtPt, ALPHA_PtPt, ALPHA_PtAr, &
    ALPHA_PtPt, ALPHA_PtPt, ALPHA_PtAr, &
    ALPHA_PtAr, ALPHA_PtAr, ALPHA_ArAr ], shape(ALPHA))
    double precision, parameter :: SIG(3,3) = reshape([ &
    SIG_PtPt, SIG_PtPt, SIG_PtAr, &
    SIG_PtPt, SIG_PtPt, SIG_PtAr, &
    SIG_PtAr, SIG_PtAr, SIG_ArAr ], shape(SIG))
    double precision, parameter :: EPS(3,3) = reshape([ &
    EPS_PtPt, EPS_PtPt, EPS_PtAr, &
    EPS_PtPt, EPS_PtPt, EPS_PtAr, &
    EPS_PtAr, EPS_PtAr, EPS_ArAr ], shape(EPS))

    !! L-Jポテンシャルから力を計算するための係数
    double precision, parameter :: COEF_PtPt = 24.00D0 * EPS_PtPt / SIG_PtPt
    double precision, parameter :: COEF_PtAr = 24.00D0 * EPS_PtAr / SIG_PtAr
    double precision, parameter :: COEF_ArAr = 24.00D0 * EPS_ArAr / SIG_ArAr
    double precision, parameter :: COEF(3,3) = reshape([ &
    COEF_PtPt, COEF_PtPt, COEF_PtAr, &
    COEF_PtPt, COEF_PtPt, COEF_PtAr, &
    COEF_PtAr, COEF_PtAr, COEF_ArAr ], shape(COEF))

    ! 分子間安定距離
    !double precision, parameter :: STDIST_Pt = 2**(1.0/6.0) * SIG_PtPt
    double precision, parameter :: STDIST_Pt = 3.8
    double precision, parameter :: STDIST_Ar = 2**(1.0/6.0) * SIG_ArAr

    !! ポテンシャルのカットオフ長さx,y,z方向
    double precision, parameter :: CUTOFF_PtPt(3) = SSIZE(:) - CUTOFFperSIG * SIG_PtPt
    double precision, parameter :: CUTOFF_PtAr(3) = SSIZE(:) - CUTOFFperSIG * SIG_PtAr
    double precision, parameter :: CUTOFF_ArAr(3) = SSIZE(:) - CUTOFFperSIG * SIG_ArAr
    double precision, parameter :: CUTOFF(3, 3, 3) = reshape( &
    [ CUTOFF_PtPt(1), CUTOFF_PtPt(1), CUTOFF_PtAr(1), &
      CUTOFF_PtPt(1), CUTOFF_PtPt(1), CUTOFF_PtAr(1), &
      CUTOFF_PtAr(1), CUTOFF_PtAr(1), CUTOFF_ArAr(1), &
      CUTOFF_PtPt(2), CUTOFF_PtPt(2), CUTOFF_PtAr(2), &
      CUTOFF_PtPt(2), CUTOFF_PtPt(2), CUTOFF_PtAr(2), &
      CUTOFF_PtAr(2), CUTOFF_PtAr(2), CUTOFF_ArAr(2), &
      CUTOFF_PtPt(3), CUTOFF_PtPt(3), CUTOFF_PtAr(3), &
      CUTOFF_PtPt(3), CUTOFF_PtPt(3), CUTOFF_PtAr(3), &
      CUTOFF_PtAr(3), CUTOFF_PtAr(3), CUTOFF_ArAr(3) ], &
    shape = [3, 3, 3])

    ! 速度スケーリング法パラメータ
    double precision, parameter :: ATEMP_AR = 100.0D0 ! Ar目標温度[K]

    ! Langevin法パラメータ
    double precision, parameter :: GAMMA = 1.0D0
    double precision, parameter :: ATEMP(2) = [double precision :: 200.0D0, 50.0D0] ! 目標温度[K]

    ! record用
    integer, parameter :: DAT_RANDOM1 = 1
    integer, parameter :: DAT_RANDOM0 = 2
    integer, parameter :: DAT_POSIT = 3
    integer, parameter :: DAT_VELOCITY = 4
    integer, parameter :: DAT_ENERGY_AR = 7
    integer, parameter :: DAT_TEMP = 8
    integer, parameter :: DAT_PERIODIC = 9
    integer, parameter :: DAT_POS = 10
    integer, parameter :: DAT_MASK = 11
    integer, parameter :: DAT_LOG = 12
    integer, parameter :: DAT_ACCELERATION = 13
    integer, parameter :: DAT_TEMP_INTERFACE = 14
    integer, parameter :: DAT_ENERGY_TOTAL = 15

end module parameters

module variables
    use parameters
    implicit none

    integer :: nowstp ! 現在のステップ数

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

    double precision :: kin_interface(2, xyz_uP(X)*xyz_uP(Y))
    double precision :: kin_interface_sum(2)

end module variables