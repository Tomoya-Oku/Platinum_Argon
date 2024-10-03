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
    ! => [N]: [kg]*[m/s^2] = 10^-26 * 10^-10 * 10^30 = 10^-6 -> 1
    ! => [Pa]: [N]/[m^2] = 10^-6 * 10^20 = 10^14 -> 1
    ! => [W]: [J]/[s] = 10^-16 * 10^15 = 10^-1 -> 1
    ! => [W/m^2]: [W]/[m^2] = 10^-1 * 10^20 = 10^19 -> 1
    ! => [J/m^2]: [J]/[m^2] = 10^-16 * 10^20 = 10^4 -> 1

    ! 不変定数(有次元)
    double precision, parameter :: AVOGADRO = 6.022D+23 ! Avogadro数
    double precision, parameter :: BOLTZMANN = 1.380649D-23 ! Boltzmann定数[J/K]
    double precision, parameter :: PI = 3.141592654D0 ! 円周率
    double precision, parameter :: DIRAC = 1.054571817D-34 ! Dirac定数[Js]
    !double precision, parameter :: GAS_CONSTANT = 8.31451 ! 一般気体定数[J/mol*K]

    ! 実験パラメータ
    ! 分子数(x, y, z)
    integer, parameter :: xyz_Pt(3) = [integer :: 8, 4, 4]
    integer, parameter :: xyz_Ar(3) = [integer :: 6, 3, 17]
    integer, parameter :: xyz(3, 3) = reshape([ &
       xyz_Pt(X), xyz_Pt(X), xyz_Ar(X), &
       xyz_Pt(Y), xyz_Pt(Y), xyz_Ar(Y), &
       xyz_Pt(Z), xyz_Pt(Z), xyz_Ar(Z) &
    ], [3, 3])

    integer, parameter :: N_uP = xyz_Pt(1) * xyz_Pt(2) * xyz_Pt(3) ! 上部Pt 分子数
    integer, parameter :: N_lP = xyz_Pt(1) * xyz_Pt(2) * xyz_Pt(3) ! 下部Pt 分子数
    integer, parameter :: N_Ar = xyz_Ar(1) * xyz_Ar(2) * xyz_Ar(3) ! Ar分子数
    integer, parameter :: N_All = N_uP + N_lP + N_Ar ! すべての分子数
    integer, parameter :: N(4) = [N_uP, N_lP, N_Ar, N_All]
    integer, parameter :: N_Max = max(N_uP, N_lP, N_Ar)
    integer, parameter :: N_LAYER = xyz_Pt(1)*xyz_Pt(2)
    integer, parameter :: PHANTOM_LAYER = 1
    integer, parameter :: INTERFACE_LAYER = xyz_Pt(Z) - PHANTOM_LAYER - 1

    integer, parameter :: INTERFACE_START(2) = [1, N_LAYER*(PHANTOM_LAYER+1)+1]
    integer, parameter :: INTERFACE_END(2) = [N_LAYER*INTERFACE_LAYER, N(L_PT)]
    integer, parameter :: PHANTOM_START(2) = [N_LAYER*INTERFACE_LAYER+1, N_LAYER+1]
    integer, parameter :: PHANTOM_END(2) = [N_LAYER*(INTERFACE_LAYER+PHANTOM_LAYER), N_LAYER*(PHANTOM_LAYER+1)]
    integer, parameter :: FIXED_START(2) = [N_LAYER*(INTERFACE_LAYER+PHANTOM_LAYER)+1, 1]
    integer, parameter :: FIXED_END(2) = [N(U_PT), N_LAYER]

    double precision, parameter :: CUTOFFperSIG = 3.000D0 ! カットオフ長さ/σ
    double precision, parameter :: CUTOFFperSIG2 = CUTOFFperSIG * CUTOFFperSIG
    double precision, parameter :: DT = 1.00D0 ! 無次元時間ステップ(無次元)
    integer, parameter :: TOTALSTEP = 1D6 ! 最大ステップ数
    integer, parameter :: NVTSTEP = 0.2D6 ! 温度補正を止めるステップ
    integer, parameter :: CALCSTEP = 0.7D6 ! 本計算を開始するステップ

    ! 分子固有定数
    double precision, parameter :: MOLMASS_Pt = 195.084D-3 ! Ptのモル質量[kg/mol]
    double precision, parameter :: MOLMASS_Ar = 39.9500D-3 ! Arのモル質量[kg/mol]
    double precision, parameter :: MASS_Pt = MOLMASS_Pt / AVOGADRO * 1.0D26 ! Pt分子の質量(無次元)
    double precision, parameter :: MASS_Ar = MOLMASS_Ar / AVOGADRO * 1.0D26 ! Ar分子の質量(無次元)
    double precision, parameter :: MASS(3) = [MASS_Pt, MASS_Pt, MASS_Ar]
    double precision, parameter :: MASS_inv(3) = [1/MASS_Pt, 1/MASS_Pt, 1/MASS_Ar]

    !! L-Jパラメータ (+Lorentz-Berthelot則)
    double precision, parameter :: ALPHA_PtPt = 1.0D0 ! Pt-Pt間相互作用強さ
    double precision, parameter :: ALPHA_PtAr = 0.1D0 ! Pt-Pt間相互作用強さ
    double precision, parameter :: ALPHA_ArAr = 1.0D0 ! Pt-Pt間相互作用強さ
    double precision, parameter :: ALPHA(3,3) = reshape([ &
    ALPHA_PtPt, ALPHA_PtPt, ALPHA_PtAr, &
    ALPHA_PtPt, ALPHA_PtPt, ALPHA_PtAr, &
    ALPHA_PtAr, ALPHA_PtAr, ALPHA_ArAr ], shape(ALPHA))
    double precision, parameter :: SIG_PtPt = 2.54D0  ! Pt-Pt間σ(無次元)
    double precision, parameter :: EPS_PtPt = 109.2D-5 ! Pt-Pt間ε(無次元)
    double precision, parameter :: SIG_ArAr = 3.405D0  ! Ar-Ar間σ(無次元)
    double precision, parameter :: EPS_ArAr = 1.65399D-5 ! Ar-Ar間ε(無次元)
    double precision, parameter :: SIG_PtAr = (SIG_PtPt + SIG_ArAr) / 2   ! Pt-Ar間σ(無次元)
    double precision, parameter :: EPS_PtAr = dsqrt(EPS_PtPt*EPS_ArAr) ! Pt-Ar間ε(無次元)
    double precision, parameter :: SIG(3,3) = reshape([ &
    SIG_PtPt, SIG_PtPt, SIG_PtAr, &
    SIG_PtPt, SIG_PtPt, SIG_PtAr, &
    SIG_PtAr, SIG_PtAr, SIG_ArAr ], shape(SIG))
    double precision, parameter :: SIG_inv(3,3) = reshape([ &
    1/SIG_PtPt, 1/SIG_PtPt, 1/SIG_PtAr, &
    1/SIG_PtPt, 1/SIG_PtPt, 1/SIG_PtAr, &
    1/SIG_PtAr, 1/SIG_PtAr, 1/SIG_ArAr ], shape(SIG_inv))
    double precision, parameter :: EPS(3,3) = reshape([ &
    EPS_PtPt, EPS_PtPt, EPS_PtAr, &
    EPS_PtPt, EPS_PtPt, EPS_PtAr, &
    EPS_PtAr, EPS_PtAr, EPS_ArAr ], shape(EPS))

    !! L-Jポテンシャルから力を計算するための係数
    double precision, parameter :: COEF_PtPt = 24.00D0 * ALPHA_PtPt * EPS_PtPt / SIG_PtPt
    double precision, parameter :: COEF_PtAr = 24.00D0 * ALPHA_PtAr * EPS_PtAr / SIG_PtAr
    double precision, parameter :: COEF_ArAr = 24.00D0 * ALPHA_ArAr * EPS_ArAr / SIG_ArAr
    double precision, parameter :: COEF(3,3) = reshape([ &
    COEF_PtPt, COEF_PtPt, COEF_PtAr, &
    COEF_PtPt, COEF_PtPt, COEF_PtAr, &
    COEF_PtAr, COEF_PtAr, COEF_ArAr ], shape(COEF))

    ! 分子間安定距離
    double precision, parameter :: STDIST_Pt = 2.0D0**(2.0D0/3.0D0) * SIG_PtPt
    double precision, parameter :: STDIST_Ar = 2.0D0**(2.0D0/3.0D0) * SIG_ArAr
    double precision, parameter :: STDIST(3) = [STDIST_Pt, STDIST_Pt, STDIST_Ar]

    ! 系のスケール
    double precision, parameter :: SSIZE(3) = [xyz(U_PT, X)*(STDIST_Pt/2), xyz(U_PT, Y)*STDIST_Pt, 60.0D0]
    double precision, parameter :: ofst_U_Pt_X = (SSIZE(X) - (STDIST_Pt/2) * (xyz(U_PT, X) - 1)) / 2.0D0
    double precision, parameter :: ofst_U_Pt_Y = (SSIZE(Y) - STDIST_Pt * (xyz(U_PT, Y) - 0.5D0)) / 2.0D0
    double precision, parameter :: ofst_U_Pt_Z = SSIZE(3) - (STDIST(U_PT)/2) * (xyz(U_PT, Z) - 1)
    double precision, parameter :: ofst_U_Pt(3) = [ofst_U_Pt_X, ofst_U_Pt_Y, ofst_U_Pt_Z]
    double precision, parameter :: ofst_L_PT_X = (SSIZE(X) - (STDIST_Pt/2) * (xyz(L_PT, X) - 1)) / 2.0D0
    double precision, parameter :: ofst_L_PT_Y = (SSIZE(Y) - STDIST_Pt * (xyz(L_PT, Y) - 0.5D0)) / 2.0D0
    double precision, parameter :: ofst_L_PT_Z = 0.0D0
    double precision, parameter :: ofst_L_PT(3) = [ofst_L_PT_X, ofst_L_PT_Y, ofst_L_PT_Z]
    double precision, parameter :: ofst_Ar_X = (SSIZE(X) - (STDIST_Ar/2) * (xyz(AR, X) - 1.0D0)) / 2.0D0
    double precision, parameter :: ofst_Ar_Y = (SSIZE(Y) - STDIST_Ar * (xyz(AR, Y) - 0.5D0)) / 2.0D0
    double precision, parameter :: ofst_Ar_Z = (SSIZE(Z) - (STDIST_Ar/2) * (xyz(AR, Z) - 1.0D0)) / 2.0D0
    double precision, parameter :: ofst_Ar(3) = [ofst_Ar_X, ofst_Ar_Y, ofst_Ar_Z]
    double precision, parameter :: OFST(3,3) = reshape([ &
    ofst_U_Pt(1), ofst_L_Pt(1), ofst_Ar(1), &
    ofst_U_Pt(2), ofst_L_Pt(2), ofst_Ar(2), &
    ofst_U_Pt(3), ofst_L_Pt(3), ofst_Ar(3) ], shape(OFST))

    ! 断面積
    double precision, parameter :: A = SSIZE(X) * SSIZE(Y)

    ! 体積
    double precision, parameter :: V = A * (SSIZE(Z) - 2 * STDIST_Pt)

    ! 圧力関連
    !double precision, parameter :: COEF_IP = (N(AR) / AVOGADRO) * GAS_CONSTANT * 1.0D16 / V

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
    double precision, parameter :: DEBYE_TEMP = 240 ! Debye温度(Pt)
    double precision, parameter :: DEBYE_FREQUENCY = (BOLTZMANN * 1.0D16) * DEBYE_TEMP / (DIRAC * 1.0D31)
    double precision, parameter :: GAMMA = 6 / PI / DEBYE_FREQUENCY
    double precision, parameter :: ATEMP(2) = [double precision :: 110.0D0, 90.0D0] ! 目標温度[K]
    double precision, parameter :: DAMPCOEF(2) = [-MASS(U_PT) / GAMMA, -MASS(L_PT) / GAMMA]
    double precision, parameter :: RANDCOEF(2) = [dsqrt(2.0D0 * (BOLTZMANN * 1.0D16) * ATEMP(U_PT) * MASS(U_PT) / GAMMA / DT ),&
    dsqrt(2.0D0 * (BOLTZMANN * 1.0D16) * ATEMP(L_PT) * MASS(L_PT) / GAMMA / DT )]

    ! 熱流束関連
    integer, parameter :: PARTITION = 20 
    double precision, parameter :: LAYER = (ofst_U_Pt_Z - STDIST_Pt) / PARTITION
    double precision, parameter :: z_phantom_upper = OFST(U_PT, Z) + STDIST(U_PT)/2.0D0
    double precision, parameter :: z_interface_upper = OFST(U_PT, Z)
    double precision, parameter :: z_interface_lower = OFST(L_PT, Z) + STDIST(L_PT)
    double precision, parameter :: z_phantom_lower = OFST(L_PT, Z) + STDIST(L_PT)/2.0D0

    ! record用
    integer, parameter :: DAT_RANDOM1 = 1
    integer, parameter :: DAT_RANDOM0 = 2
    integer, parameter :: DAT_POSIT = 3
    integer, parameter :: DAT_VELOCITY = 4
    integer, parameter :: DAT_VELENE = 5
    integer, parameter :: DAT_DFORCE = 22
    integer, parameter :: DAT_RFORCE = 23
    integer, parameter :: DAT_TEMP = 8
    integer, parameter :: DAT_PERIODIC = 9
    integer, parameter :: DAT_POS = 10
    integer, parameter :: DAT_MASK = 11
    integer, parameter :: DAT_LOG = 12
    integer, parameter :: DAT_ACCELERATION = 13
    integer, parameter :: DAT_TEMP_INTERFACE = 14
    integer, parameter :: DAT_ENERGY = 15
    integer, parameter :: DAT_TEMP_PHANTOM = 16
    integer, parameter :: DAT_TEMP_DISTRIBUTION = 17
    integer, parameter :: DAT_HEATFLUX = 18
    integer, parameter :: DAT_PRESSURE = 19
    integer, parameter :: DAT_CONDUCTIVITY = 20
    integer, parameter :: DAT_RESISTANCE = 21
    integer, parameter :: DAT_COND = 7
    integer, parameter :: DAT_Q = 24

end module parameters