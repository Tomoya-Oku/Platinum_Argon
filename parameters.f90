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
    integer, parameter :: xyz_Ar(3) = [integer :: 10, 5, 20]
    integer, parameter :: xyz(3, 3) = reshape([ &
       xyz_uP(X), xyz_lP(X), xyz_Ar(X), &
       xyz_uP(Y), xyz_lP(Y), xyz_Ar(Y), &
       xyz_uP(Z), xyz_lP(Z), xyz_Ar(Z) &
       ], [3, 3])
    integer, parameter :: i_PHANTOM(2) = [xyz(U_PT, X)*xyz(U_PT, Y)+1, xyz(L_PT, X)*xyz(L_PT, Y)+1]
    integer, parameter :: i_U_PT_FIXED = xyz(U_PT, X)*xyz(U_PT, Y)*(xyz(U_PT, Z)-1)+1
    integer, parameter :: N_LAYER(2) = [xyz(U_PT, X)*xyz(U_PT, Y), xyz(L_PT, X)*xyz(L_PT, Y)]

    integer, parameter :: N_uP = xyz_uP(1) * xyz_uP(2) * xyz_uP(3) ! 上部Pt 分子数
    integer, parameter :: N_lP = xyz_lP(1) * xyz_lP(2) * xyz_lP(3) ! 下部Pt 分子数
    integer, parameter :: N_Ar = xyz_Ar(1) * xyz_Ar(2) * xyz_Ar(3) ! Ar分子数
    integer, parameter :: N_Max = max(N_uP, N_lP, N_Ar)
    integer, parameter :: N_All = N_uP + N_lP + N_Ar ! すべての分子数
    integer, parameter :: N(4) = [N_uP, N_lP, N_Ar, N_All]

    double precision, parameter :: CUTOFFperSIG = 3.000D0 ! カットオフ長さ/σ
    double precision, parameter :: DT = 1.00D0 ! 無次元時間ステップ(無次元)
    integer, parameter :: MAXSTEP = 20000 ! 最大ステップ数
    integer, parameter :: NVTSTEP = 5000 ! 温度補正を止めるステップ

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
    double precision, parameter :: SIG(3,3) = reshape([ &
    SIG_PtPt, SIG_PtPt, SIG_PtAr, &
    SIG_PtPt, SIG_PtPt, SIG_PtAr, &
    SIG_PtAr, SIG_PtAr, SIG_ArAr ], shape(SIG))
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
    double precision, parameter :: SSIZE(3) = [xyz(U_PT, X)*(STDIST_Pt/2), xyz(U_PT, Y)*STDIST_Pt, 64.0D0]
    double precision, parameter :: ofst_U_Pt_X = (SSIZE(X) - (STDIST_Pt/2) * (xyz(U_PT, X) - 1)) / 2.0D0
    double precision, parameter :: ofst_U_Pt_Y = (SSIZE(Y) - STDIST_Pt * (xyz(U_PT, Y) - 0.5D0)) / 2.0D0
    double precision, parameter :: ofst_U_Pt_Z = SSIZE(3) - STDIST(U_PT)
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
    double precision, parameter :: DAMPER(2) = [double precision :: 50.0D0, 50.0D0]
    double precision, parameter :: ATEMP(2) = [double precision :: 400.0D0, 50.0D0] ! 目標温度[K]
    double precision, parameter :: DAMPCOEF(2) = [-MASS(U_PT)/DAMPER(U_PT), -MASS(L_PT)/DAMPER(L_PT)]
    double precision, parameter :: RANDCOEF(2) = [dsqrt(2.0D0 * BOLTZMANN * 1.0D16 * ATEMP(U_PT) * MASS(U_PT) / DT / DAMPER(U_PT)),&
    dsqrt(2.0D0 * BOLTZMANN * 1.0D16 * ATEMP(L_PT) * MASS(L_PT) / DT / DAMPER(L_PT))]

    ! 熱流束関連
    integer, parameter :: PARTITION = 20 
    double precision, parameter :: LAYER = (ofst_U_Pt_Z - STDIST_Pt) / PARTITION

    ! record用
    integer, parameter :: DAT_RANDOM1 = 1
    integer, parameter :: DAT_RANDOM0 = 2
    integer, parameter :: DAT_POSIT = 3
    integer, parameter :: DAT_VELOCITY = 4
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

end module parameters