program pvch
      use parameters
      implicit none

      double precision, dimension(3) :: pom
      double precision, dimension(3) :: pon
      double precision, dimension(3) :: vl2
      integer :: num1, i, j
      !dimension  mcolor(nkoss)
      integer :: moltype = 1
      ! manual change
      integer :: ndat= MAXSTEP / 100
      integer :: ntime0 = 0
      integer :: ndt = 1
      integer :: yellow_green = 5 ! 黄緑
      integer :: red = 14 ! 赤
      integer :: green = 2 ! 緑
      integer :: orange = 10 ! オレンジ
      integer :: blue = 0 ! 青
      integer :: white = 15 

      open(DAT_POSIT,file='posit.dat')
      open(DAT_POS,file='pos.dat')
      open(DAT_MASK,file='mask.dat')
      open(DAT_PERIODIC,file='periodic.dat')

      do i=1,3
            read(DAT_PERIODIC,*) vl2(i)
      end do
      
      write(DAT_POS,'(3I7)') moltype,N(ALL),ndat
      write(DAT_POS,'(3F15.5)') vl2(1),vl2(2),vl2(3)
      write(DAT_POS,'(2I7)') ntime0, ndt

      do i = 1, ndat
            do j = 1, N(ALL)
                  read(DAT_POSIT,'(I6,3E15.7)')num1, pon(1), pon(2), pon(3)
                  pom(:) = pon(:)
                  write(DAT_POS,'(3E15.7)') pom(1), pom(2), pom(3)
            end do
      end do

      do i = 1, ndat
            ! 上部Pt 界面
            do j = 1, xyz(U_PT, X)*xyz(U_PT, Y)
                  write(DAT_MASK,'(I7)') white
            end do
            ! 上部Pt 温度制御層
            do j = xyz(U_PT, X)*xyz(U_PT, Y)+1, xyz(U_PT, X)*xyz(U_PT, Y)*(xyz(U_PT, Z)-1)
                  write(DAT_MASK,'(I7)') blue
            end do
            ! 上部Pt 固定層
            do j = xyz(U_PT, X)*xyz(U_PT, Y)*(xyz(U_PT, Z)-1)+1, N(U_PT)
                  write(DAT_MASK,'(I7)') white
            end do

            ! 下部Pt 固定層
            do j = N(U_PT)+1, N(U_PT)+xyz(L_PT, X)*xyz(L_PT, Y)
                  write(DAT_MASK,'(I7)') white
            end do
            ! 下部Pt 温度制御層
            do j = N(U_PT)+xyz(L_PT, X)*xyz(L_PT, Y)+1, N(U_PT)+xyz(L_PT, X)*xyz(L_PT, Y)*(xyz(L_PT, Z)-1)
                  write(DAT_MASK,'(I7)') blue
            end do
            ! 下部Pt 界面
            do j = N(U_PT)+xyz(L_PT, X)*xyz(L_PT, Y)*(xyz(L_PT, Z)-1)+1, N(U_PT)+N(L_PT)
                  write(DAT_MASK,'(I7)') white
            end do

            ! Ar
            do j = N(U_PT)+N(L_PT)+1, N(ALL)
                  write(DAT_MASK,'(I7)') red
            end do
      end do
end program pvch