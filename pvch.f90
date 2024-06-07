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
      integer :: ndat= 200
      integer :: ntime0 = 0
      integer :: ndt = 1
      integer :: yellow_green = 5 ! 黄緑
      integer :: red = 14 ! 赤
      integer :: green = 2 ! 緑
      integer :: orange = 10 ! オレンジ
      integer :: blue = 0 ! 青

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
                  read(DAT_POSIT,'(I6,3D15.7)')num1, pon(1), pon(2), pon(3)
                  pom(:) = pon(:)
                  write(DAT_POS,'(3E15.7)') pom(1), pom(2), pom(3)
            end do
      end do

      do i = 1, ndat
            do j = 1, N(U_PT)+N(L_PT)
                  write(DAT_MASK,'(I7)') blue
            end do
            do j = N(U_PT)+N(L_PT)+1, N(ALL)
                  write(DAT_MASK,'(I7)') red
            end do
      end do
end program pvch