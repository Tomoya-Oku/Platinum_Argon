program pvch
      use parameters
      implicit none

      double precision, dimension(3) :: pom
      double precision, dimension(3) :: pon
      double precision, dimension(3) :: vl2
      integer :: num1, i, j
      !dimension  mcolor(nkoss)
      integer :: moltype = 1
      integer :: nmol = u_Pt_N + l_Pt_N + Ar_N
      ! manual change
      integer :: ndat= 200
      integer :: ntime0 = 0
      integer :: ndt = 1
      integer :: yellow_green = 5 ! 黄緑
      integer :: red = 14 ! 赤
      integer :: green = 2 ! 緑
      integer :: orange = 10 ! オレンジ
      integer :: blue = 0 ! 青

      open(1,file='posit.dat')
      open(2,file='pos.dat')
      open(3,file='mask.dat')
      open(4,file='period_length.dat')

      do i=1,3
            read(4,*) vl2(i)
      end do
      
      write(2,'(3I7)') moltype,nmol,ndat
      write(2,'(3F15.5)') vl2(1),vl2(2),vl2(3)
      write(2,'(2I7)') ntime0, ndt

      do i = 1, ndat
            do j = 1, nmol
                  read(1,'(I6,3D15.7)')num1, pon(1), pon(2), pon(3)
                  pom(:) = pon(:)
                  write(2,'(3E15.7)') pom(1), pom(2), pom(3)
            end do
      end do

      do i = 1, ndat
            do j = 1, u_Pt_N+l_Pt_N
                  write(3,'(I7)') blue
            end do
            do j = u_Pt_N+l_Pt_N+1, u_Pt_N+l_Pt_N+Ar_N
                  write(3,'(I7)') red
            end do
      end do
end program pvch