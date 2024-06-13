gfortran -frecursive parameters.f90 variables.f90 boundary.f90 calc_potential.f90 calc_kinetic.f90 calc_energy.f90 integral.f90 vel_scaling.f90 initialize.f90 record.f90 main.f90 -o main
main.exe
gfortran parameters.f90 variables.f90 pvch.f90 -o pvch
pvch.exe
pause