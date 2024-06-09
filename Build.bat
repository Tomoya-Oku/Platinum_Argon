gfortran -frecursive parameters_variables.f90 calc_bound.f90 calc_potential.f90 calc_kinetic.f90 calc_LeapFlog.f90 correct.f90 initialize.f90 record.f90 main.f90 -o main
main.exe
gfortran parameters_variables.f90 pvch.f90 -o pvch
pvch.exe
pause