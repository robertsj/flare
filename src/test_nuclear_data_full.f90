program test_nuclear_data_full

use nuclear_data_full

integer :: BP, i
real(8) :: E, HT_F, HT_C, HB, T_F, T_C, B, kinf, m2, nu, kappa


BP = 0
E  = 0.0_8
HT_F = 900_8
HT_C = 580_8
HB = 900_8
T_F = 900_8
T_C = 580_8
B = 0_8

do i = 1, 50*1000000
  kinf = get_kinf(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  m2 = get_m2(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  nu = get_nu(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  kappa = get_kappa(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  
!  print '(5f10.4)', kinf, m2, nu, kappa, T_F
!  
!  E = E + 0.025
  
end do

end program test_nuclear_data_full