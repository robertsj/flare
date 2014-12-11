!==============================================================================!
! NUCLEAR_DATA_FULL provides FLARE parameters for two typical assembly 
! configurations with full dependence on historical and instaneous temperatures
! and boron concentration.
!
! Version 0.1
! 
! Copyright (C) 2014 Jeremy Roberts
!==============================================================================!
module nuclear_data_full

implicit none 
  
contains 

!==============================================================================!
subroutine set_flare_data_2(BP, E, HT_F, HT_C, HBC, KINF, M2, NU, KAPPA)
  integer, intent(in) :: BP
  real(8), intent(in) :: E, HT_F, HT_C, HBC

end subroutine

!==============================================================================!
real(8) function get_kinf(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  integer, intent(in) :: bp
  real(8), intent(in) :: E, HT_F, HT_C, HB, T_F, T_C, B
  if (BP == 0) then
    get_kinf = 1.23482271817687 +                                              &
               0.384707449122803*max(E, 0.114011101397224_8) +                 &
               3.87601719379681e-5*E**2 - 0.393822001785085*E -                &
               0.0128454171802163*atan2(0.046196282542364*E*max(E,             &
               3.41395933888842_8), 2.97115376738628_8)
    get_kinf = get_kinf *                                                      &
               (1.1491430314134 + 5.52095469220095e-6*HB +                     &
                1.3800173425612e-7*T_C*B + 8.05459339830642e-9*B**2 -          &
                3.13017533377653e-5*T_F - 0.000169713214003935*B -             & 
                1.56322963647509e-7*T_C**2)
  else
    stop "not implemented!"
  end if
end function get_kinf

!==============================================================================!
real(8) function get_m2(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  integer, intent(in) :: bp
  real(8), intent(in) :: E, HT_F, HT_C, HB, T_F, T_C, B
  if (BP == 0) then
    get_m2 =  49.6898548599663 +                                               &
              7.39972776451824*atan2(49.6898548599663_8 + E,                   &
              E - 2.74137055864184*atan2(0.0285229006551038_8, E))
    get_m2 = get_m2 *                                                          &
             (37.8295588405672 + 1.22521574701153/T_F +                        &
              6.27330275666574e-5*T_C**2 + (8.32091265211943e-7*B**2 -         &
              5235.86863330276)/T_C - 2.42355249844968e-5*B -                  &
              0.0842865206608579*T_C)
  else
    stop "not implemented!"
  end if
end function get_m2

!==============================================================================!
real(8) function get_nu(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  integer, intent(in) :: bp
  real(8), intent(in) :: E, HT_F, HT_C, HB, T_F, T_C, B
  if (BP == 0) then
    get_nu = 2.54293053 + 0.00593253818142517*E - 2.29827138932442e-5*E**2 -   &
             0.107347981647*atan2(6.26610382292088_8, 6.42590456783652_8 + E) 
    get_nu = get_nu *                                                          &
                (0.98735844518091 + 1.07735148409501e-6*HB +                   &
                 6.13665526714897e-7*T_F + 5.82197178575519e-7*B +             &
                 2.19450069857174e-7*E*T_C + 5.45769701137517e-11*T_C*HT_C**2 -& 
                 0.000128828908719483*E)
  else
    stop "not implemented!"
  end if
end function get_nu

!==============================================================================!
real(8) function get_kappa(BP, E, HT_F, HT_C, HB, T_F, T_C, B)
  integer, intent(in) :: bp
  real(8), intent(in) :: E, HT_F, HT_C, HB, T_F, T_C, B
  if (BP == 0) then
    get_kappa = 3.21970171202147e-11 +                                         &
                1.29534831050797e-13*(3.08185747585896+ E)**0.615524748603661 -&
                4.16556565620788e-15*max(E, 0.4253925244888+0.893035316466691*E) 
    get_kappa = get_kappa *                                                    &
                (0.998560388047544 + 1.93688310862253e-6*T_C +                 &
                3.46062303184635e-7*HB + 4.35926253644907e-7*E*T_C +           &
                4.56945094553881e-6*E**2 - 0.000252530590442415*E -            &
                7.9095328020129e-9*T_C*E**2)
  else
    stop "not implemented!"
  end if
end function get_kappa

end module nuclear_data_full
