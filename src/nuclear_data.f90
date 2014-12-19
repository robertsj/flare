!==============================================================================!
! MODULE: nuclear_data
!
!> @author Jeremy Roberts
!> @brief  Simple nodal parameter model
!
! This module replaces the original data model developed by Nick Horelik
! and Nathan Gibson for the original release of Poropy.  That original
! model included dependence on burnup and enrichment for several burnable
! absorber options.  The model was reconstructed to include dependence on
! the historical and instantaneous temperatures and boron concentration.
!
! TODO:
!   (1) Add fits for the two-group diffusion parameters
!==============================================================================!
module nuclear_data

use nuclear_data_NOBP
use nuclear_data_IFBA
use nuclear_data_GAD

implicit none

!> No burnable poison
integer, parameter :: NOBP = 0
!> Integral Fuel Burnable Absorber (IFBA)
integer, parameter :: IFBA = 1
!> Gadolinia (GAD)
integer, parameter :: GAD = 2
 
contains

!==============================================================================!
!> @brief Set the flare parameters (kinf, M^2, kappa, and nu)
!==============================================================================!
subroutine set_flare_data(B, E, BP, HT_F, HT_C, HBC, T_F, T_C, BC,             &
                          KINF, M2, NU, KAPPA)
  !> Burnup
  real(8), intent(in) :: B
  !> Enrichment
  real(8), intent(in) :: E
  !> Burnable poison option
  integer, intent(in) :: BP
  !> Historical fuel temperature
  real(8), intent(in) :: HT_F
  !> Historical coolant temperature
  real(8), intent(in) :: HT_C
  !> Historical boron concentration
  real(8), intent(in)  :: HBC
  !> Instantaneous fuel temperature
  real(8), intent(in) :: T_F
  !> Instantaneous coolant temperature
  real(8), intent(in) :: T_C
  !> Instantaneous boron concentration
  real(8), intent(in)  :: BC

  !> Infinite multiplication factor
  real(8), intent(out) :: KINF
  !> Migration area
  real(8), intent(out) :: M2
  !> Average number of neutrons released per fission
  real(8), intent(out) :: NU
  !> Energy release per fission
  real(8), intent(out) :: KAPPA

  if (B > 50.0) then
    print *, "WARNING: Model is valid only for burnups less than 50 GWd/MTU"
  end if

  select case(BP)

    case (NOBP)
      call set_flare_data_nobp(B, E, HT_F, HT_C, HBC, T_F, T_C, BC,            &
                               KINF, M2, NU, KAPPA)
    case (IFBA)
      call set_flare_data_ifba(B, E, HT_F, HT_C, HBC, T_F, T_C, BC,            &
                               KINF, M2, NU, KAPPA)
    case (GAD)
      call set_flare_data_gad(B, E, HT_F, HT_C, HBC, T_F, T_C, BC,             &
                              KINF, M2, NU, KAPPA)
  end select

end subroutine set_flare_data

!==============================================================================!
!> @brief Set the flare parameters for no burnable poison case
!==============================================================================!
subroutine set_flare_data_nobp(B, E, HT_F, HT_C, HBC, T_F, T_C, BC,            &
                               KINF, M2, NU, KAPPA)
  real(8), intent(in) :: B, E, HT_F, HT_C, HBC, T_F, T_C, BC
  real(8), intent(out) :: KINF, M2, NU, KAPPA
  real(8) :: KINF_C, M2_C, NU_C, KAPPA_C

  ! define baseline values evaluated only as a function of burnup
  ! for T_F = 900 K, T_C = 580 K, and BC = 900 ppm.

  call set_flare_data_nobp_baseline(B, E, KINF, M2, NU, KAPPA)

  ! if the user does not request thermal-hydraulic feedback or a critical
  ! boron search, then only the baseline data is necessary.
  if (.false.) then
    return
  end if

  ! otherwise, correct values for given temperatures and boron concentration
  KINF_C = 1.2158282534503 + (0.792989071763265 -                              &
         2.60694761682009*atan2(64.5215877668043 + 0.792989071763265*B +       &
         0.144880219861946*BC - 0.000726508053601699*B*HBC, T_C))/             &
         (3.62335796338692 + E) - 3.10628898998413e-5*T_F -                    &
         0.00030859520275798*T_C
  KINF = KINF * KINF_C

  M2_C = 0.672199344961825 + -39.1094846842861/T_C +                           &
       45.9569649183781/(689.760920670368 - T_C) -                             &
       4.43259947719036e-6*T_F - 2.13611427723105e-5*BC
  M2 = M2 * M2_C

  NU_C = 0.966450688548413 + 3.63230280986546e-5*HT_C +                        &
       2.11656764904971e-5*T_C + 5.9866470356798e-7*BC +                       &
       2.19854077885927e-6*B**2 +                                              &
       2.4527081165953e-9*HBC*T_C*tanh(0.966450688548413 + B -                 &
       0.00469036290204834*T_C) - 0.000129588468541154*B
  NU = NU * NU_C

  KAPPA_C = 0.991644045347475 + 0.000105405582649882*B +                       &
          3.54735589581823e-6*T_C + 3.76594581660714e-7*HBC +                  &
          0.00358492032232963*atan2(1.15555740181132*T_C, B**2)
  KAPPA = KAPPA * KAPPA_C

end subroutine set_flare_data_nobp

!==============================================================================!
!> @brief Set the flare parameters for the baseline no burnable poison case
!==============================================================================!
subroutine set_flate_data_nobp_baseline(B, E, KINF, M2, NU, KAPPA)
  real(8), intent(in) :: B, E
  real(8), intent(out) :: KINF, M2, NU, KAPPA
  KINF = 0.0434999504004638*E + -0.255499755632519/E +                         &
         (113.190720303571 + 2.69266702457571*atan2(0.00194341029561304*E, B))/&
         (100.767219963979 + B)

  M2 = 61.176816059031 +                                                       &
       1.26126224123277*atan2(2.87806866905541 - sqrt(B), E) -                 &
       0.325493942351276*sqrt(0.0265096995706338 + B +                         &
                              atan2(B, B**2*np.sin(3.55325483234782 + E) - B))

  NU = 2.46316590714585 + 1.26388784726096e-5*B**2 +                           &
       0.0339740091175806*B/(E + 0.065393429639637*B - 1.04078502997543)

  KAPPA = 3.4255544679858e-11 - &
          1.13957455426992e-12*atan2(12.1608782794473*E -                      &
                                     1.67409202354355, B + sqrt(B))
end subroutine set_flate_data_nobp_baseline

!==============================================================================!
!> @brief Set the flare parameters for IFBA case
!==============================================================================!
subroutine set_flare_data_ifba(B, E, HT_F, HT_C, HBC, T_F, T_C, BC,            &
                               KINF, M2, NU, KAPPA)
  real(8), intent(in) :: B, E, HT_F, HT_C, HBC, T_F, T_C, BC
  real(8), intent(out) :: KINF, M2, NU, KAPPA
  real(8) :: KINF_B, M2_B, NU_B, KAPPA_B

  ! define baseline values evaluated only as a function of burnup
  ! for T_F = 900 K, T_C = 580 K, and BC = 900 ppm.

  call set_flare_data_ifba_baseline(B, E, KINF, M2_, NU, KAPPA)

  ! correct the values for the given temperatures and boron concentration

  KINF_C = 1.2158282534503 + (0.792989071763265 -                                &
         2.60694761682009*atan2(64.5215877668043 + 0.792989071763265*B +       &
         0.144880219861946*BC - 0.000726508053601699*B*HBC, T_C))/             &
         (3.62335796338692 + e) - 3.10628898998413e-5*T_F -                    &
         0.00030859520275798*T_C
  KINF = KINF * KINF_C

  M2_C = 0.500485601854407 + -75.711825087858/(T_C - 718.329988442194) + &
         atan2(-0.593493277960743, 5.5129613184136 + B +                       &
               377.442452283045*e/T_C) -                                       &
         1.98198881395194e-5*BC - 0.00032594298934755*B -                      &
         8.43692038629177e-7*e*T_F
  M2 = M2 * M2_C

  NU_C = 0.990932523857498 + 0.000162430238704035*B +                          &
         1.96216949893686e-5*T_C -                                             &
         0.0056109398354973*atan2(B, 0.044472307369302*T_C +                   &
                                  0.0105130690629856*HBC - B)
  NU = NU * NU_C

  KAPPA = 0.991644045347475 + 0.000105405582649882*B +                         &
          3.54735589581823e-6*T_C + 3.76594581660714e-7*HBC +                  &
          0.00358492032232963*atan2(1.15555740181132*T_C, B**2)
  KAPPA = KAPPA * KAPPA_B

end subroutine set_flare_data_ifba


!==============================================================================!
!> @brief Set the flare parameters for the baseline IFBA case
!==============================================================================!
subroutine set_flate_data_ifba_baseline(B, E, KINF, M2, NU, KAPPA)
  real(8), intent(in) :: B, E
  real(8), intent(out) :: KINF, M2, NU, KAPPA
  real(8) :: KINF_B, M2_B, NU_B, KAPPA_B

  call set_flare_data_nobp_baseline(B, E, KINF_B, M2_B, NU_B, KAPPA_B)

  KINF = 0.118757285967065 + (0.00438365176843444*B - 0.238643947487901)/E -   &
         0.00190945626833318*B -                                               &
         0.153141606949874*atan2(E, 0.658781263762451*B +                      &
                                 0.0008078613884391/(B - 0.00290530875308968))
  KINF = KINF + KINF_B

  M2 =  1.12290400201433*atan2(1.7813524289371*atan2(1.78214644061973, B) - e, &
        0.376670735493203*B*1.05725417553495**(E + B) - 1.75157067875079)
  M2 = M2 + M2_B

  NU = 0.00658731332549589*atan2(B, e)*                                        &
       atan2(161.246400011515 - 26.9192862760448*B/e, B**2) -                  &
       5.54592642960384e-5*B*cos(33.8773598816324*e)
  NU = NU + NU_B

  KAPPA = 3.4255544679858e-11 - &
          1.13957455426992e-12*atan2(12.1608782794473*E -                      &
                                     1.67409202354355, B + sqrt(B))
end subroutine set_flate_data_ifba_baseline

end module nuclear_data
