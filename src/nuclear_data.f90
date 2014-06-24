!==============================================================================!
! MODULE: nuclear_data
!
!> @author Jeremy Roberts, Nick Horelik, and Nathan Gibson
!> @brief  Group constant container and functions
!
! This module was adapted from the original C code generated by 
! N. Horelik as part of his and N. Gibson's project for a class
! at MIT (poropy itself was born out of J. Roberts' project for the
! same class).
!
! The data consists of standard, two-group data typically produced
! by the lattice physics code CASMO.  Data was
! generated for a standard 17x17 PWR with varying enrichment, 
! burnup, and a few different burnable poison (IFBA, WABA, and GAD).
! The data was fit to functional forms, the results of which are included
! here as a crude but instructive and very fast set of data.
!==============================================================================!
module nuclear_data

use nuclear_data_NOBP
use nuclear_data_IFBA
use nuclear_data_WABA
use nuclear_data_GAD

!> No burnable poison
integer, parameter :: NOBP = 0
!> Integrated Fuel Burnable Absorber (IFBA)
integer, parameter :: IFBA = 1
!> Wet Annular Burnable Absorber (WABA)
integer, parameter :: WABA = 2
!> Gadolinia (GAD)
integer, parameter :: GAD = 3
 
! Burnup cutoffs for parts of each fit
double precision, parameter :: XE_CUTOFF   = 0.1
double precision, parameter :: IFBA_CUTOFF = 30
double precision, parameter :: GAD_CUTOFF  = 15.0
double precision, parameter :: WABA_CUTOFF = 22.5
 
contains

!------------------------------------------------------------------------------!
!> Set the minimal coefficient set based on burnup, enrichment, and poison
subroutine set_flare_data(B, E, BP, KINF, M2, KAPPA)
  !> Burnup
  double precision, intent(in) :: B
  !> Enrichment
  double precision, intent(in) :: E
  !> Burnable poison option
  integer, intent(in)          :: BP
  !> Infinite multiplication factor
  double precision, intent(out) :: KINF
  !> Migration area
  double precision, intent(out) :: M2
  !> Energy release per fission
  double precision, intent(out) :: KAPPA

  ! Set the data for no burnable poison, as all other values are
  ! defined as corrections to this data
  call set_flare_data_nobp(B, E, KINF, M2, KAPPA)

  ! Correct the data for the burnable poison, if any
  if (BP == IFBA) then
    call set_flare_data_ifba(B, E, KINF, M2, KAPPA)
  elseif (BP == WABA) then
    call set_flare_data_waba(B, E, KINF, M2, KAPPA)
  elseif (BP == GAD) then
    call set_flare_data_gad(B, E, KINF, M2, KAPPA)
  end if

end subroutine set_flare_data

!------------------------------------------------------------------------------!
subroutine set_flare_data_nobp(B, E, KINF, M2, KAPPA)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: KINF, M2, KAPPA
  ! Cutoff breaks the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    KINF = get_K_INF_XE_0(B, E)
    M2 = get_M2_XE_0(B, E)
    KAPPA = get_KAPPA_0(B, E)
  else
    KINF = get_K_INF_XE_1(B, E)
    M2 = get_M2_XE_1(B, E)
    KAPPA = get_KAPPA_1(B, E)
  end if
end subroutine set_flare_data_nobp

!------------------------------------------------------------------------------!
subroutine set_flare_data_ifba(B, E, KINF, M2, KAPPA)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: KINF, M2, KAPPA
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    KINF = KINF - get_K_INF_XE_IFBA_DIFF_0(B, E)
    M2 = M2 - get_M2_XE_IFBA_DIFF_0(B, E)
    KAPPA = KAPPA - get_KAPPA_IFBA_DIFF_0(B, E)
  elseif (B < IFBA_CUTOFF) then
    KINF = KINF - get_K_INF_XE_IFBA_DIFF_1(B, E)
    M2 = M2 - get_M2_XE_IFBA_DIFF_1(B, E)
    KAPPA = KAPPA - get_KAPPA_IFBA_DIFF_1(B, E)
  else
    KINF = KINF - get_K_INF_XE_IFBA_DIFF_2(B, E)
    M2 = M2 - get_M2_XE_IFBA_DIFF_2(B, E)
    KAPPA = KAPPA - get_KAPPA_IFBA_DIFF_2(B, E)
  end if
end subroutine set_flare_data_ifba

!------------------------------------------------------------------------------!
subroutine set_flare_data_waba(B, E, KINF, M2, KAPPA)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: KINF, M2, KAPPA
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    KINF = KINF - get_K_INF_XE_WABA_DIFF_0(B, E)
    M2 = M2 - get_M2_XE_WABA_DIFF_0(B, E)
    KAPPA = KAPPA - get_KAPPA_WABA_DIFF_0(B, E)
  elseif (B < WABA_CUTOFF) then
    KINF = KINF - get_K_INF_XE_WABA_DIFF_1(B, E)
    M2 = M2 - get_M2_XE_WABA_DIFF_1(B, E)
    KAPPA = KAPPA - get_KAPPA_WABA_DIFF_1(B, E)
  else
    KINF = KINF - get_K_INF_XE_WABA_DIFF_2(B, E)
    M2 = M2 - get_M2_XE_WABA_DIFF_2(B, E)
    KAPPA = KAPPA - get_KAPPA_WABA_DIFF_2(B, E)
  end if
end subroutine set_flare_data_waba

!------------------------------------------------------------------------------!
subroutine set_flare_data_gad(B, E, KINF, M2, KAPPA)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: KINF, M2, KAPPA
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    KINF = KINF - get_K_INF_XE_GAD_DIFF_0(B, E)
    M2 = M2 - get_M2_XE_GAD_DIFF_0(B, E)
    KAPPA = KAPPA - get_KAPPA_GAD_DIFF_0(B, E)
  elseif (B < GAD_CUTOFF) then
    KINF = KINF - get_K_INF_XE_GAD_DIFF_1(B, E)
    M2 = M2 - get_M2_XE_GAD_DIFF_1(B, E)
    KAPPA = KAPPA - get_KAPPA_GAD_DIFF_1(B, E)
  else
    KINF = KINF - get_K_INF_XE_GAD_DIFF_2(B, E)
    M2 = M2 - get_M2_XE_GAD_DIFF_2(B, E)
    KAPPA = KAPPA - get_KAPPA_GAD_DIFF_2(B, E)
  end if
end subroutine set_flare_data_gad

!------------------------------------------------------------------------------!
!> Set the two-group data based on burnup, enrichment, and burnable poison
subroutine set_two_group_data(B, E, BP, D1, D2, A1, A2, F1, F2, S12, NU)
  double precision, intent(in) :: B, E
  integer, intent(in) :: BP
  double precision, intent(out) :: D1, D2, A1, A2, F1, F2, S12, NU

  ! Set the data for no burnable poison, as all other values are 
  ! defined as corrections to this data
  call set_two_group_data_nobp(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  
  ! Correct the data for the burnable poison, if any
  if (BP == IFBA) then
    call set_two_group_data_ifba(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  elseif (BP == WABA) then
    call set_two_group_data_waba(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  elseif (BP == GAD) then
    call set_two_group_data_gad(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  end if
  
end subroutine set_two_group_data

!------------------------------------------------------------------------------!
subroutine set_two_group_data_nobp(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: D1, D2, A1, A2, F1, F2, S12, NU
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    D1 = get_DIFF1_0(B, E)
    D2 = get_DIFF2_0(B, E)
    A1 = get_ABS1_0(B, E)
    A2 = get_ABS2_0(B, E)
    F1 = get_NUFISS1_0(B, E)
    F2 = get_NUFISS2_0(B, E)
    NU = get_NU_0(B, E)
    S12 = get_REMOV1_0(B, E) ! casmo definition
  else
    D1 = get_DIFF1_1(B, E) ! D1
    D2 = get_DIFF2_1(B, E) ! D2
    A1 = get_ABS1_1(B, E) ! A1
    A2 = get_ABS2_1(B, E) ! A2
    F1 = get_NUFISS1_1(B, E) ! F1
    F2 = get_NUFISS2_1(B, E) ! F2
    NU = get_NU_1(B, E)
    S12 = get_REMOV1_1(B, E) ! casmo definition
  end if
end subroutine set_two_group_data_nobp

!------------------------------------------------------------------------------!
subroutine set_two_group_data_ifba(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: D1, D2, A1, A2, F1, F2, S12, NU
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    D1 = D1 - get_DIFF1_IFBA_DIFF_0(B, E)
    D2 = D2 - get_DIFF2_IFBA_DIFF_0(B, E)
    A1 = A1 - get_ABS1_IFBA_DIFF_0(B, E)
    A2 = A2 - get_ABS2_IFBA_DIFF_0(B, E)
    F1 = F1 - get_NUFISS1_IFBA_DIFF_0(B, E)
    F2 = F2 - get_NUFISS2_IFBA_DIFF_0(B, E)
    NU = NU - get_NU_IFBA_DIFF_0(B, E)
    S12 = S12 - get_REMOV1_IFBA_DIFF_0(B, E)
  elseif (B < IFBA_CUTOFF) then
    D1 = D1 - get_DIFF1_IFBA_DIFF_1(B, E)
    D2 = D2 - get_DIFF2_IFBA_DIFF_1(B, E)
    A1 = A1 - get_ABS1_IFBA_DIFF_1(B, E)
    A2 = A2 - get_ABS2_IFBA_DIFF_1(B, E)
    F1 = F1 - get_NUFISS1_IFBA_DIFF_1(B, E)
    F2 = F2 - get_NUFISS2_IFBA_DIFF_1(B, E)
    NU = NU - get_NU_IFBA_DIFF_1(B, E)
    S12 = S12 - get_REMOV1_IFBA_DIFF_1(B, E)
  else
    D1 = D1 - get_DIFF1_IFBA_DIFF_2(B, E)
    D2 = D2 - get_DIFF2_IFBA_DIFF_2(B, E)
    A1 = A1 - get_ABS1_IFBA_DIFF_2(B, E)
    A2 = A2 - get_ABS2_IFBA_DIFF_2(B, E)
    F1 = F1 - get_NUFISS1_IFBA_DIFF_2(B, E)
    F2 = F2 - get_NUFISS2_IFBA_DIFF_2(B, E)
    NU = NU - get_NU_IFBA_DIFF_2(B, E)
    S12 = S12 - get_REMOV1_IFBA_DIFF_2(B, E)
  end if
end subroutine set_two_group_data_ifba

!------------------------------------------------------------------------------!
subroutine set_two_group_data_waba(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: D1, D2, A1, A2, F1, F2, S12, NU
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    D1 = D1 - get_DIFF1_WABA_DIFF_0(B, E)
    D2 = D2 - get_DIFF2_WABA_DIFF_0(B, E)
    A1 = A1 - get_ABS1_WABA_DIFF_0(B, E)
    A2 = A2 - get_ABS2_WABA_DIFF_0(B, E)
    F1 = F1 - get_NUFISS1_WABA_DIFF_0(B, E)
    F2 = F2 - get_NUFISS2_WABA_DIFF_0(B, E)
    NU = NU - get_NU_WABA_DIFF_0(B, E)
    S12 = S12 - get_REMOV1_WABA_DIFF_0(B, E)
  elseif (B < WABA_CUTOFF) then
    D1 = D1 - get_DIFF1_WABA_DIFF_1(B, E)
    D2 = D2 - get_DIFF2_WABA_DIFF_1(B, E)
    A1 = A1 - get_ABS1_WABA_DIFF_1(B, E)
    A2 = A2 - get_ABS2_WABA_DIFF_1(B, E)
    F1 = F1 - get_NUFISS1_WABA_DIFF_1(B, E)
    F2 = F2 - get_NUFISS2_WABA_DIFF_1(B, E)
    NU = NU - get_NU_WABA_DIFF_1(B, E)
    S12 = S12 - get_REMOV1_WABA_DIFF_1(B, E)
  else
    D1 = D1 - get_DIFF1_WABA_DIFF_2(B, E)
    D2 = D2 - get_DIFF2_WABA_DIFF_2(B, E)
    A1 = A1 - get_ABS1_WABA_DIFF_2(B, E)
    A2 = A2 - get_ABS2_WABA_DIFF_2(B, E)
    F1 = F1 - get_NUFISS1_WABA_DIFF_2(B, E)
    F2 = F2 - get_NUFISS2_WABA_DIFF_2(B, E)
    NU = NU - get_NU_WABA_DIFF_2(B, E)
    S12 = S12 - get_REMOV1_WABA_DIFF_2(B, E)
  end if
end subroutine set_two_group_data_waba

!------------------------------------------------------------------------------!
subroutine set_two_group_data_gad(B, E, D1, D2, A1, A2, F1, F2, S12, NU)
  double precision, intent(in) :: B, E
  double precision, intent(out) :: D1, D2, A1, A2, F1, F2, S12, NU
  ! Cutoffs break the data into more easily-represented segments
  if (B < XE_CUTOFF) then
    D1 = D1 - get_DIFF1_GAD_DIFF_0(B, E)
    D2 = D2 - get_DIFF2_GAD_DIFF_0(B, E)
    A1 = A1 - get_ABS1_GAD_DIFF_0(B, E)
    A2 = A2 - get_ABS2_GAD_DIFF_0(B, E)
    F1 = F1 - get_NUFISS1_GAD_DIFF_0(B, E)
    F2 = F2 - get_NUFISS2_GAD_DIFF_0(B, E)
    NU = NU - get_NU_GAD_DIFF_0(B, E)
    S12 = S12 - get_REMOV1_GAD_DIFF_0(B, E)
  elseif (B < GAD_CUTOFF) then
    D1 = D1 - get_DIFF1_GAD_DIFF_1(B, E)
    D2 = D2 - get_DIFF2_GAD_DIFF_1(B, E)
    A1 = A1 - get_ABS1_GAD_DIFF_1(B, E)
    A2 = A2 - get_ABS2_GAD_DIFF_1(B, E)
    F1 = F1 - get_NUFISS1_GAD_DIFF_1(B, E)
    F2 = F2 - get_NUFISS2_GAD_DIFF_1(B, E)
    NU = NU - get_NU_GAD_DIFF_1(B, E)
    S12 = S12 - get_REMOV1_GAD_DIFF_1(B, E)
  else
    D1 = D1 - get_DIFF1_GAD_DIFF_2(B, E)
    D2 = D2 - get_DIFF2_GAD_DIFF_2(B, E)
    A1 = A1 - get_ABS1_GAD_DIFF_2(B, E)
    A2 = A2 - get_ABS2_GAD_DIFF_2(B, E)
    F1 = F1 - get_NUFISS1_GAD_DIFF_2(B, E)
    F2 = F2 - get_NUFISS2_GAD_DIFF_2(B, E)
    NU = NU - get_NU_GAD_DIFF_2(B, E)
    S12 = S12 - get_REMOV1_GAD_DIFF_2(B, E)
  end if
end subroutine set_two_group_data_gad

end module nuclear_data