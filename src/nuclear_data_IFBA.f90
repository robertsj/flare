module nuclear_data_IFBA

contains

double precision function get_SM2_NO_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA24 = (/ & 
    -4.49810502987e-07, &
     8.83425439828e-06, &
    -6.65638745515e-05, &
     1.98676617087e-04, &
     6.64380326169e-04, &
     1.82102717003e-24, &
    -2.94763711704e-23, &
     1.82496398920e-22, &
    -5.68500841413e-22, &
     1.37340773249e-21  &
    /)
	get_SM2_NO_XE_IFBA_DIFF_0 =  (cIFBA24(0)*E**(4) + cIFBA24(1)*E**(3) + cIFBA24(2)*E**(2) + cIFBA24(3)*E + cIFBA24(4))*B + (cIFBA24(5)*E**(4) + cIFBA24(6)*E**(3) + cIFBA24(7)*E**(2) + cIFBA24(8)*E + cIFBA24(9))

end function get_SM2_NO_XE_IFBA_DIFF_0

double precision function get_SM2_NO_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA25 = (/ & 
    -1.34778251768e-13, &
     2.77607972796e-12, &
    -2.19202174689e-11, &
     7.99168441655e-11, &
    -1.44143512165e-10, &
     5.96274516341e-12, &
    -1.42114885231e-10, &
     1.27409467186e-09, &
    -5.21965032163e-09, &
     1.11034075124e-08, &
     6.24574014974e-11, &
     1.25232807674e-10, &
    -1.19439697980e-08, &
     8.44784784037e-08, &
    -2.80422346920e-07, &
    -4.88315858367e-09, &
     6.96624960589e-08, &
    -3.14295357994e-07, &
     2.85595819966e-07, &
     2.53917988477e-06, &
     4.24575153858e-08, &
    -6.84026132914e-07, &
     3.99847818552e-06, &
    -9.93640221562e-06, &
    -5.50008054251e-06, &
    -5.59663801107e-08, &
     9.02454740208e-07, &
    -5.59694945491e-06, &
     1.87224691905e-05, &
    -1.85305502008e-05  &
    /)
	get_SM2_NO_XE_IFBA_DIFF_1 =  (cIFBA25(0)*E**(4) + cIFBA25(1)*E**(3) + cIFBA25(2)*E**(2) + cIFBA25(3)*E + cIFBA25(4))*B**(5) + (cIFBA25(5)*E**(4) + cIFBA25(6)*E**(3) + cIFBA25(7)*E**(2) + cIFBA25(8)*E + cIFBA25(9))*B**(4) + (cIFBA25(10)*E**(4) + cIFBA25(11)*E**(3) + cIFBA25(12)*E**(2) + cIFBA25(13)*E + cIFBA25(14))*B**(3) + (cIFBA25(15)*E**(4) + cIFBA25(16)*E**(3) + cIFBA25(17)*E**(2) + cIFBA25(18)*E + cIFBA25(19))*B**(2) + (cIFBA25(20)*E**(4) + cIFBA25(21)*E**(3) + cIFBA25(22)*E**(2) + cIFBA25(23)*E + cIFBA25(24))*B + (cIFBA25(25)*E**(4) + cIFBA25(26)*E**(3) + cIFBA25(27)*E**(2) + cIFBA25(28)*E + cIFBA25(29))

end function get_SM2_NO_XE_IFBA_DIFF_1

double precision function get_SM2_NO_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cIFBA26 = (/ & 
     1.14772712182e-11, &
    -1.67778787884e-10, &
     9.16558088330e-10, &
    -2.16810671324e-09, &
     1.85026609649e-09, &
    -1.50080325687e-09, &
     2.14993303433e-08, &
    -1.15846103875e-07, &
     2.71036450315e-07, &
    -2.28588779392e-07, &
     5.57306635911e-08, &
    -7.55545280237e-07, &
     3.88103780442e-06, &
    -8.67620254243e-06, &
     7.14018630431e-06, &
    -5.14653842321e-07, &
     5.66050692927e-06, &
    -2.18639173642e-05, &
     2.84588094111e-05, &
    -1.88165067775e-05  &
    /)
	get_SM2_NO_XE_IFBA_DIFF_2 =  (cIFBA26(0)*E**(4) + cIFBA26(1)*E**(3) + cIFBA26(2)*E**(2) + cIFBA26(3)*E + cIFBA26(4))*B**(3) + (cIFBA26(5)*E**(4) + cIFBA26(6)*E**(3) + cIFBA26(7)*E**(2) + cIFBA26(8)*E + cIFBA26(9))*B**(2) + (cIFBA26(10)*E**(4) + cIFBA26(11)*E**(3) + cIFBA26(12)*E**(2) + cIFBA26(13)*E + cIFBA26(14))*B + (cIFBA26(15)*E**(4) + cIFBA26(16)*E**(3) + cIFBA26(17)*E**(2) + cIFBA26(18)*E + cIFBA26(19))

end function get_SM2_NO_XE_IFBA_DIFF_2

double precision function get_M2_NO_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA27 = (/ & 
     1.82111982949e-02, &
    -2.98663556892e-01, &
     1.88303588778e+00, &
    -5.28758938057e+00, &
     1.52187653147e+00, &
     2.36738608312e-03, &
    -5.10143436609e-02, &
     4.57768774013e-01, &
    -2.27452321960e+00, &
     7.33656640674e+00  &
    /)
	get_M2_NO_XE_IFBA_DIFF_0 =  (cIFBA27(0)*E**(4) + cIFBA27(1)*E**(3) + cIFBA27(2)*E**(2) + cIFBA27(3)*E + cIFBA27(4))*B + (cIFBA27(5)*E**(4) + cIFBA27(6)*E**(3) + cIFBA27(7)*E**(2) + cIFBA27(8)*E + cIFBA27(9))

end function get_M2_NO_XE_IFBA_DIFF_0

double precision function get_M2_NO_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA28 = (/ & 
     1.92626680121e-07, &
    -3.62459748177e-06, &
     2.68099994004e-05, &
    -9.43107951177e-05, &
     1.37051503114e-04, &
    -1.36136708827e-05, &
     2.56755067995e-04, &
    -1.91384991370e-03, &
     6.85476515429e-03, &
    -1.03385288388e-02, &
     3.10061590651e-04, &
    -5.86363371487e-03, &
     4.42146326376e-02, &
    -1.63142183408e-01, &
     2.63217549466e-01, &
    -2.42323127023e-03, &
     4.57982603085e-02, &
    -3.50664738998e-01, &
     1.36452075049e+00, &
    -2.54635258458e+00, &
     3.09920111549e-03, &
    -5.80351943755e-02, &
     4.63011240359e-01, &
    -2.14648747311e+00, &
     6.90083586622e+00  &
    /)
	get_M2_NO_XE_IFBA_DIFF_1 =  (cIFBA28(0)*E**(4) + cIFBA28(1)*E**(3) + cIFBA28(2)*E**(2) + cIFBA28(3)*E + cIFBA28(4))*B**(4) + (cIFBA28(5)*E**(4) + cIFBA28(6)*E**(3) + cIFBA28(7)*E**(2) + cIFBA28(8)*E + cIFBA28(9))*B**(3) + (cIFBA28(10)*E**(4) + cIFBA28(11)*E**(3) + cIFBA28(12)*E**(2) + cIFBA28(13)*E + cIFBA28(14))*B**(2) + (cIFBA28(15)*E**(4) + cIFBA28(16)*E**(3) + cIFBA28(17)*E**(2) + cIFBA28(18)*E + cIFBA28(19))*B + (cIFBA28(20)*E**(4) + cIFBA28(21)*E**(3) + cIFBA28(22)*E**(2) + cIFBA28(23)*E + cIFBA28(24))

end function get_M2_NO_XE_IFBA_DIFF_1

double precision function get_M2_NO_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA29 = (/ & 
    -2.06728992916e-08, &
     3.30024046487e-07, &
    -1.96174750618e-06, &
     5.15631476528e-06, &
    -5.04348629617e-06, &
     4.26761531500e-06, &
    -6.80574074364e-05, &
     4.03986109449e-04, &
    -1.06048382834e-03, &
     1.03594304107e-03, &
    -3.26525961793e-04, &
     5.20374650423e-03, &
    -3.08531007139e-02, &
     8.09033482663e-02, &
    -7.89410168697e-02, &
     1.09862888740e-02, &
    -1.75051147681e-01, &
     1.03696222039e+00, &
    -2.71700438488e+00, &
     2.64896701695e+00, &
    -1.36562921686e-01, &
     2.17643546151e+00, &
    -1.28830886408e+01, &
     3.37366000320e+01, &
    -3.28488857429e+01  &
    /)
	get_M2_NO_XE_IFBA_DIFF_2 =  (cIFBA29(0)*E**(4) + cIFBA29(1)*E**(3) + cIFBA29(2)*E**(2) + cIFBA29(3)*E + cIFBA29(4))*B**(4) + (cIFBA29(5)*E**(4) + cIFBA29(6)*E**(3) + cIFBA29(7)*E**(2) + cIFBA29(8)*E + cIFBA29(9))*B**(3) + (cIFBA29(10)*E**(4) + cIFBA29(11)*E**(3) + cIFBA29(12)*E**(2) + cIFBA29(13)*E + cIFBA29(14))*B**(2) + (cIFBA29(15)*E**(4) + cIFBA29(16)*E**(3) + cIFBA29(17)*E**(2) + cIFBA29(18)*E + cIFBA29(19))*B + (cIFBA29(20)*E**(4) + cIFBA29(21)*E**(3) + cIFBA29(22)*E**(2) + cIFBA29(23)*E + cIFBA29(24))

end function get_M2_NO_XE_IFBA_DIFF_2

double precision function get_M2_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA30 = (/ & 
     2.36738151395e-02, &
    -3.98448847769e-01, &
     2.52552726926e+00, &
    -6.79834847755e+00, &
     8.43532122958e-01, &
     2.36738608312e-03, &
    -5.10143436609e-02, &
     4.57768774013e-01, &
    -2.27452321960e+00, &
     7.33656640674e+00  &
    /)
	get_M2_XE_IFBA_DIFF_0 =  (cIFBA30(0)*E**(4) + cIFBA30(1)*E**(3) + cIFBA30(2)*E**(2) + cIFBA30(3)*E + cIFBA30(4))*B + (cIFBA30(5)*E**(4) + cIFBA30(6)*E**(3) + cIFBA30(7)*E**(2) + cIFBA30(8)*E + cIFBA30(9))

end function get_M2_XE_IFBA_DIFF_0

double precision function get_M2_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA31 = (/ & 
     1.52938715987e-07, &
    -3.00547463814e-06, &
     2.31776208626e-05, &
    -8.47112119213e-05, &
     1.27230147902e-04, &
    -1.06607040385e-05, &
     2.10807916625e-04, &
    -1.64515768706e-03, &
     6.14649379005e-03, &
    -9.61202957520e-03, &
     2.37530733508e-04, &
    -4.73619167000e-03, &
     3.76303074149e-02, &
    -1.45773530423e-01, &
     2.45204764496e-01, &
    -1.73021069444e-03, &
     3.49660721845e-02, &
    -2.87074662504e-01, &
     1.19539460381e+00, &
    -2.36618609372e+00, &
     2.00098562475e-03, &
    -4.04676250588e-02, &
     3.56672438970e-01, &
    -1.84647649114e+00, &
     6.52066528766e+00  &
    /)
	get_M2_XE_IFBA_DIFF_1 =  (cIFBA31(0)*E**(4) + cIFBA31(1)*E**(3) + cIFBA31(2)*E**(2) + cIFBA31(3)*E + cIFBA31(4))*B**(4) + (cIFBA31(5)*E**(4) + cIFBA31(6)*E**(3) + cIFBA31(7)*E**(2) + cIFBA31(8)*E + cIFBA31(9))*B**(3) + (cIFBA31(10)*E**(4) + cIFBA31(11)*E**(3) + cIFBA31(12)*E**(2) + cIFBA31(13)*E + cIFBA31(14))*B**(2) + (cIFBA31(15)*E**(4) + cIFBA31(16)*E**(3) + cIFBA31(17)*E**(2) + cIFBA31(18)*E + cIFBA31(19))*B + (cIFBA31(20)*E**(4) + cIFBA31(21)*E**(3) + cIFBA31(22)*E**(2) + cIFBA31(23)*E + cIFBA31(24))

end function get_M2_XE_IFBA_DIFF_1

double precision function get_M2_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA32 = (/ & 
    -9.47210632170e-09, &
     1.66361443261e-07, &
    -1.06415794534e-06, &
     2.95169271082e-06, &
    -2.98580495410e-06, &
     1.55415971563e-06, &
    -2.78449484160e-05, &
     1.80550846124e-04, &
    -5.05902364568e-04, &
     5.14812722602e-04, &
    -8.55348289437e-05, &
     1.58875074383e-03, &
    -1.05466677991e-02, &
     3.00700043937e-02, &
    -3.09228525858e-02, &
     1.71702780932e-03, &
    -3.45577904235e-02, &
     2.40528113272e-01, &
    -7.09281035157e-01, &
     7.44546108496e-01, &
    -8.05874431385e-03, &
     2.12466232981e-01, &
    -1.66909238505e+00, &
     5.31277690748e+00, &
    -5.80255913801e+00  &
    /)
	get_M2_XE_IFBA_DIFF_2 =  (cIFBA32(0)*E**(4) + cIFBA32(1)*E**(3) + cIFBA32(2)*E**(2) + cIFBA32(3)*E + cIFBA32(4))*B**(4) + (cIFBA32(5)*E**(4) + cIFBA32(6)*E**(3) + cIFBA32(7)*E**(2) + cIFBA32(8)*E + cIFBA32(9))*B**(3) + (cIFBA32(10)*E**(4) + cIFBA32(11)*E**(3) + cIFBA32(12)*E**(2) + cIFBA32(13)*E + cIFBA32(14))*B**(2) + (cIFBA32(15)*E**(4) + cIFBA32(16)*E**(3) + cIFBA32(17)*E**(2) + cIFBA32(18)*E + cIFBA32(19))*B + (cIFBA32(20)*E**(4) + cIFBA32(21)*E**(3) + cIFBA32(22)*E**(2) + cIFBA32(23)*E + cIFBA32(24))

end function get_M2_XE_IFBA_DIFF_2

double precision function get_NUFISS1_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA33 = (/ & 
    -5.28117278147e-06, &
     8.83594847487e-05, &
    -5.53925837901e-04, &
     1.58820101632e-03, &
    -1.72408971137e-03, &
     4.18853059964e-07, &
    -7.05372753575e-06, &
     4.60352143869e-05, &
    -1.40292133147e-04, &
     1.20503295444e-04  &
    /)
	get_NUFISS1_IFBA_DIFF_0 =  (cIFBA33(0)*E**(4) + cIFBA33(1)*E**(3) + cIFBA33(2)*E**(2) + cIFBA33(3)*E + cIFBA33(4))*B + (cIFBA33(5)*E**(4) + cIFBA33(6)*E**(3) + cIFBA33(7)*E**(2) + cIFBA33(8)*E + cIFBA33(9))

end function get_NUFISS1_IFBA_DIFF_0

double precision function get_NUFISS1_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA34 = (/ & 
    -3.48991117394e-12, &
     5.77427670649e-11, &
    -3.53818739342e-10, &
     9.51122142595e-10, &
    -9.49546727473e-10, &
     2.70687938870e-10, &
    -4.51555010149e-09, &
     2.79413837784e-08, &
    -7.59925415338e-08, &
     7.69211424646e-08, &
    -7.14300010400e-09, &
     1.20408977464e-07, &
    -7.54951029960e-07, &
     2.08775267029e-06, &
    -2.15815401044e-06, &
     7.52600990293e-08, &
    -1.28560036790e-06, &
     8.21281847917e-06, &
    -2.33211251932e-05, &
     2.50224926281e-05, &
    -2.94698794671e-07, &
     5.09166780289e-06, &
    -3.32643686109e-05, &
     9.83323719523e-05, &
    -1.12679094882e-04, &
     3.20832050070e-07, &
    -5.63890440251e-06, &
     3.85738520367e-05, &
    -1.22741062436e-04, &
     1.08214961091e-04  &
    /)
	get_NUFISS1_IFBA_DIFF_1 =  (cIFBA34(0)*E**(4) + cIFBA34(1)*E**(3) + cIFBA34(2)*E**(2) + cIFBA34(3)*E + cIFBA34(4))*B**(5) + (cIFBA34(5)*E**(4) + cIFBA34(6)*E**(3) + cIFBA34(7)*E**(2) + cIFBA34(8)*E + cIFBA34(9))*B**(4) + (cIFBA34(10)*E**(4) + cIFBA34(11)*E**(3) + cIFBA34(12)*E**(2) + cIFBA34(13)*E + cIFBA34(14))*B**(3) + (cIFBA34(15)*E**(4) + cIFBA34(16)*E**(3) + cIFBA34(17)*E**(2) + cIFBA34(18)*E + cIFBA34(19))*B**(2) + (cIFBA34(20)*E**(4) + cIFBA34(21)*E**(3) + cIFBA34(22)*E**(2) + cIFBA34(23)*E + cIFBA34(24))*B + (cIFBA34(25)*E**(4) + cIFBA34(26)*E**(3) + cIFBA34(27)*E**(2) + cIFBA34(28)*E + cIFBA34(29))

end function get_NUFISS1_IFBA_DIFF_1

double precision function get_NUFISS1_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:14) :: cIFBA35 = (/ & 
     5.91211161748e-10, &
    -1.04589916195e-08, &
     6.61847784470e-08, &
    -1.73945925412e-07, &
     1.55302051869e-07, &
    -5.84449011482e-08, &
     1.07443630454e-06, &
    -7.06857444017e-06, &
     1.92844495265e-05, &
    -1.72695666924e-05, &
     1.22859159987e-06, &
    -2.41434583882e-05, &
     1.69720675086e-04, &
    -4.99261705968e-04, &
     4.51806863218e-04  &
    /)
	get_NUFISS1_IFBA_DIFF_2 =  (cIFBA35(0)*E**(4) + cIFBA35(1)*E**(3) + cIFBA35(2)*E**(2) + cIFBA35(3)*E + cIFBA35(4))*B**(2) + (cIFBA35(5)*E**(4) + cIFBA35(6)*E**(3) + cIFBA35(7)*E**(2) + cIFBA35(8)*E + cIFBA35(9))*B + (cIFBA35(10)*E**(4) + cIFBA35(11)*E**(3) + cIFBA35(12)*E**(2) + cIFBA35(13)*E + cIFBA35(14))

end function get_NUFISS1_IFBA_DIFF_2

double precision function get_K2_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA36 = (/ & 
     4.55283166405e-04, &
    -9.14204824075e-03, &
     7.29896642140e-02, &
    -2.58556208930e-01, &
    -4.55614035822e-02, &
    -4.55267325364e-05, &
     8.42792644830e-04, &
    -4.31309449378e-03, &
    -1.48936025776e-02, &
     2.71179243961e-01  &
    /)
	get_K2_IFBA_DIFF_0 =  (cIFBA36(0)*E**(4) + cIFBA36(1)*E**(3) + cIFBA36(2)*E**(2) + cIFBA36(3)*E + cIFBA36(4))*B + (cIFBA36(5)*E**(4) + cIFBA36(6)*E**(3) + cIFBA36(7)*E**(2) + cIFBA36(8)*E + cIFBA36(9))

end function get_K2_IFBA_DIFF_0

double precision function get_K2_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA37 = (/ & 
     5.97397552550e-09, &
    -1.19146431884e-07, &
     9.53924224211e-07, &
    -3.70329011629e-06, &
     6.02684452275e-06, &
    -3.80457550103e-07, &
     7.72056736413e-06, &
    -6.35419863598e-05, &
     2.57316028927e-04, &
    -4.46573431486e-04, &
     7.02167597956e-06, &
    -1.47095026268e-04, &
     1.27590663721e-03, &
    -5.60095708457e-03, &
     1.10063182504e-02, &
    -2.73919785376e-05, &
     6.39077383500e-04, &
    -6.58443462065e-03, &
     3.65234904390e-02, &
    -9.91856031800e-02, &
    -9.87645835192e-05, &
     1.88435446361e-03, &
    -1.19991945513e-02, &
     1.19348296393e-02, &
     2.22344629935e-01  &
    /)
	get_K2_IFBA_DIFF_1 =  (cIFBA37(0)*E**(4) + cIFBA37(1)*E**(3) + cIFBA37(2)*E**(2) + cIFBA37(3)*E + cIFBA37(4))*B**(4) + (cIFBA37(5)*E**(4) + cIFBA37(6)*E**(3) + cIFBA37(7)*E**(2) + cIFBA37(8)*E + cIFBA37(9))*B**(3) + (cIFBA37(10)*E**(4) + cIFBA37(11)*E**(3) + cIFBA37(12)*E**(2) + cIFBA37(13)*E + cIFBA37(14))*B**(2) + (cIFBA37(15)*E**(4) + cIFBA37(16)*E**(3) + cIFBA37(17)*E**(2) + cIFBA37(18)*E + cIFBA37(19))*B + (cIFBA37(20)*E**(4) + cIFBA37(21)*E**(3) + cIFBA37(22)*E**(2) + cIFBA37(23)*E + cIFBA37(24))

end function get_K2_IFBA_DIFF_1

double precision function get_K2_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA38 = (/ & 
    -1.25860472352e-09, &
     1.99585239235e-08, &
    -1.16791070996e-07, &
     2.99823946196e-07, &
    -2.84823300958e-07, &
     2.60675069943e-07, &
    -4.14015154126e-06, &
     2.42522274860e-05, &
    -6.23084357879e-05, &
     5.92069540843e-05, &
    -1.98399383731e-05, &
     3.15784998792e-04, &
    -1.85291205861e-03, &
     4.76808485921e-03, &
    -4.53609473735e-03, &
     6.55393883846e-04, &
    -1.04594877910e-02, &
     6.15097556109e-02, &
    -1.58691185721e-01, &
     1.51378322652e-01, &
    -7.89941551391e-03, &
     1.26417564463e-01, &
    -7.45041658165e-01, &
     1.92818702428e+00, &
    -1.84851658444e+00  &
    /)
	get_K2_IFBA_DIFF_2 =  (cIFBA38(0)*E**(4) + cIFBA38(1)*E**(3) + cIFBA38(2)*E**(2) + cIFBA38(3)*E + cIFBA38(4))*B**(4) + (cIFBA38(5)*E**(4) + cIFBA38(6)*E**(3) + cIFBA38(7)*E**(2) + cIFBA38(8)*E + cIFBA38(9))*B**(3) + (cIFBA38(10)*E**(4) + cIFBA38(11)*E**(3) + cIFBA38(12)*E**(2) + cIFBA38(13)*E + cIFBA38(14))*B**(2) + (cIFBA38(15)*E**(4) + cIFBA38(16)*E**(3) + cIFBA38(17)*E**(2) + cIFBA38(18)*E + cIFBA38(19))*B + (cIFBA38(20)*E**(4) + cIFBA38(21)*E**(3) + cIFBA38(22)*E**(2) + cIFBA38(23)*E + cIFBA38(24))

end function get_K2_IFBA_DIFF_2

double precision function get_K1_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA39 = (/ & 
    -1.27482887564e-04, &
     2.13927952437e-03, &
    -1.31132064985e-02, &
     3.44486084536e-02, &
    -4.82439183713e-02, &
    -7.28416336041e-06, &
     1.16546598823e-04, &
    -7.05689977267e-04, &
     2.04102890545e-03, &
     6.00096259304e-03  &
    /)
	get_K1_IFBA_DIFF_0 =  (cIFBA39(0)*E**(4) + cIFBA39(1)*E**(3) + cIFBA39(2)*E**(2) + cIFBA39(3)*E + cIFBA39(4))*B + (cIFBA39(5)*E**(4) + cIFBA39(6)*E**(3) + cIFBA39(7)*E**(2) + cIFBA39(8)*E + cIFBA39(9))

end function get_K1_IFBA_DIFF_0

double precision function get_K1_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA40 = (/ & 
    -1.54383405036e-10, &
     2.66397561457e-09, &
    -1.72001193132e-08, &
     4.95911318995e-08, &
    -5.48712905638e-08, &
     1.17399076810e-08, &
    -2.05196592769e-07, &
     1.34629918143e-06, &
    -3.96695469901e-06, &
     4.53897971637e-06, &
    -3.01217230849e-07, &
     5.36018657172e-06, &
    -3.59975473246e-05, &
     1.09681987298e-04, &
    -1.32830631149e-04, &
     2.98306461856e-06, &
    -5.44334797644e-05, &
     3.78545596197e-04, &
    -1.21914493138e-03, &
     1.64939706677e-03, &
    -9.38701228688e-06, &
     1.75593814300e-04, &
    -1.27592725294e-03, &
     4.50381210225e-03, &
    -8.02961066466e-03, &
     2.24162877703e-06, &
    -3.50251383846e-05, &
     1.99290925112e-04, &
    -3.38836499736e-04, &
     8.06094639544e-03  &
    /)
	get_K1_IFBA_DIFF_1 =  (cIFBA40(0)*E**(4) + cIFBA40(1)*E**(3) + cIFBA40(2)*E**(2) + cIFBA40(3)*E + cIFBA40(4))*B**(5) + (cIFBA40(5)*E**(4) + cIFBA40(6)*E**(3) + cIFBA40(7)*E**(2) + cIFBA40(8)*E + cIFBA40(9))*B**(4) + (cIFBA40(10)*E**(4) + cIFBA40(11)*E**(3) + cIFBA40(12)*E**(2) + cIFBA40(13)*E + cIFBA40(14))*B**(3) + (cIFBA40(15)*E**(4) + cIFBA40(16)*E**(3) + cIFBA40(17)*E**(2) + cIFBA40(18)*E + cIFBA40(19))*B**(2) + (cIFBA40(20)*E**(4) + cIFBA40(21)*E**(3) + cIFBA40(22)*E**(2) + cIFBA40(23)*E + cIFBA40(24))*B + (cIFBA40(25)*E**(4) + cIFBA40(26)*E**(3) + cIFBA40(27)*E**(2) + cIFBA40(28)*E + cIFBA40(29))

end function get_K1_IFBA_DIFF_1

double precision function get_K1_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cIFBA41 = (/ & 
    -4.15032998075e-09, &
     6.85388311671e-08, &
    -4.17133534125e-07, &
     1.10818448469e-06, &
    -1.08824640337e-06, &
     6.46784838056e-07, &
    -1.07009393396e-05, &
     6.51982651302e-05, &
    -1.73063117960e-04, &
     1.69252578718e-04, &
    -3.24423487323e-05, &
     5.39050894146e-04, &
    -3.29756166887e-03, &
     8.77036554914e-03, &
    -8.53538824095e-03, &
     5.19546599455e-04, &
    -8.69658096097e-03, &
     5.36793686608e-02, &
    -1.44152305777e-01, &
     1.39645993384e-01  &
    /)
	get_K1_IFBA_DIFF_2 =  (cIFBA41(0)*E**(4) + cIFBA41(1)*E**(3) + cIFBA41(2)*E**(2) + cIFBA41(3)*E + cIFBA41(4))*B**(3) + (cIFBA41(5)*E**(4) + cIFBA41(6)*E**(3) + cIFBA41(7)*E**(2) + cIFBA41(8)*E + cIFBA41(9))*B**(2) + (cIFBA41(10)*E**(4) + cIFBA41(11)*E**(3) + cIFBA41(12)*E**(2) + cIFBA41(13)*E + cIFBA41(14))*B + (cIFBA41(15)*E**(4) + cIFBA41(16)*E**(3) + cIFBA41(17)*E**(2) + cIFBA41(18)*E + cIFBA41(19))

end function get_K1_IFBA_DIFF_2

double precision function get_NUFISS2_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA42 = (/ & 
    -1.27475137205e-04, &
     2.12458670963e-03, &
    -1.29259894823e-02, &
     2.86826981508e-02, &
    -3.09602363175e-02, &
    -1.82172872771e-06, &
     2.79335664644e-05, &
    -2.46490446732e-04, &
     2.00743028112e-03, &
     8.31364958614e-04  &
    /)
	get_NUFISS2_IFBA_DIFF_0 =  (cIFBA42(0)*E**(4) + cIFBA42(1)*E**(3) + cIFBA42(2)*E**(2) + cIFBA42(3)*E + cIFBA42(4))*B + (cIFBA42(5)*E**(4) + cIFBA42(6)*E**(3) + cIFBA42(7)*E**(2) + cIFBA42(8)*E + cIFBA42(9))

end function get_NUFISS2_IFBA_DIFF_0

double precision function get_NUFISS2_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA43 = (/ & 
     1.17708078191e-10, &
    -1.78070097157e-09, &
     9.64204473727e-09, &
    -2.10611894090e-08, &
     1.27188777030e-08, &
    -1.02141890677e-08, &
     1.54844550539e-07, &
    -8.38976815011e-07, &
     1.81942746034e-06, &
    -1.02847191713e-06, &
     3.18551484438e-07, &
    -4.85455312108e-06, &
     2.64318759016e-05, &
    -5.71368965687e-05, &
     2.92798222096e-05, &
    -4.21473003946e-06, &
     6.50282818231e-05, &
    -3.59691974912e-04, &
     7.87309637978e-04, &
    -3.45913260028e-04, &
     2.04021315762e-05, &
    -3.25101709262e-04, &
     1.88867839566e-03, &
    -4.46855942834e-03, &
     1.60290604026e-03, &
    -2.01938263152e-05, &
     3.51541503755e-04, &
    -2.35807565005e-03, &
     8.02731574705e-03, &
    -5.51220826901e-03  &
    /)
	get_NUFISS2_IFBA_DIFF_1 =  (cIFBA43(0)*E**(4) + cIFBA43(1)*E**(3) + cIFBA43(2)*E**(2) + cIFBA43(3)*E + cIFBA43(4))*B**(5) + (cIFBA43(5)*E**(4) + cIFBA43(6)*E**(3) + cIFBA43(7)*E**(2) + cIFBA43(8)*E + cIFBA43(9))*B**(4) + (cIFBA43(10)*E**(4) + cIFBA43(11)*E**(3) + cIFBA43(12)*E**(2) + cIFBA43(13)*E + cIFBA43(14))*B**(3) + (cIFBA43(15)*E**(4) + cIFBA43(16)*E**(3) + cIFBA43(17)*E**(2) + cIFBA43(18)*E + cIFBA43(19))*B**(2) + (cIFBA43(20)*E**(4) + cIFBA43(21)*E**(3) + cIFBA43(22)*E**(2) + cIFBA43(23)*E + cIFBA43(24))*B + (cIFBA43(25)*E**(4) + cIFBA43(26)*E**(3) + cIFBA43(27)*E**(2) + cIFBA43(28)*E + cIFBA43(29))

end function get_NUFISS2_IFBA_DIFF_1

double precision function get_NUFISS2_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:14) :: cIFBA44 = (/ & 
     1.61196142219e-08, &
    -2.31496864059e-07, &
     1.15450134359e-06, &
    -2.18423361192e-06, &
     8.46787218288e-07, &
    -1.78305377143e-06, &
     2.66561164732e-05, &
    -1.42074369167e-04, &
     3.02338793073e-04, &
    -1.49221584760e-04, &
     4.43918022293e-05, &
    -6.87276016248e-04, &
     3.92300680025e-03, &
    -9.60595115598e-03, &
     5.38276977870e-03  &
    /)
	get_NUFISS2_IFBA_DIFF_2 =  (cIFBA44(0)*E**(4) + cIFBA44(1)*E**(3) + cIFBA44(2)*E**(2) + cIFBA44(3)*E + cIFBA44(4))*B**(2) + (cIFBA44(5)*E**(4) + cIFBA44(6)*E**(3) + cIFBA44(7)*E**(2) + cIFBA44(8)*E + cIFBA44(9))*B + (cIFBA44(10)*E**(4) + cIFBA44(11)*E**(3) + cIFBA44(12)*E**(2) + cIFBA44(13)*E + cIFBA44(14))

end function get_NUFISS2_IFBA_DIFF_2

double precision function get_BOR1_NO_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA45 = (/ & 
     1.09262484486e-07, &
    -1.64136198318e-06, &
     8.97841892901e-06, &
    -2.18210589408e-05, &
     3.34204218364e-05, &
    -3.46010067690e-08, &
     5.78140197340e-07, &
    -3.73348261436e-06, &
     1.18920734766e-05, &
    -1.90042883962e-05  &
    /)
	get_BOR1_NO_XE_IFBA_DIFF_0 =  (cIFBA45(0)*E**(4) + cIFBA45(1)*E**(3) + cIFBA45(2)*E**(2) + cIFBA45(3)*E + cIFBA45(4))*B + (cIFBA45(5)*E**(4) + cIFBA45(6)*E**(3) + cIFBA45(7)*E**(2) + cIFBA45(8)*E + cIFBA45(9))

end function get_BOR1_NO_XE_IFBA_DIFF_0

double precision function get_BOR1_NO_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA46 = (/ & 
     1.34110936187e-14, &
    -3.13118878608e-13, &
     2.58628566455e-12, &
    -9.23049427214e-12, &
     1.22746887345e-11, &
    -1.20847546714e-12, &
     2.84896063636e-11, &
    -2.38603029622e-10, &
     8.67832121753e-10, &
    -1.18450193778e-09, &
     3.51198307329e-11, &
    -8.72663900385e-10, &
     7.63515829049e-09, &
    -2.90395904230e-08, &
     4.18024058139e-08, &
    -3.59567865797e-10, &
     1.03419490331e-08, &
    -9.99198740881e-08, &
     4.15939330085e-07, &
    -6.63426153860e-07, &
     5.76581251854e-10, &
    -3.50706674338e-08, &
     4.49122175145e-07, &
    -2.29792966741e-06, &
     4.56344512385e-06, &
     1.56045101329e-09, &
     9.60432792355e-10, &
    -3.14513255218e-07, &
     2.95096462991e-06, &
    -1.00283250160e-05  &
    /)
	get_BOR1_NO_XE_IFBA_DIFF_1 =  (cIFBA46(0)*E**(4) + cIFBA46(1)*E**(3) + cIFBA46(2)*E**(2) + cIFBA46(3)*E + cIFBA46(4))*B**(5) + (cIFBA46(5)*E**(4) + cIFBA46(6)*E**(3) + cIFBA46(7)*E**(2) + cIFBA46(8)*E + cIFBA46(9))*B**(4) + (cIFBA46(10)*E**(4) + cIFBA46(11)*E**(3) + cIFBA46(12)*E**(2) + cIFBA46(13)*E + cIFBA46(14))*B**(3) + (cIFBA46(15)*E**(4) + cIFBA46(16)*E**(3) + cIFBA46(17)*E**(2) + cIFBA46(18)*E + cIFBA46(19))*B**(2) + (cIFBA46(20)*E**(4) + cIFBA46(21)*E**(3) + cIFBA46(22)*E**(2) + cIFBA46(23)*E + cIFBA46(24))*B + (cIFBA46(25)*E**(4) + cIFBA46(26)*E**(3) + cIFBA46(27)*E**(2) + cIFBA46(28)*E + cIFBA46(29))

end function get_BOR1_NO_XE_IFBA_DIFF_1

double precision function get_BOR1_NO_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA47 = (/ & 
     2.94300320736e-14, &
    -4.67652435195e-13, &
     2.75343906486e-12, &
    -7.11228176556e-12, &
     6.79982673454e-12, &
    -7.97670848188e-12, &
     1.26635568991e-10, &
    -7.44866013179e-10, &
     1.92193286792e-09, &
    -1.83520778344e-09, &
     8.50821788859e-10, &
    -1.34979494190e-08, &
     7.93349030994e-08, &
    -2.04532262790e-07, &
     1.95113460792e-07, &
    -4.45701255334e-08, &
     7.06749767629e-07, &
    -4.15179648437e-06, &
     1.06973097706e-05, &
    -1.01973303650e-05, &
     1.14559724859e-06, &
    -1.81610841920e-05, &
     1.06655873863e-04, &
    -2.74702002145e-04, &
     2.61725912154e-04, &
    -1.15523293615e-05, &
     1.83132090567e-04, &
    -1.07544122275e-03, &
     2.76956961047e-03, &
    -2.63765181013e-03  &
    /)
	get_BOR1_NO_XE_IFBA_DIFF_2 =  (cIFBA47(0)*E**(4) + cIFBA47(1)*E**(3) + cIFBA47(2)*E**(2) + cIFBA47(3)*E + cIFBA47(4))*B**(5) + (cIFBA47(5)*E**(4) + cIFBA47(6)*E**(3) + cIFBA47(7)*E**(2) + cIFBA47(8)*E + cIFBA47(9))*B**(4) + (cIFBA47(10)*E**(4) + cIFBA47(11)*E**(3) + cIFBA47(12)*E**(2) + cIFBA47(13)*E + cIFBA47(14))*B**(3) + (cIFBA47(15)*E**(4) + cIFBA47(16)*E**(3) + cIFBA47(17)*E**(2) + cIFBA47(18)*E + cIFBA47(19))*B**(2) + (cIFBA47(20)*E**(4) + cIFBA47(21)*E**(3) + cIFBA47(22)*E**(2) + cIFBA47(23)*E + cIFBA47(24))*B + (cIFBA47(25)*E**(4) + cIFBA47(26)*E**(3) + cIFBA47(27)*E**(2) + cIFBA47(28)*E + cIFBA47(29))

end function get_BOR1_NO_XE_IFBA_DIFF_2

double precision function get_BOR2_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA48 = (/ & 
    -7.28454215506e-02, &
     1.00527027165e+00, &
    -4.74665461981e+00, &
     8.31915808180e+00, &
    -1.48640164075e+01, &
     1.82110067051e-02, &
    -2.88462356408e-01, &
     1.80551186229e+00, &
    -6.27699409791e+00, &
     1.57636487386e+01  &
    /)
	get_BOR2_XE_IFBA_DIFF_0 =  (cIFBA48(0)*E**(4) + cIFBA48(1)*E**(3) + cIFBA48(2)*E**(2) + cIFBA48(3)*E + cIFBA48(4))*B + (cIFBA48(5)*E**(4) + cIFBA48(6)*E**(3) + cIFBA48(7)*E**(2) + cIFBA48(8)*E + cIFBA48(9))

end function get_BOR2_XE_IFBA_DIFF_0

double precision function get_BOR2_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA49 = (/ & 
     1.18071218118e-06, &
    -1.98487595324e-05, &
     1.27419216918e-04, &
    -3.75387539930e-04, &
     4.36712787474e-04, &
    -7.76411816838e-05, &
     1.31357150006e-03, &
    -8.52610379502e-03, &
     2.56350921431e-02, &
    -3.09993991891e-02, &
     1.59654322801e-03, &
    -2.71984039632e-02, &
     1.79191699085e-01, &
    -5.56536634913e-01, &
     7.21946741535e-01, &
    -1.04547042476e-02, &
     1.79005703700e-01, &
    -1.20505213720e+00, &
     3.98770084449e+00, &
    -6.09113873275e+00, &
     1.02963517661e-02, &
    -1.66372654984e-01, &
     1.11046821130e+00, &
    -4.55296202853e+00, &
     1.40422551989e+01  &
    /)
	get_BOR2_XE_IFBA_DIFF_1 =  (cIFBA49(0)*E**(4) + cIFBA49(1)*E**(3) + cIFBA49(2)*E**(2) + cIFBA49(3)*E + cIFBA49(4))*B**(4) + (cIFBA49(5)*E**(4) + cIFBA49(6)*E**(3) + cIFBA49(7)*E**(2) + cIFBA49(8)*E + cIFBA49(9))*B**(3) + (cIFBA49(10)*E**(4) + cIFBA49(11)*E**(3) + cIFBA49(12)*E**(2) + cIFBA49(13)*E + cIFBA49(14))*B**(2) + (cIFBA49(15)*E**(4) + cIFBA49(16)*E**(3) + cIFBA49(17)*E**(2) + cIFBA49(18)*E + cIFBA49(19))*B + (cIFBA49(20)*E**(4) + cIFBA49(21)*E**(3) + cIFBA49(22)*E**(2) + cIFBA49(23)*E + cIFBA49(24))

end function get_BOR2_XE_IFBA_DIFF_1

double precision function get_BOR2_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA50 = (/ & 
    -8.15264913513e-08, &
     1.37192038619e-06, &
    -8.70038469636e-06, &
     2.46539314907e-05, &
    -2.62587140660e-05, &
     1.07720188534e-05, &
    -1.86525501362e-04, &
     1.22362492492e-03, &
    -3.60151098308e-03, &
     3.98971828144e-03, &
    -3.43253598357e-04, &
     6.60271703114e-03, &
    -4.82049508972e-02, &
     1.57290091310e-01, &
    -1.91115613295e-01, &
    -4.22555887820e-03, &
     2.88591406169e-02, &
     1.36210323952e-01, &
    -1.41748397802e+00, &
     2.67191220969e+00, &
     2.28234983706e-01, &
    -3.13999109429e+00, &
     1.46920633230e+01, &
    -2.49649287641e+01, &
     8.40586402176e+00  &
    /)
	get_BOR2_XE_IFBA_DIFF_2 =  (cIFBA50(0)*E**(4) + cIFBA50(1)*E**(3) + cIFBA50(2)*E**(2) + cIFBA50(3)*E + cIFBA50(4))*B**(4) + (cIFBA50(5)*E**(4) + cIFBA50(6)*E**(3) + cIFBA50(7)*E**(2) + cIFBA50(8)*E + cIFBA50(9))*B**(3) + (cIFBA50(10)*E**(4) + cIFBA50(11)*E**(3) + cIFBA50(12)*E**(2) + cIFBA50(13)*E + cIFBA50(14))*B**(2) + (cIFBA50(15)*E**(4) + cIFBA50(16)*E**(3) + cIFBA50(17)*E**(2) + cIFBA50(18)*E + cIFBA50(19))*B + (cIFBA50(20)*E**(4) + cIFBA50(21)*E**(3) + cIFBA50(22)*E**(2) + cIFBA50(23)*E + cIFBA50(24))

end function get_BOR2_XE_IFBA_DIFF_2

double precision function get_REMOV1_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA51 = (/ & 
     3.27798350920e-05, &
    -5.21077994935e-04, &
     3.06280242530e-03, &
    -7.96018832349e-03, &
     8.93851314980e-03, &
    -2.73175928420e-06, &
     4.59177493174e-05, &
    -2.99872667733e-04, &
     9.75459242916e-04, &
    -1.53096752648e-03  &
    /)
	get_REMOV1_IFBA_DIFF_0 =  (cIFBA51(0)*E**(4) + cIFBA51(1)*E**(3) + cIFBA51(2)*E**(2) + cIFBA51(3)*E + cIFBA51(4))*B + (cIFBA51(5)*E**(4) + cIFBA51(6)*E**(3) + cIFBA51(7)*E**(2) + cIFBA51(8)*E + cIFBA51(9))

end function get_REMOV1_IFBA_DIFF_0

double precision function get_REMOV1_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA52 = (/ & 
    -1.06431903529e-12, &
     8.85222280293e-12, &
     7.94352124550e-12, &
    -2.25356476102e-10, &
     4.86801680897e-10, &
     5.47210905751e-11, &
    -1.25860339673e-10, &
    -4.96092133970e-09, &
     3.29570962150e-08, &
    -5.93547088546e-08, &
    -1.22369740355e-09, &
    -5.26542326004e-09, &
     2.29146922875e-07, &
    -1.33176910441e-06, &
     2.36837844800e-06, &
     2.38882491604e-08, &
    -2.45945072968e-08, &
    -2.91027633088e-06, &
     1.99886681736e-05, &
    -3.97618516712e-05, &
    -2.33495518025e-07, &
     1.72315377587e-06, &
     9.08144757248e-06, &
    -1.15173689885e-04, &
     2.95106122590e-04, &
     3.03544193496e-07, &
    -2.88894851135e-06, &
    -8.89527970657e-06, &
     2.10462243007e-04, &
    -7.59064191927e-04  &
    /)
	get_REMOV1_IFBA_DIFF_1 =  (cIFBA52(0)*E**(4) + cIFBA52(1)*E**(3) + cIFBA52(2)*E**(2) + cIFBA52(3)*E + cIFBA52(4))*B**(5) + (cIFBA52(5)*E**(4) + cIFBA52(6)*E**(3) + cIFBA52(7)*E**(2) + cIFBA52(8)*E + cIFBA52(9))*B**(4) + (cIFBA52(10)*E**(4) + cIFBA52(11)*E**(3) + cIFBA52(12)*E**(2) + cIFBA52(13)*E + cIFBA52(14))*B**(3) + (cIFBA52(15)*E**(4) + cIFBA52(16)*E**(3) + cIFBA52(17)*E**(2) + cIFBA52(18)*E + cIFBA52(19))*B**(2) + (cIFBA52(20)*E**(4) + cIFBA52(21)*E**(3) + cIFBA52(22)*E**(2) + cIFBA52(23)*E + cIFBA52(24))*B + (cIFBA52(25)*E**(4) + cIFBA52(26)*E**(3) + cIFBA52(27)*E**(2) + cIFBA52(28)*E + cIFBA52(29))

end function get_REMOV1_IFBA_DIFF_1

double precision function get_REMOV1_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA53 = (/ & 
     1.66572318793e-12, &
    -2.83231147310e-11, &
     1.77570747237e-10, &
    -4.85977433349e-10, &
     4.89941848553e-10, &
    -4.77004007610e-10, &
     8.09864995918e-09, &
    -5.07120288919e-08, &
     1.38650761047e-07, &
    -1.39656774237e-07, &
     5.33356565477e-08, &
    -9.04249807419e-07, &
     5.65537879020e-06, &
    -1.54463694818e-05, &
     1.55435147639e-05, &
    -2.91086707865e-06, &
     4.92823050140e-05, &
    -3.07850712668e-04, &
     8.39927645973e-04, &
    -8.44337403166e-04, &
     7.75504071702e-05, &
    -1.31119218503e-03, &
     8.18075437877e-03, &
    -2.22954474018e-02, &
     2.23870949288e-02, &
    -8.06720681368e-04, &
     1.36221756323e-02, &
    -8.48931419691e-02, &
     2.31114426837e-01, &
    -2.31761681715e-01  &
    /)
	get_REMOV1_IFBA_DIFF_2 =  (cIFBA53(0)*E**(4) + cIFBA53(1)*E**(3) + cIFBA53(2)*E**(2) + cIFBA53(3)*E + cIFBA53(4))*B**(5) + (cIFBA53(5)*E**(4) + cIFBA53(6)*E**(3) + cIFBA53(7)*E**(2) + cIFBA53(8)*E + cIFBA53(9))*B**(4) + (cIFBA53(10)*E**(4) + cIFBA53(11)*E**(3) + cIFBA53(12)*E**(2) + cIFBA53(13)*E + cIFBA53(14))*B**(3) + (cIFBA53(15)*E**(4) + cIFBA53(16)*E**(3) + cIFBA53(17)*E**(2) + cIFBA53(18)*E + cIFBA53(19))*B**(2) + (cIFBA53(20)*E**(4) + cIFBA53(21)*E**(3) + cIFBA53(22)*E**(2) + cIFBA53(23)*E + cIFBA53(24))*B + (cIFBA53(25)*E**(4) + cIFBA53(26)*E**(3) + cIFBA53(27)*E**(2) + cIFBA53(28)*E + cIFBA53(29))

end function get_REMOV1_IFBA_DIFF_2

double precision function get_DIFF2_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA54 = (/ & 
     7.28361571570e-05, &
    -1.03425940584e-03, &
     5.22654175150e-03, &
    -1.10936592056e-02, &
     1.20617817243e-02, &
    -7.28413561697e-06, &
     1.29658076600e-04, &
    -9.29755345665e-04, &
     3.62622176524e-03, &
    -4.61085101471e-03  &
    /)
	get_DIFF2_IFBA_DIFF_0 =  (cIFBA54(0)*E**(4) + cIFBA54(1)*E**(3) + cIFBA54(2)*E**(2) + cIFBA54(3)*E + cIFBA54(4))*B + (cIFBA54(5)*E**(4) + cIFBA54(6)*E**(3) + cIFBA54(7)*E**(2) + cIFBA54(8)*E + cIFBA54(9))

end function get_DIFF2_IFBA_DIFF_0

double precision function get_DIFF2_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA55 = (/ & 
    -5.34010191429e-10, &
     9.18153418662e-09, &
    -5.88161874287e-08, &
     1.66088471652e-07, &
    -1.72660697626e-07, &
     3.75684011650e-08, &
    -6.46367131405e-07, &
     4.15591007989e-06, &
    -1.18283446626e-05, &
     1.23967871173e-05, &
    -8.31495856754e-07, &
     1.43572942758e-05, &
    -9.31356682465e-05, &
     2.69719073156e-04, &
    -2.87302743593e-04, &
     5.52042723867e-06, &
    -9.67352400165e-05, &
     6.45923485883e-04, &
    -1.97951507374e-03, &
     2.20424858339e-03, &
    -1.38445950380e-06, &
     3.46074477966e-05, &
    -3.68169032774e-04, &
     2.18906457294e-03, &
    -3.19790391740e-03  &
    /)
	get_DIFF2_IFBA_DIFF_1 =  (cIFBA55(0)*E**(4) + cIFBA55(1)*E**(3) + cIFBA55(2)*E**(2) + cIFBA55(3)*E + cIFBA55(4))*B**(4) + (cIFBA55(5)*E**(4) + cIFBA55(6)*E**(3) + cIFBA55(7)*E**(2) + cIFBA55(8)*E + cIFBA55(9))*B**(3) + (cIFBA55(10)*E**(4) + cIFBA55(11)*E**(3) + cIFBA55(12)*E**(2) + cIFBA55(13)*E + cIFBA55(14))*B**(2) + (cIFBA55(15)*E**(4) + cIFBA55(16)*E**(3) + cIFBA55(17)*E**(2) + cIFBA55(18)*E + cIFBA55(19))*B + (cIFBA55(20)*E**(4) + cIFBA55(21)*E**(3) + cIFBA55(22)*E**(2) + cIFBA55(23)*E + cIFBA55(24))

end function get_DIFF2_IFBA_DIFF_1

double precision function get_DIFF2_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA56 = (/ & 
    -4.28111536833e-10, &
     6.93820313716e-09, &
    -4.16293163393e-08, &
     1.09558669389e-07, &
    -1.06708057514e-07, &
     8.49579567821e-08, &
    -1.37664153623e-06, &
     8.25800190392e-06, &
    -2.17278642950e-05, &
     2.11562573424e-05, &
    -6.21158743866e-06, &
     1.00653233425e-04, &
    -6.03759831602e-04, &
     1.58850933939e-03, &
    -1.54656095271e-03, &
     1.98314076746e-04, &
    -3.21442401855e-03, &
     1.92856178374e-02, &
    -5.07537553761e-02, &
     4.94216215604e-02, &
    -2.32636160474e-03, &
     3.77280983167e-02, &
    -2.26457013513e-01, &
     5.96284432734e-01, &
    -5.80889477324e-01  &
    /)
	get_DIFF2_IFBA_DIFF_2 =  (cIFBA56(0)*E**(4) + cIFBA56(1)*E**(3) + cIFBA56(2)*E**(2) + cIFBA56(3)*E + cIFBA56(4))*B**(4) + (cIFBA56(5)*E**(4) + cIFBA56(6)*E**(3) + cIFBA56(7)*E**(2) + cIFBA56(8)*E + cIFBA56(9))*B**(3) + (cIFBA56(10)*E**(4) + cIFBA56(11)*E**(3) + cIFBA56(12)*E**(2) + cIFBA56(13)*E + cIFBA56(14))*B**(2) + (cIFBA56(15)*E**(4) + cIFBA56(16)*E**(3) + cIFBA56(17)*E**(2) + cIFBA56(18)*E + cIFBA56(19))*B + (cIFBA56(20)*E**(4) + cIFBA56(21)*E**(3) + cIFBA56(22)*E**(2) + cIFBA56(23)*E + cIFBA56(24))

end function get_DIFF2_IFBA_DIFF_2

double precision function get_DIFF1_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA57 = (/ & 
     5.46319362745e-04, &
    -9.05676860956e-03, &
     5.58630702854e-02, &
    -1.51267222272e-01, &
     1.31391840962e-01, &
     1.27465895194e-04, &
    -2.03216983046e-03, &
     1.22462051107e-02, &
    -3.44573992975e-02, &
     4.98974013335e-02  &
    /)
	get_DIFF1_IFBA_DIFF_0 =  (cIFBA57(0)*E**(4) + cIFBA57(1)*E**(3) + cIFBA57(2)*E**(2) + cIFBA57(3)*E + cIFBA57(4))*B + (cIFBA57(5)*E**(4) + cIFBA57(6)*E**(3) + cIFBA57(7)*E**(2) + cIFBA57(8)*E + cIFBA57(9))

end function get_DIFF1_IFBA_DIFF_0

double precision function get_DIFF1_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA58 = (/ & 
    -3.68047285501e-10, &
     5.98549783191e-09, &
    -3.62861188908e-08, &
     9.77033190308e-08, &
    -9.95919938695e-08, &
     2.74735300734e-08, &
    -4.47434979241e-07, &
     2.72490650153e-06, &
    -7.41201503632e-06, &
     7.71334191815e-06, &
    -7.31999916828e-07, &
     1.19579640320e-05, &
    -7.34365427945e-05, &
     2.03408833595e-04, &
    -2.19758710260e-04, &
     8.22421563673e-06, &
    -1.35261618391e-04, &
     8.43946062006e-04, &
    -2.41831557281e-03, &
     2.81260768657e-03, &
    -3.17650982484e-05, &
     5.30188926913e-04, &
    -3.41840754816e-03, &
     1.05570860087e-02, &
    -1.47600779834e-02, &
    -1.90018056990e-05, &
     3.02041492146e-04, &
    -1.62610430518e-03, &
     2.04706361742e-03, &
     1.35404955707e-02  &
    /)
	get_DIFF1_IFBA_DIFF_1 =  (cIFBA58(0)*E**(4) + cIFBA58(1)*E**(3) + cIFBA58(2)*E**(2) + cIFBA58(3)*E + cIFBA58(4))*B**(5) + (cIFBA58(5)*E**(4) + cIFBA58(6)*E**(3) + cIFBA58(7)*E**(2) + cIFBA58(8)*E + cIFBA58(9))*B**(4) + (cIFBA58(10)*E**(4) + cIFBA58(11)*E**(3) + cIFBA58(12)*E**(2) + cIFBA58(13)*E + cIFBA58(14))*B**(3) + (cIFBA58(15)*E**(4) + cIFBA58(16)*E**(3) + cIFBA58(17)*E**(2) + cIFBA58(18)*E + cIFBA58(19))*B**(2) + (cIFBA58(20)*E**(4) + cIFBA58(21)*E**(3) + cIFBA58(22)*E**(2) + cIFBA58(23)*E + cIFBA58(24))*B + (cIFBA58(25)*E**(4) + cIFBA58(26)*E**(3) + cIFBA58(27)*E**(2) + cIFBA58(28)*E + cIFBA58(29))

end function get_DIFF1_IFBA_DIFF_1

double precision function get_DIFF1_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA59 = (/ & 
     3.59621191479e-10, &
    -5.66305584406e-09, &
     3.31021288783e-08, &
    -8.51138391543e-08, &
     8.12557019771e-08, &
    -9.92221725284e-08, &
     1.56568314972e-06, &
    -9.17034727520e-06, &
     2.36248407334e-05, &
    -2.25936138683e-05, &
     1.07635841596e-05, &
    -1.70175751780e-04, &
     9.98643839799e-04, &
    -2.57743202419e-03, &
     2.46902473992e-03, &
    -5.73080669306e-04, &
     9.07737204601e-03, &
    -5.33656477830e-02, &
     1.37972258172e-01, &
    -1.32378367546e-01, &
     1.49467010134e-02, &
    -2.37162754418e-01, &
     1.39666061057e+00, &
    -3.61686502105e+00, &
     3.47545070025e+00, &
    -1.52491068655e-01, &
     2.42352101504e+00, &
    -1.42948516291e+01, &
     3.70751516571e+01, &
    -3.56754061663e+01  &
    /)
	get_DIFF1_IFBA_DIFF_2 =  (cIFBA59(0)*E**(4) + cIFBA59(1)*E**(3) + cIFBA59(2)*E**(2) + cIFBA59(3)*E + cIFBA59(4))*B**(5) + (cIFBA59(5)*E**(4) + cIFBA59(6)*E**(3) + cIFBA59(7)*E**(2) + cIFBA59(8)*E + cIFBA59(9))*B**(4) + (cIFBA59(10)*E**(4) + cIFBA59(11)*E**(3) + cIFBA59(12)*E**(2) + cIFBA59(13)*E + cIFBA59(14))*B**(3) + (cIFBA59(15)*E**(4) + cIFBA59(16)*E**(3) + cIFBA59(17)*E**(2) + cIFBA59(18)*E + cIFBA59(19))*B**(2) + (cIFBA59(20)*E**(4) + cIFBA59(21)*E**(3) + cIFBA59(22)*E**(2) + cIFBA59(23)*E + cIFBA59(24))*B + (cIFBA59(25)*E**(4) + cIFBA59(26)*E**(3) + cIFBA59(27)*E**(2) + cIFBA59(28)*E + cIFBA59(29))

end function get_DIFF1_IFBA_DIFF_2

double precision function get_BOR1_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA60 = (/ & 
     5.46370928026e-04, &
    -7.84353194853e-03, &
     3.50637973408e-02, &
    -7.76223747303e-02, &
     7.15616467484e-01, &
    -4.73485085648e-04, &
     8.63928115187e-03, &
    -6.50247890446e-02, &
     2.70254136545e-01, &
    -6.16036418602e-01  &
    /)
	get_BOR1_XE_IFBA_DIFF_0 =  (cIFBA60(0)*E**(4) + cIFBA60(1)*E**(3) + cIFBA60(2)*E**(2) + cIFBA60(3)*E + cIFBA60(4))*B + (cIFBA60(5)*E**(4) + cIFBA60(6)*E**(3) + cIFBA60(7)*E**(2) + cIFBA60(8)*E + cIFBA60(9))

end function get_BOR1_XE_IFBA_DIFF_0

double precision function get_BOR1_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA61 = (/ & 
     1.55895370694e-09, &
    -2.73843529773e-08, &
     1.83362225301e-07, &
    -5.59000881632e-07, &
     6.62128159376e-07, &
    -1.38663480056e-07, &
     2.46852505272e-06, &
    -1.68190139668e-05, &
     5.24590313104e-05, &
    -6.40478982435e-05, &
     4.40774232120e-06, &
    -7.97765824105e-05, &
     5.56306007155e-04, &
    -1.79290466381e-03, &
     2.29375219635e-03, &
    -5.96719126563e-05, &
     1.10215304982e-03, &
    -7.93606509596e-03, &
     2.68952552197e-02, &
    -3.72316187765e-02, &
     3.07480021198e-04, &
    -5.80363498005e-03, &
     4.37384366124e-02, &
    -1.61710033821e-01, &
     2.61688843014e-01, &
    -2.53337302052e-04, &
     4.83084585336e-03, &
    -4.04891003274e-02, &
     1.99119987379e-01, &
    -5.25831041842e-01  &
    /)
	get_BOR1_XE_IFBA_DIFF_1 =  (cIFBA61(0)*E**(4) + cIFBA61(1)*E**(3) + cIFBA61(2)*E**(2) + cIFBA61(3)*E + cIFBA61(4))*B**(5) + (cIFBA61(5)*E**(4) + cIFBA61(6)*E**(3) + cIFBA61(7)*E**(2) + cIFBA61(8)*E + cIFBA61(9))*B**(4) + (cIFBA61(10)*E**(4) + cIFBA61(11)*E**(3) + cIFBA61(12)*E**(2) + cIFBA61(13)*E + cIFBA61(14))*B**(3) + (cIFBA61(15)*E**(4) + cIFBA61(16)*E**(3) + cIFBA61(17)*E**(2) + cIFBA61(18)*E + cIFBA61(19))*B**(2) + (cIFBA61(20)*E**(4) + cIFBA61(21)*E**(3) + cIFBA61(22)*E**(2) + cIFBA61(23)*E + cIFBA61(24))*B + (cIFBA61(25)*E**(4) + cIFBA61(26)*E**(3) + cIFBA61(27)*E**(2) + cIFBA61(28)*E + cIFBA61(29))

end function get_BOR1_XE_IFBA_DIFF_1

double precision function get_BOR1_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:14) :: cIFBA62 = (/ & 
    -2.93828296355e-07, &
     4.98582645734e-06, &
    -3.08161766673e-05, &
     8.06603258447e-05, &
    -7.38805354035e-05, &
     3.11886140285e-05, &
    -5.40818593759e-04, &
     3.41826296566e-03, &
    -9.10675940185e-03, &
     8.22048216942e-03, &
    -7.39798288424e-04, &
     1.33130976172e-02, &
    -8.77483400052e-02, &
     2.44709303653e-01, &
    -2.17050623628e-01  &
    /)
	get_BOR1_XE_IFBA_DIFF_2 =  (cIFBA62(0)*E**(4) + cIFBA62(1)*E**(3) + cIFBA62(2)*E**(2) + cIFBA62(3)*E + cIFBA62(4))*B**(2) + (cIFBA62(5)*E**(4) + cIFBA62(6)*E**(3) + cIFBA62(7)*E**(2) + cIFBA62(8)*E + cIFBA62(9))*B + (cIFBA62(10)*E**(4) + cIFBA62(11)*E**(3) + cIFBA62(12)*E**(2) + cIFBA62(13)*E + cIFBA62(14))

end function get_BOR1_XE_IFBA_DIFF_2

double precision function get_XE_YIELD_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA63 = (/ & 
     1.09265535029e-05, &
    -1.74339247712e-04, &
     1.04676404752e-03, &
    -2.84791739432e-03, &
     3.15393566150e-03, &
    -1.82116295314e-06, &
     2.84101706236e-05, &
    -1.66075767906e-04, &
     4.38927043468e-04, &
    -4.80743551238e-04  &
    /)
	get_XE_YIELD_IFBA_DIFF_0 =  (cIFBA63(0)*E**(4) + cIFBA63(1)*E**(3) + cIFBA63(2)*E**(2) + cIFBA63(3)*E + cIFBA63(4))*B + (cIFBA63(5)*E**(4) + cIFBA63(6)*E**(3) + cIFBA63(7)*E**(2) + cIFBA63(8)*E + cIFBA63(9))

end function get_XE_YIELD_IFBA_DIFF_0

double precision function get_XE_YIELD_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA64 = (/ & 
     1.28755834315e-11, &
    -2.11417509758e-10, &
     1.49536292393e-09, &
    -5.86198881119e-09, &
     1.10802938666e-08, &
    -6.03309642330e-10, &
     9.75501660967e-09, &
    -7.19162929199e-08, &
     3.15327735041e-07, &
    -6.85295493262e-07, &
     1.29592843184e-09, &
    -3.14206135447e-09, &
     1.48903963296e-07, &
    -2.57528341093e-06, &
     1.06003889883e-05, &
     1.61037066916e-07, &
    -3.06232799819e-06, &
     2.06687957884e-05, &
    -5.25757988711e-05, &
     9.44120621015e-06, &
    -6.04313555494e-07, &
     1.09453753275e-05, &
    -7.69622385135e-05, &
     2.57444588440e-04, &
    -3.79271817990e-04  &
    /)
	get_XE_YIELD_IFBA_DIFF_1 =  (cIFBA64(0)*E**(4) + cIFBA64(1)*E**(3) + cIFBA64(2)*E**(2) + cIFBA64(3)*E + cIFBA64(4))*B**(4) + (cIFBA64(5)*E**(4) + cIFBA64(6)*E**(3) + cIFBA64(7)*E**(2) + cIFBA64(8)*E + cIFBA64(9))*B**(3) + (cIFBA64(10)*E**(4) + cIFBA64(11)*E**(3) + cIFBA64(12)*E**(2) + cIFBA64(13)*E + cIFBA64(14))*B**(2) + (cIFBA64(15)*E**(4) + cIFBA64(16)*E**(3) + cIFBA64(17)*E**(2) + cIFBA64(18)*E + cIFBA64(19))*B + (cIFBA64(20)*E**(4) + cIFBA64(21)*E**(3) + cIFBA64(22)*E**(2) + cIFBA64(23)*E + cIFBA64(24))

end function get_XE_YIELD_IFBA_DIFF_1

double precision function get_XE_YIELD_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA65 = (/ & 
    -9.24495268177e-13, &
     1.96361301014e-11, &
    -1.46895370391e-10, &
     4.58307672646e-10, &
    -5.07400424531e-10, &
    -4.74294336888e-13, &
    -1.06877765473e-09, &
     1.31112656034e-08, &
    -5.14680799345e-08, &
     6.59847568961e-08, &
     1.62570129618e-08, &
    -1.75653778899e-07, &
     5.16695166636e-07, &
     7.34999031279e-09, &
    -1.38947029861e-06, &
    -1.08451554776e-06, &
     1.45324414666e-05, &
    -6.92758871818e-05, &
     1.38341609870e-04, &
    -8.76655956034e-05, &
     1.95711564459e-05, &
    -2.77375476454e-04, &
     1.44261775212e-03, &
    -3.30858494999e-03, &
     2.72060317289e-03  &
    /)
	get_XE_YIELD_IFBA_DIFF_2 =  (cIFBA65(0)*E**(4) + cIFBA65(1)*E**(3) + cIFBA65(2)*E**(2) + cIFBA65(3)*E + cIFBA65(4))*B**(4) + (cIFBA65(5)*E**(4) + cIFBA65(6)*E**(3) + cIFBA65(7)*E**(2) + cIFBA65(8)*E + cIFBA65(9))*B**(3) + (cIFBA65(10)*E**(4) + cIFBA65(11)*E**(3) + cIFBA65(12)*E**(2) + cIFBA65(13)*E + cIFBA65(14))*B**(2) + (cIFBA65(15)*E**(4) + cIFBA65(16)*E**(3) + cIFBA65(17)*E**(2) + cIFBA65(18)*E + cIFBA65(19))*B + (cIFBA65(20)*E**(4) + cIFBA65(21)*E**(3) + cIFBA65(22)*E**(2) + cIFBA65(23)*E + cIFBA65(24))

end function get_XE_YIELD_IFBA_DIFF_2

double precision function get_NU_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA66 = (/ & 
    -2.36746590612e-03, &
     3.77580493690e-02, &
    -2.23073611643e-01, &
     5.76769326762e-01, &
    -5.39313110175e-01, &
     2.54956658462e-04, &
    -4.00160647254e-03, &
     2.31049153661e-02, &
    -5.75091665303e-02, &
     4.84856366119e-02  &
    /)
	get_NU_IFBA_DIFF_0 =  (cIFBA66(0)*E**(4) + cIFBA66(1)*E**(3) + cIFBA66(2)*E**(2) + cIFBA66(3)*E + cIFBA66(4))*B + (cIFBA66(5)*E**(4) + cIFBA66(6)*E**(3) + cIFBA66(7)*E**(2) + cIFBA66(8)*E + cIFBA66(9))

end function get_NU_IFBA_DIFF_0

double precision function get_NU_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA67 = (/ & 
     9.17913901995e-10, &
    -1.44329989439e-08, &
     8.37128272690e-08, &
    -2.10241014103e-07, &
     1.87593256249e-07, &
    -7.13813314835e-08, &
     1.11959882440e-06, &
    -6.46933795179e-06, &
     1.61304004714e-05, &
    -1.41288210588e-05, &
     1.94894815630e-06, &
    -3.05005155453e-05, &
     1.75555508421e-04, &
    -4.33872932100e-04, &
     3.69865459109e-04, &
    -2.24638954621e-05, &
     3.51740476149e-04, &
    -2.02359378240e-03, &
     4.97403869824e-03, &
    -4.10973983451e-03, &
     1.03034526970e-04, &
    -1.63273839043e-03, &
     9.55668239972e-03, &
    -2.40856980172e-02, &
     2.04579756633e-02, &
    -1.61279442374e-04, &
     2.59885993543e-03, &
    -1.57570386145e-02, &
     4.32606463311e-02, &
    -4.86996660042e-02  &
    /)
	get_NU_IFBA_DIFF_1 =  (cIFBA67(0)*E**(4) + cIFBA67(1)*E**(3) + cIFBA67(2)*E**(2) + cIFBA67(3)*E + cIFBA67(4))*B**(5) + (cIFBA67(5)*E**(4) + cIFBA67(6)*E**(3) + cIFBA67(7)*E**(2) + cIFBA67(8)*E + cIFBA67(9))*B**(4) + (cIFBA67(10)*E**(4) + cIFBA67(11)*E**(3) + cIFBA67(12)*E**(2) + cIFBA67(13)*E + cIFBA67(14))*B**(3) + (cIFBA67(15)*E**(4) + cIFBA67(16)*E**(3) + cIFBA67(17)*E**(2) + cIFBA67(18)*E + cIFBA67(19))*B**(2) + (cIFBA67(20)*E**(4) + cIFBA67(21)*E**(3) + cIFBA67(22)*E**(2) + cIFBA67(23)*E + cIFBA67(24))*B + (cIFBA67(25)*E**(4) + cIFBA67(26)*E**(3) + cIFBA67(27)*E**(2) + cIFBA67(28)*E + cIFBA67(29))

end function get_NU_IFBA_DIFF_1

double precision function get_NU_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA68 = (/ & 
    -4.08040503348e-10, &
     6.53763139445e-09, &
    -3.88741058142e-08, &
     1.01704174144e-07, &
    -9.87880363555e-08, &
     1.02356915801e-07, &
    -1.64046529116e-06, &
     9.75764436783e-06, &
    -2.55369441196e-05, &
     2.48135083347e-05, &
    -1.00609130906e-05, &
     1.61323099993e-04, &
    -9.60044240766e-04, &
     2.51384695247e-03, &
    -2.44389213552e-03, &
     4.83992847954e-04, &
    -7.76633549760e-03, &
     4.62532790112e-02, &
    -1.21207881035e-01, &
     1.17924050436e-01, &
    -1.13964084709e-02, &
     1.83063847314e-01, &
    -1.09147715194e+00, &
     2.86359393318e+00, &
    -2.78913139863e+00, &
     1.05142865042e-01, &
    -1.69128149506e+00, &
     1.00990607385e+01, &
    -2.65394335263e+01, &
     2.58922273844e+01  &
    /)
	get_NU_IFBA_DIFF_2 =  (cIFBA68(0)*E**(4) + cIFBA68(1)*E**(3) + cIFBA68(2)*E**(2) + cIFBA68(3)*E + cIFBA68(4))*B**(5) + (cIFBA68(5)*E**(4) + cIFBA68(6)*E**(3) + cIFBA68(7)*E**(2) + cIFBA68(8)*E + cIFBA68(9))*B**(4) + (cIFBA68(10)*E**(4) + cIFBA68(11)*E**(3) + cIFBA68(12)*E**(2) + cIFBA68(13)*E + cIFBA68(14))*B**(3) + (cIFBA68(15)*E**(4) + cIFBA68(16)*E**(3) + cIFBA68(17)*E**(2) + cIFBA68(18)*E + cIFBA68(19))*B**(2) + (cIFBA68(20)*E**(4) + cIFBA68(21)*E**(3) + cIFBA68(22)*E**(2) + cIFBA68(23)*E + cIFBA68(24))*B + (cIFBA68(25)*E**(4) + cIFBA68(26)*E**(3) + cIFBA68(27)*E**(2) + cIFBA68(28)*E + cIFBA68(29))

end function get_NU_IFBA_DIFF_2

double precision function get_BOR2_NO_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA69 = (/ & 
     1.81780368046e-07, &
    -4.43818220568e-06, &
     4.14529947879e-05, &
    -1.68037842742e-04, &
    -2.98962833473e-06, &
    -9.83630880490e-13, &
    -2.42849495811e-07, &
     5.28317189758e-06, &
    -5.18217978560e-05, &
     2.59178774046e-04  &
    /)
	get_BOR2_NO_XE_IFBA_DIFF_0 =  (cIFBA69(0)*E**(4) + cIFBA69(1)*E**(3) + cIFBA69(2)*E**(2) + cIFBA69(3)*E + cIFBA69(4))*B + (cIFBA69(5)*E**(4) + cIFBA69(6)*E**(3) + cIFBA69(7)*E**(2) + cIFBA69(8)*E + cIFBA69(9))

end function get_BOR2_NO_XE_IFBA_DIFF_0

double precision function get_BOR2_NO_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA70 = (/ & 
    -7.54012057948e-13, &
     1.32082207212e-11, &
    -8.75192757057e-11, &
     2.59720917542e-10, &
    -2.90323285535e-10, &
     7.52151438141e-11, &
    -1.32348783786e-09, &
     8.83600484137e-09, &
    -2.65920170018e-08, &
     3.05174583060e-08, &
    -2.69309932944e-09, &
     4.76794874783e-08, &
    -3.21823214559e-07, &
     9.89030719024e-07, &
    -1.18211862388e-06, &
     4.03362322746e-08, &
    -7.20783369689e-07, &
     4.95100049513e-06, &
    -1.57617471399e-05, &
     2.02700835817e-05, &
    -2.16256450492e-07, &
     3.92105554512e-06, &
    -2.77783911537e-05, &
     9.49209305673e-05, &
    -1.44254443987e-04, &
     1.57379860248e-07, &
    -2.86042082655e-06, &
     2.15503633989e-05, &
    -9.67530946538e-05, &
     3.02570060072e-04  &
    /)
	get_BOR2_NO_XE_IFBA_DIFF_1 =  (cIFBA70(0)*E**(4) + cIFBA70(1)*E**(3) + cIFBA70(2)*E**(2) + cIFBA70(3)*E + cIFBA70(4))*B**(5) + (cIFBA70(5)*E**(4) + cIFBA70(6)*E**(3) + cIFBA70(7)*E**(2) + cIFBA70(8)*E + cIFBA70(9))*B**(4) + (cIFBA70(10)*E**(4) + cIFBA70(11)*E**(3) + cIFBA70(12)*E**(2) + cIFBA70(13)*E + cIFBA70(14))*B**(3) + (cIFBA70(15)*E**(4) + cIFBA70(16)*E**(3) + cIFBA70(17)*E**(2) + cIFBA70(18)*E + cIFBA70(19))*B**(2) + (cIFBA70(20)*E**(4) + cIFBA70(21)*E**(3) + cIFBA70(22)*E**(2) + cIFBA70(23)*E + cIFBA70(24))*B + (cIFBA70(25)*E**(4) + cIFBA70(26)*E**(3) + cIFBA70(27)*E**(2) + cIFBA70(28)*E + cIFBA70(29))

end function get_BOR2_NO_XE_IFBA_DIFF_1

double precision function get_BOR2_NO_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA71 = (/ & 
     6.07325451758e-13, &
    -9.62185857690e-12, &
     5.65939363849e-11, &
    -1.46477621888e-10, &
     1.40717433039e-10, &
    -1.54736445725e-10, &
     2.45064252720e-09, &
    -1.44087731414e-08, &
     3.72800499979e-08, &
    -3.58038506440e-08, &
     1.55172796197e-08, &
    -2.45654531756e-07, &
     1.44370350182e-06, &
    -3.73378640850e-06, &
     3.58477014169e-06, &
    -7.65346075276e-07, &
     1.21103726942e-05, &
    -7.11343182829e-05, &
     1.83879695984e-04, &
    -1.76469116357e-04, &
     1.85624755633e-05, &
    -2.93564507394e-04, &
     1.72329559720e-03, &
    -4.45198650504e-03, &
     4.27024447462e-03, &
    -1.77129915332e-04, &
     2.79986624704e-03, &
    -1.64259459693e-02, &
     4.24090443665e-02, &
    -4.06459637415e-02  &
    /)
	get_BOR2_NO_XE_IFBA_DIFF_2 =  (cIFBA71(0)*E**(4) + cIFBA71(1)*E**(3) + cIFBA71(2)*E**(2) + cIFBA71(3)*E + cIFBA71(4))*B**(5) + (cIFBA71(5)*E**(4) + cIFBA71(6)*E**(3) + cIFBA71(7)*E**(2) + cIFBA71(8)*E + cIFBA71(9))*B**(4) + (cIFBA71(10)*E**(4) + cIFBA71(11)*E**(3) + cIFBA71(12)*E**(2) + cIFBA71(13)*E + cIFBA71(14))*B**(3) + (cIFBA71(15)*E**(4) + cIFBA71(16)*E**(3) + cIFBA71(17)*E**(2) + cIFBA71(18)*E + cIFBA71(19))*B**(2) + (cIFBA71(20)*E**(4) + cIFBA71(21)*E**(3) + cIFBA71(22)*E**(2) + cIFBA71(23)*E + cIFBA71(24))*B + (cIFBA71(25)*E**(4) + cIFBA71(26)*E**(3) + cIFBA71(27)*E**(2) + cIFBA71(28)*E + cIFBA71(29))

end function get_BOR2_NO_XE_IFBA_DIFF_2

double precision function get_KAPPA_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA72 = (/ & 
     9.10523059210e-15, &
    -1.45926493888e-13, &
     8.69367435451e-13, &
    -2.28623393180e-12, &
     2.26751587296e-12, &
     2.97156650715e-20, &
     4.80424690040e-17, &
    -5.79406020039e-16, &
     3.28705850610e-15, &
    -1.19600539452e-14  &
    /)
	get_KAPPA_IFBA_DIFF_0 =  (cIFBA72(0)*E**(4) + cIFBA72(1)*E**(3) + cIFBA72(2)*E**(2) + cIFBA72(3)*E + cIFBA72(4))*B + (cIFBA72(5)*E**(4) + cIFBA72(6)*E**(3) + cIFBA72(7)*E**(2) + cIFBA72(8)*E + cIFBA72(9))

end function get_KAPPA_IFBA_DIFF_0

double precision function get_KAPPA_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA73 = (/ & 
    -7.18940914802e-22, &
     1.80811449073e-20, &
    -1.47969016292e-19, &
     5.01736097651e-19, &
    -6.23062503049e-19, &
     1.45976610421e-19, &
    -2.84507707986e-18, &
     2.00832679404e-17, &
    -6.17334049476e-17, &
     7.13346897599e-17, &
    -7.04805241495e-18, &
     1.26171945244e-16, &
    -8.35038817778e-16, &
     2.44076650239e-15, &
    -2.71067695417e-15, &
     1.18678289081e-16, &
    -2.03452447189e-15, &
     1.29828468765e-14, &
    -3.68231201683e-14, &
     4.00382706434e-14, &
    -6.80732927048e-16, &
     1.12929973444e-14, &
    -6.98756008867e-14, &
     1.92557967295e-13, &
    -2.05115213437e-13, &
     1.35425286931e-15, &
    -2.16753251101e-14, &
     1.28564395271e-13, &
    -3.33423829053e-13, &
     3.12534393755e-13  &
    /)
	get_KAPPA_IFBA_DIFF_1 =  (cIFBA73(0)*E**(4) + cIFBA73(1)*E**(3) + cIFBA73(2)*E**(2) + cIFBA73(3)*E + cIFBA73(4))*B**(5) + (cIFBA73(5)*E**(4) + cIFBA73(6)*E**(3) + cIFBA73(7)*E**(2) + cIFBA73(8)*E + cIFBA73(9))*B**(4) + (cIFBA73(10)*E**(4) + cIFBA73(11)*E**(3) + cIFBA73(12)*E**(2) + cIFBA73(13)*E + cIFBA73(14))*B**(3) + (cIFBA73(15)*E**(4) + cIFBA73(16)*E**(3) + cIFBA73(17)*E**(2) + cIFBA73(18)*E + cIFBA73(19))*B**(2) + (cIFBA73(20)*E**(4) + cIFBA73(21)*E**(3) + cIFBA73(22)*E**(2) + cIFBA73(23)*E + cIFBA73(24))*B + (cIFBA73(25)*E**(4) + cIFBA73(26)*E**(3) + cIFBA73(27)*E**(2) + cIFBA73(28)*E + cIFBA73(29))

end function get_KAPPA_IFBA_DIFF_1

double precision function get_KAPPA_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA74 = (/ & 
     8.68604110368e-22, &
    -1.32889617660e-20, &
     7.49927881313e-20, &
    -1.84892917703e-19, &
     1.67905808740e-19, &
    -2.08550098717e-19, &
     3.18838845783e-18, &
    -1.79771935453e-17, &
     4.42724187825e-17, &
    -4.01418792354e-17, &
     1.91484353932e-17, &
    -2.92135753462e-16, &
     1.64302176782e-15, &
    -4.03376912446e-15, &
     3.64308364920e-15, &
    -8.32153249697e-16, &
     1.26424151460e-14, &
    -7.07432714859e-14, &
     1.72603644189e-13, &
    -1.54683525529e-13, &
     1.68975763403e-14, &
    -2.54749221638e-13, &
     1.41210032557e-12, &
    -3.40494753495e-12, &
     3.00663168645e-12, &
    -1.27457369369e-13, &
     1.89669271197e-12, &
    -1.03414986331e-11, &
     2.44068243027e-11, &
    -2.09534468696e-11  &
    /)
	get_KAPPA_IFBA_DIFF_2 =  (cIFBA74(0)*E**(4) + cIFBA74(1)*E**(3) + cIFBA74(2)*E**(2) + cIFBA74(3)*E + cIFBA74(4))*B**(5) + (cIFBA74(5)*E**(4) + cIFBA74(6)*E**(3) + cIFBA74(7)*E**(2) + cIFBA74(8)*E + cIFBA74(9))*B**(4) + (cIFBA74(10)*E**(4) + cIFBA74(11)*E**(3) + cIFBA74(12)*E**(2) + cIFBA74(13)*E + cIFBA74(14))*B**(3) + (cIFBA74(15)*E**(4) + cIFBA74(16)*E**(3) + cIFBA74(17)*E**(2) + cIFBA74(18)*E + cIFBA74(19))*B**(2) + (cIFBA74(20)*E**(4) + cIFBA74(21)*E**(3) + cIFBA74(22)*E**(2) + cIFBA74(23)*E + cIFBA74(24))*B + (cIFBA74(25)*E**(4) + cIFBA74(26)*E**(3) + cIFBA74(27)*E**(2) + cIFBA74(28)*E + cIFBA74(29))

end function get_KAPPA_IFBA_DIFF_2

double precision function get_ABS1_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA75 = (/ & 
    -4.00606446720e-06, &
     6.67679543569e-05, &
    -4.16997827797e-04, &
     1.17204434699e-03, &
    -8.57340147361e-04, &
     1.64386068348e-07, &
    -1.70993700602e-06, &
     3.77990951415e-07, &
     7.65150044494e-05, &
    -9.37066570083e-04  &
    /)
	get_ABS1_IFBA_DIFF_0 =  (cIFBA75(0)*E**(4) + cIFBA75(1)*E**(3) + cIFBA75(2)*E**(2) + cIFBA75(3)*E + cIFBA75(4))*B + (cIFBA75(5)*E**(4) + cIFBA75(6)*E**(3) + cIFBA75(7)*E**(2) + cIFBA75(8)*E + cIFBA75(9))

end function get_ABS1_IFBA_DIFF_0

double precision function get_ABS1_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA76 = (/ & 
    -5.70247495951e-12, &
     8.30837705311e-11, &
    -4.26366493204e-10, &
     8.52083861477e-10, &
    -4.06216887981e-10, &
     3.46465262497e-10, &
    -4.86467905452e-09, &
     2.30572807497e-08, &
    -3.62145715239e-08, &
    -5.99978766386e-09, &
    -5.68860243471e-09, &
     7.05831218298e-08, &
    -2.33138630136e-07, &
    -2.14575279852e-07, &
     1.76025474854e-06, &
     5.73044365733e-09, &
     1.68615350353e-07, &
    -3.57263176754e-06, &
     2.20143896926e-05, &
    -4.95070415595e-05, &
     2.77657956991e-07, &
    -5.57820011362e-06, &
     4.60634700325e-05, &
    -1.97847625866e-04, &
     4.36472444403e-04, &
    -2.45544817069e-07, &
     3.94079204269e-06, &
    -2.63552077565e-05, &
     1.23174282538e-04, &
    -9.47154978994e-04  &
    /)
	get_ABS1_IFBA_DIFF_1 =  (cIFBA76(0)*E**(4) + cIFBA76(1)*E**(3) + cIFBA76(2)*E**(2) + cIFBA76(3)*E + cIFBA76(4))*B**(5) + (cIFBA76(5)*E**(4) + cIFBA76(6)*E**(3) + cIFBA76(7)*E**(2) + cIFBA76(8)*E + cIFBA76(9))*B**(4) + (cIFBA76(10)*E**(4) + cIFBA76(11)*E**(3) + cIFBA76(12)*E**(2) + cIFBA76(13)*E + cIFBA76(14))*B**(3) + (cIFBA76(15)*E**(4) + cIFBA76(16)*E**(3) + cIFBA76(17)*E**(2) + cIFBA76(18)*E + cIFBA76(19))*B**(2) + (cIFBA76(20)*E**(4) + cIFBA76(21)*E**(3) + cIFBA76(22)*E**(2) + cIFBA76(23)*E + cIFBA76(24))*B + (cIFBA76(25)*E**(4) + cIFBA76(26)*E**(3) + cIFBA76(27)*E**(2) + cIFBA76(28)*E + cIFBA76(29))

end function get_ABS1_IFBA_DIFF_1

double precision function get_ABS1_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA77 = (/ & 
     1.00459533234e-12, &
    -1.59671698226e-11, &
     9.49317564029e-11, &
    -2.49841587266e-10, &
     2.44869116380e-10, &
    -2.55264661378e-10, &
     4.04963201779e-09, &
    -2.40356130070e-08, &
     6.31592470322e-08, &
    -6.18162835891e-08, &
     2.57517728558e-08, &
    -4.07503019732e-07, &
     2.41280682719e-06, &
    -6.32595636181e-06, &
     6.17886460002e-06, &
    -1.28739585519e-06, &
     2.03066308971e-05, &
    -1.19859761139e-04, &
     3.13321362731e-04, &
    -3.05224433478e-04, &
     3.18491057704e-05, &
    -5.00413949132e-04, &
     2.94239018192e-03, &
    -7.66327986016e-03, &
     7.44116247197e-03, &
    -3.10909742817e-04, &
     4.86258994264e-03, &
    -2.84606824177e-02, &
     7.37868157868e-02, &
    -7.13808340299e-02  &
    /)
	get_ABS1_IFBA_DIFF_2 =  (cIFBA77(0)*E**(4) + cIFBA77(1)*E**(3) + cIFBA77(2)*E**(2) + cIFBA77(3)*E + cIFBA77(4))*B**(5) + (cIFBA77(5)*E**(4) + cIFBA77(6)*E**(3) + cIFBA77(7)*E**(2) + cIFBA77(8)*E + cIFBA77(9))*B**(4) + (cIFBA77(10)*E**(4) + cIFBA77(11)*E**(3) + cIFBA77(12)*E**(2) + cIFBA77(13)*E + cIFBA77(14))*B**(3) + (cIFBA77(15)*E**(4) + cIFBA77(16)*E**(3) + cIFBA77(17)*E**(2) + cIFBA77(18)*E + cIFBA77(19))*B**(2) + (cIFBA77(20)*E**(4) + cIFBA77(21)*E**(3) + cIFBA77(22)*E**(2) + cIFBA77(23)*E + cIFBA77(24))*B + (cIFBA77(25)*E**(4) + cIFBA77(26)*E**(3) + cIFBA77(27)*E**(2) + cIFBA77(28)*E + cIFBA77(29))

end function get_ABS1_IFBA_DIFF_2

double precision function get_ABS2_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA78 = (/ & 
     1.36590170952e-04, &
    -2.07059230270e-03, &
     1.10005417004e-02, &
    -2.08656694358e-02, &
     3.44758607100e-02, &
     3.09581531634e-06, &
    -4.69350917742e-05, &
     2.06196255246e-04, &
     5.21072262673e-04, &
    -1.92799721309e-02  &
    /)
	get_ABS2_IFBA_DIFF_0 =  (cIFBA78(0)*E**(4) + cIFBA78(1)*E**(3) + cIFBA78(2)*E**(2) + cIFBA78(3)*E + cIFBA78(4))*B + (cIFBA78(5)*E**(4) + cIFBA78(6)*E**(3) + cIFBA78(7)*E**(2) + cIFBA78(8)*E + cIFBA78(9))

end function get_ABS2_IFBA_DIFF_0

double precision function get_ABS2_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA79 = (/ & 
     4.42382815390e-11, &
    -8.56955430603e-10, &
     6.27867466522e-09, &
    -2.05251267140e-08, &
     2.47928458610e-08, &
    -3.07002569175e-09, &
     6.26720294667e-08, &
    -4.84160263848e-07, &
     1.67437745017e-06, &
    -2.16090260015e-06, &
     6.70001737028e-08, &
    -1.51790609361e-06, &
     1.28834502133e-05, &
    -4.89209664634e-05, &
     7.04703802690e-05, &
    -3.96888828734e-07, &
     1.25470372267e-05, &
    -1.32297261925e-04, &
     6.01037600368e-04, &
    -1.06067711054e-03, &
    -1.69860931963e-06, &
    -2.60304173639e-06, &
     3.29299738219e-04, &
    -2.54816382315e-03, &
     7.23240899138e-03, &
     1.01073533489e-05, &
    -1.59893556625e-04, &
     8.73671988235e-04, &
    -1.16818160237e-03, &
    -1.76390528772e-02  &
    /)
	get_ABS2_IFBA_DIFF_1 =  (cIFBA79(0)*E**(4) + cIFBA79(1)*E**(3) + cIFBA79(2)*E**(2) + cIFBA79(3)*E + cIFBA79(4))*B**(5) + (cIFBA79(5)*E**(4) + cIFBA79(6)*E**(3) + cIFBA79(7)*E**(2) + cIFBA79(8)*E + cIFBA79(9))*B**(4) + (cIFBA79(10)*E**(4) + cIFBA79(11)*E**(3) + cIFBA79(12)*E**(2) + cIFBA79(13)*E + cIFBA79(14))*B**(3) + (cIFBA79(15)*E**(4) + cIFBA79(16)*E**(3) + cIFBA79(17)*E**(2) + cIFBA79(18)*E + cIFBA79(19))*B**(2) + (cIFBA79(20)*E**(4) + cIFBA79(21)*E**(3) + cIFBA79(22)*E**(2) + cIFBA79(23)*E + cIFBA79(24))*B + (cIFBA79(25)*E**(4) + cIFBA79(26)*E**(3) + cIFBA79(27)*E**(2) + cIFBA79(28)*E + cIFBA79(29))

end function get_ABS2_IFBA_DIFF_1

double precision function get_ABS2_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA80 = (/ & 
     2.22558340517e-10, &
    -3.58718998897e-09, &
     2.14193870205e-08, &
    -5.63194842397e-08, &
     5.49783594359e-08, &
    -4.59306225595e-08, &
     7.40287468934e-07, &
    -4.41997064325e-06, &
     1.16251074761e-05, &
    -1.13550300022e-05, &
     3.52064390247e-06, &
    -5.67390961197e-05, &
     3.38668075124e-04, &
    -8.90695122552e-04, &
     8.70012655757e-04, &
    -1.18717034233e-04, &
     1.91355281247e-03, &
    -1.14192709064e-02, &
     3.00279453598e-02, &
    -2.93047787763e-02, &
     1.48365521603e-03, &
    -2.39348713799e-02, &
     1.42911626487e-01, &
    -3.76166201274e-01, &
     3.66330236975e-01  &
    /)
	get_ABS2_IFBA_DIFF_2 =  (cIFBA80(0)*E**(4) + cIFBA80(1)*E**(3) + cIFBA80(2)*E**(2) + cIFBA80(3)*E + cIFBA80(4))*B**(4) + (cIFBA80(5)*E**(4) + cIFBA80(6)*E**(3) + cIFBA80(7)*E**(2) + cIFBA80(8)*E + cIFBA80(9))*B**(3) + (cIFBA80(10)*E**(4) + cIFBA80(11)*E**(3) + cIFBA80(12)*E**(2) + cIFBA80(13)*E + cIFBA80(14))*B**(2) + (cIFBA80(15)*E**(4) + cIFBA80(16)*E**(3) + cIFBA80(17)*E**(2) + cIFBA80(18)*E + cIFBA80(19))*B + (cIFBA80(20)*E**(4) + cIFBA80(21)*E**(3) + cIFBA80(22)*E**(2) + cIFBA80(23)*E + cIFBA80(24))

end function get_ABS2_IFBA_DIFF_2

double precision function get_XE2_MAC_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA81 = (/ & 
    -1.18369442232e-05, &
     2.47739006403e-04, &
    -2.52885115376e-03, &
     1.67631873036e-02, &
    -8.81957301163e-04, &
     1.82176927955e-24, &
    -6.19280680306e-23, &
     8.22252703191e-22, &
    -6.45305356130e-21, &
     3.77819823685e-20  &
    /)
	get_XE2_MAC_IFBA_DIFF_0 =  (cIFBA81(0)*E**(4) + cIFBA81(1)*E**(3) + cIFBA81(2)*E**(2) + cIFBA81(3)*E + cIFBA81(4))*B + (cIFBA81(5)*E**(4) + cIFBA81(6)*E**(3) + cIFBA81(7)*E**(2) + cIFBA81(8)*E + cIFBA81(9))

end function get_XE2_MAC_IFBA_DIFF_0

double precision function get_XE2_MAC_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA82 = (/ & 
    -3.14756168331e-13, &
     5.91880960791e-12, &
    -4.65818432272e-11, &
     1.85079347095e-10, &
    -3.31684324781e-10, &
     1.70609554447e-11, &
    -3.44137644453e-10, &
     3.04520596400e-09, &
    -1.38148767980e-08, &
     2.77912233661e-08, &
    -1.45083313690e-10, &
     4.08157195626e-09, &
    -5.39087249260e-08, &
     3.32865578188e-07, &
    -8.24153930380e-07, &
    -4.11054968734e-09, &
     5.69641569838e-08, &
    -3.04131245868e-08, &
    -2.44148235779e-06, &
     1.00859157186e-05, &
     5.20371473915e-08, &
    -9.71238874605e-07, &
     5.70774213523e-06, &
    -6.73647643741e-06, &
    -3.93513439518e-05, &
    -1.02823332616e-08, &
     5.61709220158e-07, &
    -6.93128536582e-06, &
     4.83563625427e-05, &
    -5.87948593998e-05  &
    /)
	get_XE2_MAC_IFBA_DIFF_1 =  (cIFBA82(0)*E**(4) + cIFBA82(1)*E**(3) + cIFBA82(2)*E**(2) + cIFBA82(3)*E + cIFBA82(4))*B**(5) + (cIFBA82(5)*E**(4) + cIFBA82(6)*E**(3) + cIFBA82(7)*E**(2) + cIFBA82(8)*E + cIFBA82(9))*B**(4) + (cIFBA82(10)*E**(4) + cIFBA82(11)*E**(3) + cIFBA82(12)*E**(2) + cIFBA82(13)*E + cIFBA82(14))*B**(3) + (cIFBA82(15)*E**(4) + cIFBA82(16)*E**(3) + cIFBA82(17)*E**(2) + cIFBA82(18)*E + cIFBA82(19))*B**(2) + (cIFBA82(20)*E**(4) + cIFBA82(21)*E**(3) + cIFBA82(22)*E**(2) + cIFBA82(23)*E + cIFBA82(24))*B + (cIFBA82(25)*E**(4) + cIFBA82(26)*E**(3) + cIFBA82(27)*E**(2) + cIFBA82(28)*E + cIFBA82(29))

end function get_XE2_MAC_IFBA_DIFF_1

double precision function get_XE2_MAC_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cIFBA83 = (/ & 
     3.67653881721e-11, &
    -6.26425856985e-10, &
     3.92659432976e-09, &
    -1.06437548387e-08, &
     1.05112829897e-08, &
    -5.71266036876e-09, &
     9.77423273933e-08, &
    -6.17182237272e-07, &
     1.69203747753e-06, &
    -1.69868838545e-06, &
     2.77246796674e-07, &
    -4.75328596168e-06, &
     3.01701596096e-05, &
    -8.36254145899e-05, &
     8.60249610172e-05, &
    -4.25334141296e-06, &
     7.26267451999e-05, &
    -4.58839272292e-04, &
     1.27037176236e-03, &
    -1.35675684920e-03  &
    /)
	get_XE2_MAC_IFBA_DIFF_2 =  (cIFBA83(0)*E**(4) + cIFBA83(1)*E**(3) + cIFBA83(2)*E**(2) + cIFBA83(3)*E + cIFBA83(4))*B**(3) + (cIFBA83(5)*E**(4) + cIFBA83(6)*E**(3) + cIFBA83(7)*E**(2) + cIFBA83(8)*E + cIFBA83(9))*B**(2) + (cIFBA83(10)*E**(4) + cIFBA83(11)*E**(3) + cIFBA83(12)*E**(2) + cIFBA83(13)*E + cIFBA83(14))*B + (cIFBA83(15)*E**(4) + cIFBA83(16)*E**(3) + cIFBA83(17)*E**(2) + cIFBA83(18)*E + cIFBA83(19))

end function get_XE2_MAC_IFBA_DIFF_2

double precision function get_K_INF_NO_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA84 = (/ & 
    -5.46323939687e-05, &
     9.10540555425e-04, &
    -6.11374120090e-03, &
     2.33684886144e-02, &
    -7.39544528861e-02, &
    -4.00685050639e-05, &
     7.55703819547e-04, &
    -3.79310621040e-03, &
    -1.61384516136e-02, &
     2.80477723103e-01  &
    /)
	get_K_INF_NO_XE_IFBA_DIFF_0 =  (cIFBA84(0)*E**(4) + cIFBA84(1)*E**(3) + cIFBA84(2)*E**(2) + cIFBA84(3)*E + cIFBA84(4))*B + (cIFBA84(5)*E**(4) + cIFBA84(6)*E**(3) + cIFBA84(7)*E**(2) + cIFBA84(8)*E + cIFBA84(9))

end function get_K_INF_NO_XE_IFBA_DIFF_0

double precision function get_K_INF_NO_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA85 = (/ & 
     5.87026264785e-09, &
    -1.20886988892e-07, &
     9.97177775496e-07, &
    -3.96804341056e-06, &
     6.58005109567e-06, &
    -3.65984306352e-07, &
     7.69866924105e-06, &
    -6.55740541627e-05, &
     2.73367496782e-04, &
    -4.85285924466e-04, &
     6.47672414644e-06, &
    -1.42061025360e-04, &
     1.28880143795e-03, &
    -5.87780380773e-03, &
     1.19014766326e-02, &
    -1.93113701408e-05, &
     5.20561480204e-04, &
    -6.08504270210e-03, &
     3.69099273579e-02, &
    -1.06269635283e-01, &
    -1.42884663601e-04, &
     2.67352172887e-03, &
    -1.72788071497e-02, &
     2.65154069969e-02, &
     2.28631724753e-01  &
    /)
	get_K_INF_NO_XE_IFBA_DIFF_1 =  (cIFBA85(0)*E**(4) + cIFBA85(1)*E**(3) + cIFBA85(2)*E**(2) + cIFBA85(3)*E + cIFBA85(4))*B**(4) + (cIFBA85(5)*E**(4) + cIFBA85(6)*E**(3) + cIFBA85(7)*E**(2) + cIFBA85(8)*E + cIFBA85(9))*B**(3) + (cIFBA85(10)*E**(4) + cIFBA85(11)*E**(3) + cIFBA85(12)*E**(2) + cIFBA85(13)*E + cIFBA85(14))*B**(2) + (cIFBA85(15)*E**(4) + cIFBA85(16)*E**(3) + cIFBA85(17)*E**(2) + cIFBA85(18)*E + cIFBA85(19))*B + (cIFBA85(20)*E**(4) + cIFBA85(21)*E**(3) + cIFBA85(22)*E**(2) + cIFBA85(23)*E + cIFBA85(24))

end function get_K_INF_NO_XE_IFBA_DIFF_1

double precision function get_K_INF_NO_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA86 = (/ & 
     3.93546027347e-10, &
    -6.37284580893e-09, &
     3.87368697705e-08, &
    -1.03601531819e-07, &
     1.03060184588e-07, &
    -7.67836115039e-08, &
     1.23707932486e-06, &
    -7.50020642492e-06, &
     2.00250392097e-05, &
    -1.99248782596e-05, &
     5.46198384024e-06, &
    -8.73305307156e-05, &
     5.26905042287e-04, &
    -1.40026008430e-03, &
     1.38928547856e-03, &
    -1.68669817923e-04, &
     2.67051905740e-03, &
    -1.60017457488e-02, &
     4.21578927968e-02, &
    -4.14252627958e-02, &
     1.92369346070e-03, &
    -3.01785823494e-02, &
     1.79927970674e-01, &
    -4.69562768892e-01, &
     4.51391944808e-01  &
    /)
	get_K_INF_NO_XE_IFBA_DIFF_2 =  (cIFBA86(0)*E**(4) + cIFBA86(1)*E**(3) + cIFBA86(2)*E**(2) + cIFBA86(3)*E + cIFBA86(4))*B**(4) + (cIFBA86(5)*E**(4) + cIFBA86(6)*E**(3) + cIFBA86(7)*E**(2) + cIFBA86(8)*E + cIFBA86(9))*B**(3) + (cIFBA86(10)*E**(4) + cIFBA86(11)*E**(3) + cIFBA86(12)*E**(2) + cIFBA86(13)*E + cIFBA86(14))*B**(2) + (cIFBA86(15)*E**(4) + cIFBA86(16)*E**(3) + cIFBA86(17)*E**(2) + cIFBA86(18)*E + cIFBA86(19))*B + (cIFBA86(20)*E**(4) + cIFBA86(21)*E**(3) + cIFBA86(22)*E**(2) + cIFBA86(23)*E + cIFBA86(24))

end function get_K_INF_NO_XE_IFBA_DIFF_2

double precision function get_XE2_MIC_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA87 = (/ & 
     3.64248749227e+02, &
    -7.13916952974e+03, &
     5.53204236962e+04, &
    -2.05333729642e+05, &
     8.33708100390e+04, &
     1.27480575000e+02, &
    -2.14409919318e+03, &
     1.42464416995e+04, &
    -5.15836414273e+04, &
     1.54594904035e+05  &
    /)
	get_XE2_MIC_IFBA_DIFF_0 =  (cIFBA87(0)*E**(4) + cIFBA87(1)*E**(3) + cIFBA87(2)*E**(2) + cIFBA87(3)*E + cIFBA87(4))*B + (cIFBA87(5)*E**(4) + cIFBA87(6)*E**(3) + cIFBA87(7)*E**(2) + cIFBA87(8)*E + cIFBA87(9))

end function get_XE2_MIC_IFBA_DIFF_0

double precision function get_XE2_MIC_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA88 = (/ & 
    -4.24352823025e-04, &
     7.43342949470e-03, &
    -4.92288974105e-02, &
     1.46810051230e-01, &
    -1.66186528204e-01, &
     3.87580016178e-02, &
    -6.86066704898e-01, &
     4.60588662022e+00, &
    -1.40063381373e+01, &
     1.63582834886e+01, &
    -1.21617625720e+00, &
     2.18562991687e+01, &
    -1.49843587936e+02, &
     4.70461341135e+02, &
    -5.79823880572e+02, &
     1.45392128201e+01, &
    -2.68293307444e+02, &
     1.91231757673e+03, &
    -6.39088933792e+03, &
     8.80118843995e+03, &
    -4.22476438209e+01, &
     8.49105362566e+02, &
    -6.84696598812e+03, &
     2.76542149242e+04, &
    -5.26580515404e+04, &
    -6.48040364399e+01, &
     1.05715211451e+03, &
    -5.52983169999e+03, &
     1.96611672691e+03, &
     9.99820296980e+04  &
    /)
	get_XE2_MIC_IFBA_DIFF_1 =  (cIFBA88(0)*E**(4) + cIFBA88(1)*E**(3) + cIFBA88(2)*E**(2) + cIFBA88(3)*E + cIFBA88(4))*B**(5) + (cIFBA88(5)*E**(4) + cIFBA88(6)*E**(3) + cIFBA88(7)*E**(2) + cIFBA88(8)*E + cIFBA88(9))*B**(4) + (cIFBA88(10)*E**(4) + cIFBA88(11)*E**(3) + cIFBA88(12)*E**(2) + cIFBA88(13)*E + cIFBA88(14))*B**(3) + (cIFBA88(15)*E**(4) + cIFBA88(16)*E**(3) + cIFBA88(17)*E**(2) + cIFBA88(18)*E + cIFBA88(19))*B**(2) + (cIFBA88(20)*E**(4) + cIFBA88(21)*E**(3) + cIFBA88(22)*E**(2) + cIFBA88(23)*E + cIFBA88(24))*B + (cIFBA88(25)*E**(4) + cIFBA88(26)*E**(3) + cIFBA88(27)*E**(2) + cIFBA88(28)*E + cIFBA88(29))

end function get_XE2_MIC_IFBA_DIFF_1

double precision function get_XE2_MIC_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cIFBA89 = (/ & 
    -2.57580422509e-05, &
     3.95636906402e-04, &
    -2.40300462346e-03, &
     6.79200429401e-03, &
    -7.48015375549e-03, &
     1.01999733596e-02, &
    -1.59430232716e-01, &
     9.62702858437e-01, &
    -2.64973735939e+00, &
     2.79823134960e+00, &
    -1.40969641946e+00, &
     2.22181809522e+01, &
    -1.33751286171e+02, &
     3.63206001484e+02, &
    -3.75101456004e+02, &
     8.96163812836e+01, &
    -1.41910135732e+03, &
     8.52587085560e+03, &
    -2.29621236041e+04, &
     2.33866911747e+04, &
    -2.67968771439e+03, &
     4.25559379308e+04, &
    -2.55300720388e+05, &
     6.83761295676e+05, &
    -6.89921241603e+05, &
     3.04172996591e+04, &
    -4.83915968891e+05, &
     2.89963313818e+06, &
    -7.73439987499e+06, &
     7.75642286224e+06  &
    /)
	get_XE2_MIC_IFBA_DIFF_2 =  (cIFBA89(0)*E**(4) + cIFBA89(1)*E**(3) + cIFBA89(2)*E**(2) + cIFBA89(3)*E + cIFBA89(4))*B**(5) + (cIFBA89(5)*E**(4) + cIFBA89(6)*E**(3) + cIFBA89(7)*E**(2) + cIFBA89(8)*E + cIFBA89(9))*B**(4) + (cIFBA89(10)*E**(4) + cIFBA89(11)*E**(3) + cIFBA89(12)*E**(2) + cIFBA89(13)*E + cIFBA89(14))*B**(3) + (cIFBA89(15)*E**(4) + cIFBA89(16)*E**(3) + cIFBA89(17)*E**(2) + cIFBA89(18)*E + cIFBA89(19))*B**(2) + (cIFBA89(20)*E**(4) + cIFBA89(21)*E**(3) + cIFBA89(22)*E**(2) + cIFBA89(23)*E + cIFBA89(24))*B + (cIFBA89(25)*E**(4) + cIFBA89(26)*E**(3) + cIFBA89(27)*E**(2) + cIFBA89(28)*E + cIFBA89(29))

end function get_XE2_MIC_IFBA_DIFF_2

double precision function get_SM2_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA90 = (/ & 
     2.02505231410e-03, &
    -1.80005688143e+01, &
     3.57127414075e+02, &
    -2.43480800440e+03, &
    -8.24360652724e+02, &
     1.45690750313e+00, &
    -2.52044593182e+01, &
     1.78353763509e+02, &
    -7.79681351405e+02, &
     3.48846198870e+03  &
    /)
	get_SM2_XE_IFBA_DIFF_0 =  (cIFBA90(0)*E**(4) + cIFBA90(1)*E**(3) + cIFBA90(2)*E**(2) + cIFBA90(3)*E + cIFBA90(4))*B + (cIFBA90(5)*E**(4) + cIFBA90(6)*E**(3) + cIFBA90(7)*E**(2) + cIFBA90(8)*E + cIFBA90(9))

end function get_SM2_XE_IFBA_DIFF_0

double precision function get_SM2_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA91 = (/ & 
     2.54950405849e-05, &
    -6.85593636100e-04, &
     6.72376777846e-03, &
    -2.90984821068e-02, &
     4.89483369067e-02, &
    -1.56040008329e-03, &
     4.36844456883e-02, &
    -4.46502763945e-01, &
     2.03105479066e+00, &
    -3.67155972361e+00, &
     2.92737529206e-02, &
    -8.52685531459e-01, &
     9.22238632574e+00, &
    -4.54016927712e+01, &
     9.32188678280e+01, &
    -1.33679785634e-01, &
     4.31324900710e+00, &
    -5.41615741395e+01, &
     3.23310739477e+02, &
    -9.01491955791e+02, &
    -7.09278555781e-01, &
     1.41345626007e+01, &
    -8.70721160657e+01, &
     2.24225404555e+01, &
     2.53014582524e+03  &
    /)
	get_SM2_XE_IFBA_DIFF_1 =  (cIFBA91(0)*E**(4) + cIFBA91(1)*E**(3) + cIFBA91(2)*E**(2) + cIFBA91(3)*E + cIFBA91(4))*B**(4) + (cIFBA91(5)*E**(4) + cIFBA91(6)*E**(3) + cIFBA91(7)*E**(2) + cIFBA91(8)*E + cIFBA91(9))*B**(3) + (cIFBA91(10)*E**(4) + cIFBA91(11)*E**(3) + cIFBA91(12)*E**(2) + cIFBA91(13)*E + cIFBA91(14))*B**(2) + (cIFBA91(15)*E**(4) + cIFBA91(16)*E**(3) + cIFBA91(17)*E**(2) + cIFBA91(18)*E + cIFBA91(19))*B + (cIFBA91(20)*E**(4) + cIFBA91(21)*E**(3) + cIFBA91(22)*E**(2) + cIFBA91(23)*E + cIFBA91(24))

end function get_SM2_XE_IFBA_DIFF_1

double precision function get_SM2_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA92 = (/ & 
    -1.14845488536e-05, &
     1.72946046545e-04, &
    -9.70896817037e-04, &
     2.42981152627e-03, &
    -2.29031062266e-03, &
     2.67625132977e-03, &
    -4.05769465965e-02, &
     2.29122153134e-01, &
    -5.75825935268e-01, &
     5.43996875274e-01, &
    -2.26577779567e-01, &
     3.45329656133e+00, &
    -1.95757130015e+01, &
     4.93027812472e+01, &
    -4.65735025197e+01, &
     8.26288885160e+00, &
    -1.26500613805e+02, &
     7.19222475462e+02, &
    -1.81314063481e+03, &
     1.70817448616e+03, &
    -1.09394156124e+02, &
     1.68255629561e+03, &
    -9.59974486987e+03, &
     2.42530484414e+04, &
    -2.27055118804e+04  &
    /)
	get_SM2_XE_IFBA_DIFF_2 =  (cIFBA92(0)*E**(4) + cIFBA92(1)*E**(3) + cIFBA92(2)*E**(2) + cIFBA92(3)*E + cIFBA92(4))*B**(4) + (cIFBA92(5)*E**(4) + cIFBA92(6)*E**(3) + cIFBA92(7)*E**(2) + cIFBA92(8)*E + cIFBA92(9))*B**(3) + (cIFBA92(10)*E**(4) + cIFBA92(11)*E**(3) + cIFBA92(12)*E**(2) + cIFBA92(13)*E + cIFBA92(14))*B**(2) + (cIFBA92(15)*E**(4) + cIFBA92(16)*E**(3) + cIFBA92(17)*E**(2) + cIFBA92(18)*E + cIFBA92(19))*B + (cIFBA92(20)*E**(4) + cIFBA92(21)*E**(3) + cIFBA92(22)*E**(2) + cIFBA92(23)*E + cIFBA92(24))

end function get_SM2_XE_IFBA_DIFF_2

double precision function get_K_INF_XE_IFBA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cIFBA93 = (/ & 
     3.09592562073e-04, &
    -6.76243584932e-03, &
     5.87207330155e-02, &
    -2.21750299752e-01, &
    -9.54409127934e-02, &
    -4.00685050639e-05, &
     7.55703819547e-04, &
    -3.79310621040e-03, &
    -1.61384516136e-02, &
     2.80477723103e-01  &
    /)
	get_K_INF_XE_IFBA_DIFF_0 =  (cIFBA93(0)*E**(4) + cIFBA93(1)*E**(3) + cIFBA93(2)*E**(2) + cIFBA93(3)*E + cIFBA93(4))*B + (cIFBA93(5)*E**(4) + cIFBA93(6)*E**(3) + cIFBA93(7)*E**(2) + cIFBA93(8)*E + cIFBA93(9))

end function get_K_INF_XE_IFBA_DIFF_0

double precision function get_K_INF_XE_IFBA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA94 = (/ & 
     5.84567052338e-09, &
    -1.19372402635e-07, &
     9.75237271846e-07, &
    -3.84332449962e-06, &
     6.31932119035e-06, &
    -3.69925575284e-07, &
     7.68692638322e-06, &
    -6.45916471566e-05, &
     2.65734908862e-04, &
    -4.66485038334e-04, &
     6.76080584167e-06, &
    -1.45209814193e-04, &
     1.28802275638e-03, &
    -5.75243927698e-03, &
     1.14553628448e-02, &
    -2.42999977721e-05, &
     5.98161846735e-04, &
    -6.45142189533e-03, &
     3.69152030132e-02, &
    -1.02571817870e-01, &
    -1.17271998064e-04, &
     2.19113214357e-03, &
    -1.39043812115e-02, &
     1.73675065385e-02, &
     2.24369511520e-01  &
    /)
	get_K_INF_XE_IFBA_DIFF_1 =  (cIFBA94(0)*E**(4) + cIFBA94(1)*E**(3) + cIFBA94(2)*E**(2) + cIFBA94(3)*E + cIFBA94(4))*B**(4) + (cIFBA94(5)*E**(4) + cIFBA94(6)*E**(3) + cIFBA94(7)*E**(2) + cIFBA94(8)*E + cIFBA94(9))*B**(3) + (cIFBA94(10)*E**(4) + cIFBA94(11)*E**(3) + cIFBA94(12)*E**(2) + cIFBA94(13)*E + cIFBA94(14))*B**(2) + (cIFBA94(15)*E**(4) + cIFBA94(16)*E**(3) + cIFBA94(17)*E**(2) + cIFBA94(18)*E + cIFBA94(19))*B + (cIFBA94(20)*E**(4) + cIFBA94(21)*E**(3) + cIFBA94(22)*E**(2) + cIFBA94(23)*E + cIFBA94(24))

end function get_K_INF_XE_IFBA_DIFF_1

double precision function get_K_INF_XE_IFBA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cIFBA95 = (/ & 
    -1.42087478374e-10, &
     2.14984358552e-09, &
    -1.15433775473e-08, &
     2.66115761378e-08, &
    -2.18219297318e-08, &
     2.85914939659e-08, &
    -4.37089775299e-07, &
     2.36195663171e-06, &
    -5.47499749299e-06, &
     4.49094375956e-06, &
    -2.08031682898e-06, &
     3.22736955667e-05, &
    -1.76311417981e-04, &
     4.14229080415e-04, &
    -3.44243751807e-04, &
     6.39216913917e-05, &
    -1.00977729222e-03, &
     5.58795140917e-03, &
    -1.34071152737e-02, &
     1.15106665185e-02, &
    -6.85949445675e-04, &
     1.10241443450e-02, &
    -6.12461646963e-02, &
     1.49458093537e-01, &
    -1.36326713782e-01  &
    /)
	get_K_INF_XE_IFBA_DIFF_2 =  (cIFBA95(0)*E**(4) + cIFBA95(1)*E**(3) + cIFBA95(2)*E**(2) + cIFBA95(3)*E + cIFBA95(4))*B**(4) + (cIFBA95(5)*E**(4) + cIFBA95(6)*E**(3) + cIFBA95(7)*E**(2) + cIFBA95(8)*E + cIFBA95(9))*B**(3) + (cIFBA95(10)*E**(4) + cIFBA95(11)*E**(3) + cIFBA95(12)*E**(2) + cIFBA95(13)*E + cIFBA95(14))*B**(2) + (cIFBA95(15)*E**(4) + cIFBA95(16)*E**(3) + cIFBA95(17)*E**(2) + cIFBA95(18)*E + cIFBA95(19))*B + (cIFBA95(20)*E**(4) + cIFBA95(21)*E**(3) + cIFBA95(22)*E**(2) + cIFBA95(23)*E + cIFBA95(24))

end function get_K_INF_XE_IFBA_DIFF_2


end module nuclear_data_IFBA
