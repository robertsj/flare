module nuclear_data_WABA

contains

double precision function get_SM2_NO_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA24 = (/ & 
    -4.49810502987e-07, &
     8.83425439828e-06, &
    -6.65638745515e-05, &
     1.98676617087e-04, &
     6.64380326169e-04, &
     9.10521334203e-25, &
    -1.45926209261e-23, &
     8.69365693357e-23, &
    -2.43622924732e-22, &
     4.94751120635e-22  &
    /)
	get_SM2_NO_XE_WABA_DIFF_0 =  (cWABA24(0)*E**(4) + cWABA24(1)*E**(3) + cWABA24(2)*E**(2) + cWABA24(3)*E + cWABA24(4))*B + (cWABA24(5)*E**(4) + cWABA24(6)*E**(3) + cWABA24(7)*E**(2) + cWABA24(8)*E + cWABA24(9))

end function get_SM2_NO_XE_WABA_DIFF_0

double precision function get_SM2_NO_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA25 = (/ & 
    -2.14015643292e-11, &
     2.77800238927e-10, &
    -1.18140000813e-09, &
     2.80465015279e-09, &
     4.11604392506e-11, &
     6.64994008430e-10, &
    -7.91777407110e-09, &
     2.56911669016e-08, &
    -4.87870991492e-08, &
    -1.03233439636e-07, &
    -6.10419996558e-09, &
     6.40021737725e-08, &
    -8.61732692836e-08, &
    -1.53715386129e-08, &
     2.75836504060e-06, &
     1.95858224468e-08, &
    -2.01586281073e-07, &
    -4.14232461984e-10, &
    -5.60745540717e-07, &
    -1.80426916576e-05, &
    -5.30982246217e-09, &
     1.39222153462e-08, &
     8.42698614416e-07, &
    -3.68992582895e-06, &
     8.20588946933e-07  &
    /)
	get_SM2_NO_XE_WABA_DIFF_1 =  (cWABA25(0)*E**(4) + cWABA25(1)*E**(3) + cWABA25(2)*E**(2) + cWABA25(3)*E + cWABA25(4))*B**(4) + (cWABA25(5)*E**(4) + cWABA25(6)*E**(3) + cWABA25(7)*E**(2) + cWABA25(8)*E + cWABA25(9))*B**(3) + (cWABA25(10)*E**(4) + cWABA25(11)*E**(3) + cWABA25(12)*E**(2) + cWABA25(13)*E + cWABA25(14))*B**(2) + (cWABA25(15)*E**(4) + cWABA25(16)*E**(3) + cWABA25(17)*E**(2) + cWABA25(18)*E + cWABA25(19))*B + (cWABA25(20)*E**(4) + cWABA25(21)*E**(3) + cWABA25(22)*E**(2) + cWABA25(23)*E + cWABA25(24))

end function get_SM2_NO_XE_WABA_DIFF_1

double precision function get_SM2_NO_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA26 = (/ & 
    -7.69771032697e-14, &
     1.49536816251e-12, &
    -7.71115497502e-12, &
     7.69699483740e-12, &
     1.22213578859e-12, &
     3.72079251091e-11, &
    -6.61966107672e-10, &
     3.77238032001e-09, &
    -7.34726681607e-09, &
     5.10788292634e-09, &
    -4.32027292995e-09, &
     7.56168801977e-08, &
    -4.48861172018e-07, &
     1.00472265528e-06, &
    -8.13254873210e-07, &
     1.74387281307e-07, &
    -3.05157452873e-06, &
     1.86300554536e-05, &
    -4.45414393147e-05, &
     3.78965806757e-05, &
    -2.12421406859e-06, &
     3.72774694363e-05, &
    -2.31952479568e-04, &
     5.65430974562e-04, &
    -5.32309602533e-04  &
    /)
	get_SM2_NO_XE_WABA_DIFF_2 =  (cWABA26(0)*E**(4) + cWABA26(1)*E**(3) + cWABA26(2)*E**(2) + cWABA26(3)*E + cWABA26(4))*B**(4) + (cWABA26(5)*E**(4) + cWABA26(6)*E**(3) + cWABA26(7)*E**(2) + cWABA26(8)*E + cWABA26(9))*B**(3) + (cWABA26(10)*E**(4) + cWABA26(11)*E**(3) + cWABA26(12)*E**(2) + cWABA26(13)*E + cWABA26(14))*B**(2) + (cWABA26(15)*E**(4) + cWABA26(16)*E**(3) + cWABA26(17)*E**(2) + cWABA26(18)*E + cWABA26(19))*B + (cWABA26(20)*E**(4) + cWABA26(21)*E**(3) + cWABA26(22)*E**(2) + cWABA26(23)*E + cWABA26(24))

end function get_SM2_NO_XE_WABA_DIFF_2

double precision function get_M2_NO_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA27 = (/ & 
     1.82102217694e-02, &
    -2.98647893816e-01, &
     1.88294265276e+00, &
    -5.28734536505e+00, &
     1.52163967125e+00, &
     2.00312101313e-03, &
    -4.23694781105e-02, &
     3.70949180853e-01, &
    -1.78203723578e+00, &
     4.68058588326e+00  &
    /)
	get_M2_NO_XE_WABA_DIFF_0 =  (cWABA27(0)*E**(4) + cWABA27(1)*E**(3) + cWABA27(2)*E**(2) + cWABA27(3)*E + cWABA27(4))*B + (cWABA27(5)*E**(4) + cWABA27(6)*E**(3) + cWABA27(7)*E**(2) + cWABA27(8)*E + cWABA27(9))

end function get_M2_NO_XE_WABA_DIFF_0

double precision function get_M2_NO_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA28 = (/ & 
     1.27689972590e-07, &
    -2.22474891845e-06, &
     1.43503664139e-05, &
    -4.04267814283e-05, &
     4.11528620810e-05, &
    -6.41193028038e-06, &
     1.12682008225e-04, &
    -7.35028993413e-04, &
     2.10420233462e-03, &
    -2.19806607822e-03, &
     1.04481806090e-04, &
    -1.85383759449e-03, &
     1.22291561251e-02, &
    -3.55502498891e-02, &
     3.80640964174e-02, &
    -5.25676898970e-04, &
     9.36358386701e-03, &
    -6.13783738575e-02, &
     1.74464018378e-01, &
    -1.75422867013e-01, &
    -4.89716891229e-04, &
     1.02171223894e-02, &
    -9.27434926568e-02, &
     4.39891020670e-01, &
    -9.99277176473e-01, &
     2.96882019996e-03, &
    -5.69483299319e-02, &
     4.51664656216e-01, &
    -1.96859332540e+00, &
     4.76732552507e+00  &
    /)
	get_M2_NO_XE_WABA_DIFF_1 =  (cWABA28(0)*E**(4) + cWABA28(1)*E**(3) + cWABA28(2)*E**(2) + cWABA28(3)*E + cWABA28(4))*B**(5) + (cWABA28(5)*E**(4) + cWABA28(6)*E**(3) + cWABA28(7)*E**(2) + cWABA28(8)*E + cWABA28(9))*B**(4) + (cWABA28(10)*E**(4) + cWABA28(11)*E**(3) + cWABA28(12)*E**(2) + cWABA28(13)*E + cWABA28(14))*B**(3) + (cWABA28(15)*E**(4) + cWABA28(16)*E**(3) + cWABA28(17)*E**(2) + cWABA28(18)*E + cWABA28(19))*B**(2) + (cWABA28(20)*E**(4) + cWABA28(21)*E**(3) + cWABA28(22)*E**(2) + cWABA28(23)*E + cWABA28(24))*B + (cWABA28(25)*E**(4) + cWABA28(26)*E**(3) + cWABA28(27)*E**(2) + cWABA28(28)*E + cWABA28(29))

end function get_M2_NO_XE_WABA_DIFF_1

double precision function get_M2_NO_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA29 = (/ & 
     7.31398811181e-09, &
    -1.28500574207e-07, &
     8.02403869322e-07, &
    -2.01925761074e-06, &
     1.83636100906e-06, &
    -1.51925036227e-06, &
     2.65811476691e-05, &
    -1.66058647827e-04, &
     4.19545739388e-04, &
    -3.83433200078e-04, &
     1.12478545945e-04, &
    -1.96184459920e-03, &
     1.22693403902e-02, &
    -3.10759053521e-02, &
     2.84720017873e-02, &
    -3.45503390600e-03, &
     6.02101519118e-02, &
    -3.77595784408e-01, &
     9.56815753818e-01, &
    -8.73359783583e-01, &
     3.62299018586e-02, &
    -6.34107112330e-01, &
     4.00763502507e+00, &
    -1.01428690538e+01, &
     7.90744624875e+00  &
    /)
	get_M2_NO_XE_WABA_DIFF_2 =  (cWABA29(0)*E**(4) + cWABA29(1)*E**(3) + cWABA29(2)*E**(2) + cWABA29(3)*E + cWABA29(4))*B**(4) + (cWABA29(5)*E**(4) + cWABA29(6)*E**(3) + cWABA29(7)*E**(2) + cWABA29(8)*E + cWABA29(9))*B**(3) + (cWABA29(10)*E**(4) + cWABA29(11)*E**(3) + cWABA29(12)*E**(2) + cWABA29(13)*E + cWABA29(14))*B**(2) + (cWABA29(15)*E**(4) + cWABA29(16)*E**(3) + cWABA29(17)*E**(2) + cWABA29(18)*E + cWABA29(19))*B + (cWABA29(20)*E**(4) + cWABA29(21)*E**(3) + cWABA29(22)*E**(2) + cWABA29(23)*E + cWABA29(24))

end function get_M2_NO_XE_WABA_DIFF_2

double precision function get_M2_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA30 = (/ & 
     2.36753116611e-02, &
    -3.98472839498e-01, &
     2.52567000912e+00, &
    -6.79872186927e+00, &
     8.43894385050e-01, &
     2.00312101313e-03, &
    -4.23694781105e-02, &
     3.70949180853e-01, &
    -1.78203723578e+00, &
     4.68058588326e+00  &
    /)
	get_M2_XE_WABA_DIFF_0 =  (cWABA30(0)*E**(4) + cWABA30(1)*E**(3) + cWABA30(2)*E**(2) + cWABA30(3)*E + cWABA30(4))*B + (cWABA30(5)*E**(4) + cWABA30(6)*E**(3) + cWABA30(7)*E**(2) + cWABA30(8)*E + cWABA30(9))

end function get_M2_XE_WABA_DIFF_0

double precision function get_M2_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA31 = (/ & 
     2.93758297904e-08, &
    -5.98627921329e-07, &
     4.38327674025e-06, &
    -1.36275992971e-05, &
     1.45305111184e-05, &
    -1.14246871446e-06, &
     2.53854649725e-05, &
    -1.99092556089e-04, &
     6.60765705467e-04, &
    -7.61450471796e-04, &
     5.89970102113e-06, &
    -2.15893324813e-04, &
     2.14501675730e-03, &
    -8.31624375363e-03, &
     1.08834108500e-02, &
     2.21671307969e-04, &
    -3.12061032828e-03, &
     1.58491051398e-02, &
    -3.49352151397e-02, &
     3.41760585901e-02, &
    -2.42010013378e-03, &
     4.28116295757e-02, &
    -2.95936958586e-01, &
     9.92346292140e-01, &
    -1.54760895369e+00, &
     3.79707374148e-03, &
    -7.13518918278e-02, &
     5.42143205945e-01, &
    -2.20337232547e+00, &
     4.93793275308e+00  &
    /)
	get_M2_XE_WABA_DIFF_1 =  (cWABA31(0)*E**(4) + cWABA31(1)*E**(3) + cWABA31(2)*E**(2) + cWABA31(3)*E + cWABA31(4))*B**(5) + (cWABA31(5)*E**(4) + cWABA31(6)*E**(3) + cWABA31(7)*E**(2) + cWABA31(8)*E + cWABA31(9))*B**(4) + (cWABA31(10)*E**(4) + cWABA31(11)*E**(3) + cWABA31(12)*E**(2) + cWABA31(13)*E + cWABA31(14))*B**(3) + (cWABA31(15)*E**(4) + cWABA31(16)*E**(3) + cWABA31(17)*E**(2) + cWABA31(18)*E + cWABA31(19))*B**(2) + (cWABA31(20)*E**(4) + cWABA31(21)*E**(3) + cWABA31(22)*E**(2) + cWABA31(23)*E + cWABA31(24))*B + (cWABA31(25)*E**(4) + cWABA31(26)*E**(3) + cWABA31(27)*E**(2) + cWABA31(28)*E + cWABA31(29))

end function get_M2_XE_WABA_DIFF_1

double precision function get_M2_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA32 = (/ & 
    -9.52143376130e-10, &
     9.78507721995e-09, &
    -5.28162543382e-08, &
     2.95053746602e-07, &
    -4.75982508966e-07, &
     2.27824522354e-10, &
     1.30117639402e-06, &
    -1.05430541276e-05, &
     9.64890958355e-07, &
     3.24875955410e-05, &
     1.30929185042e-05, &
    -3.17833391032e-04, &
     2.21176894584e-03, &
    -4.15822624346e-03, &
     1.88099276163e-03, &
    -7.91606003180e-04, &
     1.64114298611e-02, &
    -1.11177112155e-01, &
     2.48022101199e-01, &
    -1.77495852204e-01, &
     1.25128624725e-02, &
    -2.46618872274e-01, &
     1.66581977959e+00, &
    -3.95694899928e+00, &
     1.88230235411e+00  &
    /)
	get_M2_XE_WABA_DIFF_2 =  (cWABA32(0)*E**(4) + cWABA32(1)*E**(3) + cWABA32(2)*E**(2) + cWABA32(3)*E + cWABA32(4))*B**(4) + (cWABA32(5)*E**(4) + cWABA32(6)*E**(3) + cWABA32(7)*E**(2) + cWABA32(8)*E + cWABA32(9))*B**(3) + (cWABA32(10)*E**(4) + cWABA32(11)*E**(3) + cWABA32(12)*E**(2) + cWABA32(13)*E + cWABA32(14))*B**(2) + (cWABA32(15)*E**(4) + cWABA32(16)*E**(3) + cWABA32(17)*E**(2) + cWABA32(18)*E + cWABA32(19))*B + (cWABA32(20)*E**(4) + cWABA32(21)*E**(3) + cWABA32(22)*E**(2) + cWABA32(23)*E + cWABA32(24))

end function get_M2_XE_WABA_DIFF_2

double precision function get_NUFISS1_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA33 = (/ & 
    -5.28107165934e-06, &
     8.83578658681e-05, &
    -5.53916220197e-04, &
     1.58817589436e-03, &
    -1.72406537402e-03, &
    -5.47560074002e-08, &
     6.26002916448e-07, &
     1.18644949321e-07, &
    -1.90198771505e-05, &
     4.18095847810e-05  &
    /)
	get_NUFISS1_WABA_DIFF_0 =  (cWABA33(0)*E**(4) + cWABA33(1)*E**(3) + cWABA33(2)*E**(2) + cWABA33(3)*E + cWABA33(4))*B + (cWABA33(5)*E**(4) + cWABA33(6)*E**(3) + cWABA33(7)*E**(2) + cWABA33(8)*E + cWABA33(9))

end function get_NUFISS1_WABA_DIFF_0

double precision function get_NUFISS1_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA34 = (/ & 
    -1.50914178262e-12, &
    -5.31983466335e-11, &
     6.28546678092e-10, &
    -2.04943697569e-09, &
     2.20992116832e-09, &
     6.06903847774e-11, &
     3.18234076103e-09, &
    -3.99769675658e-08, &
     1.50984482956e-07, &
    -1.97826542155e-07, &
    -5.36271762470e-09, &
     1.73462550549e-08, &
     3.20107640750e-07, &
    -2.08954755790e-06, &
     3.85410233769e-06, &
     6.66096241898e-08, &
    -7.43175143702e-07, &
     1.92005639559e-06, &
     4.85208313782e-06, &
    -2.66262583315e-05, &
    -6.53278290548e-09, &
    -1.54527965990e-07, &
     4.80201662700e-06, &
    -3.10460543828e-05, &
     5.59230617020e-05  &
    /)
	get_NUFISS1_WABA_DIFF_1 =  (cWABA34(0)*E**(4) + cWABA34(1)*E**(3) + cWABA34(2)*E**(2) + cWABA34(3)*E + cWABA34(4))*B**(4) + (cWABA34(5)*E**(4) + cWABA34(6)*E**(3) + cWABA34(7)*E**(2) + cWABA34(8)*E + cWABA34(9))*B**(3) + (cWABA34(10)*E**(4) + cWABA34(11)*E**(3) + cWABA34(12)*E**(2) + cWABA34(13)*E + cWABA34(14))*B**(2) + (cWABA34(15)*E**(4) + cWABA34(16)*E**(3) + cWABA34(17)*E**(2) + cWABA34(18)*E + cWABA34(19))*B + (cWABA34(20)*E**(4) + cWABA34(21)*E**(3) + cWABA34(22)*E**(2) + cWABA34(23)*E + cWABA34(24))

end function get_NUFISS1_WABA_DIFF_1

double precision function get_NUFISS1_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA35 = (/ & 
    -7.68698988925e-12, &
     1.21048607470e-10, &
    -6.99562602847e-10, &
     1.76168655441e-09, &
    -1.63929704450e-09, &
     1.46695510142e-09, &
    -2.32206835479e-08, &
     1.34907133067e-07, &
    -3.41254083371e-07, &
     3.18409122068e-07, &
    -1.00855412761e-07, &
     1.60785334240e-06, &
    -9.41452825129e-06, &
     2.39959883975e-05, &
    -2.25127405079e-05, &
     2.92069570798e-06, &
    -4.69482985524e-05, &
     2.77561787434e-04, &
    -7.15210717287e-04, &
     6.77275516531e-04, &
    -2.95008554909e-05, &
     4.77405375881e-04, &
    -2.84264534536e-03, &
     7.38437338320e-03, &
    -7.11539995494e-03  &
    /)
	get_NUFISS1_WABA_DIFF_2 =  (cWABA35(0)*E**(4) + cWABA35(1)*E**(3) + cWABA35(2)*E**(2) + cWABA35(3)*E + cWABA35(4))*B**(4) + (cWABA35(5)*E**(4) + cWABA35(6)*E**(3) + cWABA35(7)*E**(2) + cWABA35(8)*E + cWABA35(9))*B**(3) + (cWABA35(10)*E**(4) + cWABA35(11)*E**(3) + cWABA35(12)*E**(2) + cWABA35(13)*E + cWABA35(14))*B**(2) + (cWABA35(15)*E**(4) + cWABA35(16)*E**(3) + cWABA35(17)*E**(2) + cWABA35(18)*E + cWABA35(19))*B + (cWABA35(20)*E**(4) + cWABA35(21)*E**(3) + cWABA35(22)*E**(2) + cWABA35(23)*E + cWABA35(24))

end function get_NUFISS1_WABA_DIFF_2

double precision function get_K2_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA36 = (/ & 
     4.55283166406e-04, &
    -9.14204824075e-03, &
     7.29896642141e-02, &
    -2.58556208930e-01, &
    -4.55614035821e-02, &
    -2.18579411817e-05, &
     4.89101885545e-04, &
    -3.24346621934e-03, &
    -4.02867177525e-03, &
     2.12452162762e-01  &
    /)
	get_K2_WABA_DIFF_0 =  (cWABA36(0)*E**(4) + cWABA36(1)*E**(3) + cWABA36(2)*E**(2) + cWABA36(3)*E + cWABA36(4))*B + (cWABA36(5)*E**(4) + cWABA36(6)*E**(3) + cWABA36(7)*E**(2) + cWABA36(8)*E + cWABA36(9))

end function get_K2_WABA_DIFF_0

double precision function get_K2_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA37 = (/ & 
     1.48052031769e-09, &
     3.37507242347e-08, &
    -7.12769454735e-07, &
     4.00577314977e-06, &
    -7.31203856760e-06, &
    -5.57125226028e-07, &
     7.50203988689e-06, &
    -3.10225542307e-05, &
     1.90888259271e-05, &
     9.11201727241e-05, &
     1.65407758646e-05, &
    -2.69315488041e-04, &
     1.62880404662e-03, &
    -4.32454143042e-03, &
     4.34359227707e-03, &
    -1.19568171305e-04, &
     2.09545264244e-03, &
    -1.42886242813e-02, &
     4.73462826125e-02, &
    -7.56689588764e-02, &
     4.44052513813e-05, &
    -6.38735742770e-04, &
     3.89135460330e-03, &
    -2.29143573357e-02, &
     2.19941039721e-01  &
    /)
	get_K2_WABA_DIFF_1 =  (cWABA37(0)*E**(4) + cWABA37(1)*E**(3) + cWABA37(2)*E**(2) + cWABA37(3)*E + cWABA37(4))*B**(4) + (cWABA37(5)*E**(4) + cWABA37(6)*E**(3) + cWABA37(7)*E**(2) + cWABA37(8)*E + cWABA37(9))*B**(3) + (cWABA37(10)*E**(4) + cWABA37(11)*E**(3) + cWABA37(12)*E**(2) + cWABA37(13)*E + cWABA37(14))*B**(2) + (cWABA37(15)*E**(4) + cWABA37(16)*E**(3) + cWABA37(17)*E**(2) + cWABA37(18)*E + cWABA37(19))*B + (cWABA37(20)*E**(4) + cWABA37(21)*E**(3) + cWABA37(22)*E**(2) + cWABA37(23)*E + cWABA37(24))

end function get_K2_WABA_DIFF_1

double precision function get_K2_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA38 = (/ & 
    -3.66222897341e-11, &
     5.68071326811e-10, &
    -3.14126038770e-09, &
     7.15884989286e-09, &
    -5.87527665541e-09, &
     8.67831485995e-09, &
    -1.35607652922e-07, &
     7.55710768541e-07, &
    -1.73303466393e-06, &
     1.42888432973e-06, &
    -7.95152857074e-07, &
     1.25402517675e-05, &
    -7.05992887866e-05, &
     1.63348746152e-04, &
    -1.35758123566e-04, &
     3.49133859479e-05, &
    -5.56622670044e-04, &
     3.17235282807e-03, &
    -7.41902953823e-03, &
     6.23234419358e-03, &
    -7.29183401551e-04, &
     1.17649872039e-02, &
    -6.79600451059e-02, &
     1.60547292815e-01, &
    -1.36353893503e-01, &
     5.78976613623e-03, &
    -9.46863530401e-02, &
     5.55170082076e-01, &
    -1.31805239886e+00, &
     1.12390036768e+00  &
    /)
	get_K2_WABA_DIFF_2 =  (cWABA38(0)*E**(4) + cWABA38(1)*E**(3) + cWABA38(2)*E**(2) + cWABA38(3)*E + cWABA38(4))*B**(5) + (cWABA38(5)*E**(4) + cWABA38(6)*E**(3) + cWABA38(7)*E**(2) + cWABA38(8)*E + cWABA38(9))*B**(4) + (cWABA38(10)*E**(4) + cWABA38(11)*E**(3) + cWABA38(12)*E**(2) + cWABA38(13)*E + cWABA38(14))*B**(3) + (cWABA38(15)*E**(4) + cWABA38(16)*E**(3) + cWABA38(17)*E**(2) + cWABA38(18)*E + cWABA38(19))*B**(2) + (cWABA38(20)*E**(4) + cWABA38(21)*E**(3) + cWABA38(22)*E**(2) + cWABA38(23)*E + cWABA38(24))*B + (cWABA38(25)*E**(4) + cWABA38(26)*E**(3) + cWABA38(27)*E**(2) + cWABA38(28)*E + cWABA38(29))

end function get_K2_WABA_DIFF_2

double precision function get_K1_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA39 = (/ & 
    -1.27476909584e-04, &
     2.13918363385e-03, &
    -1.31126356827e-02, &
     3.44471144608e-02, &
    -4.82424681412e-02, &
    -1.09259633423e-05, &
     1.68987895502e-04, &
    -9.22290380311e-04, &
     1.28219077582e-03, &
     2.84378360923e-03  &
    /)
	get_K1_WABA_DIFF_0 =  (cWABA39(0)*E**(4) + cWABA39(1)*E**(3) + cWABA39(2)*E**(2) + cWABA39(3)*E + cWABA39(4))*B + (cWABA39(5)*E**(4) + cWABA39(6)*E**(3) + cWABA39(7)*E**(2) + cWABA39(8)*E + cWABA39(9))

end function get_K1_WABA_DIFF_0

double precision function get_K1_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA40 = (/ & 
     4.22189387030e-10, &
    -7.83457568714e-09, &
     5.40254574896e-08, &
    -1.63945906073e-07, &
     1.84129616351e-07, &
    -2.79968021341e-08, &
     5.00739274866e-07, &
    -3.34881397605e-06, &
     9.92146881838e-06, &
    -1.09583580232e-05, &
     6.17779988859e-07, &
    -1.07084525576e-05, &
     6.95455369522e-05, &
    -2.00609991247e-04, &
     2.16286215972e-04, &
    -5.52576720962e-06, &
     9.24645345059e-05, &
    -5.76658603772e-04, &
     1.58158630488e-03, &
    -1.57104218447e-03, &
     1.87158576839e-05, &
    -3.01546355414e-04, &
     1.77390564444e-03, &
    -4.32949211518e-03, &
     2.43654714128e-03, &
    -1.64568743157e-05, &
     2.67644002092e-04, &
    -1.56600768301e-03, &
     3.11385796849e-03, &
     7.82497644084e-04  &
    /)
	get_K1_WABA_DIFF_1 =  (cWABA40(0)*E**(4) + cWABA40(1)*E**(3) + cWABA40(2)*E**(2) + cWABA40(3)*E + cWABA40(4))*B**(5) + (cWABA40(5)*E**(4) + cWABA40(6)*E**(3) + cWABA40(7)*E**(2) + cWABA40(8)*E + cWABA40(9))*B**(4) + (cWABA40(10)*E**(4) + cWABA40(11)*E**(3) + cWABA40(12)*E**(2) + cWABA40(13)*E + cWABA40(14))*B**(3) + (cWABA40(15)*E**(4) + cWABA40(16)*E**(3) + cWABA40(17)*E**(2) + cWABA40(18)*E + cWABA40(19))*B**(2) + (cWABA40(20)*E**(4) + cWABA40(21)*E**(3) + cWABA40(22)*E**(2) + cWABA40(23)*E + cWABA40(24))*B + (cWABA40(25)*E**(4) + cWABA40(26)*E**(3) + cWABA40(27)*E**(2) + cWABA40(28)*E + cWABA40(29))

end function get_K1_WABA_DIFF_1

double precision function get_K1_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA41 = (/ & 
     1.96523840700e-03, &
    -2.97359373744e-02, &
     1.66540093639e-01, &
    -4.09969067306e-01, &
     3.65148904495e-01, &
     3.44853465294e-03, &
    -5.27777990716e-02, &
     2.98719855491e-01, &
    -7.39902512905e-01, &
     6.72979165470e-01, &
     2.08750945999e+00, &
    -3.10632273161e+01, &
     1.68770850700e+02, &
    -3.95620120653e+02, &
     3.37294477324e+02, &
    -3.28960186197e+00, &
     5.14672453701e+01, &
    -2.97977152991e+02, &
     7.57211618949e+02, &
    -7.14453985449e+02  &
    /)
	get_K1_WABA_DIFF_2 =  (cWABA41(0)*E**(4) + cWABA41(1)*E**(3) + cWABA41(2)*E**(2) + cWABA41(3)*E + cWABA41(4))*exp(B*(cWABA41(5)*E**(4) + cWABA41(6)*E**(3) + cWABA41(7)*E**(2) + cWABA41(8)*E + cWABA41(9))) + (cWABA41(10)*E**(4) + cWABA41(11)*E**(3) + cWABA41(12)*E**(2) + cWABA41(13)*E + cWABA41(14))*exp(B*(cWABA41(15)*E**(4) + cWABA41(16)*E**(3) + cWABA41(17)*E**(2) + cWABA41(18)*E + cWABA41(19)))

end function get_K1_WABA_DIFF_2

double precision function get_NUFISS2_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA42 = (/ & 
    -1.27475137205e-04, &
     2.12458670963e-03, &
    -1.29259894823e-02, &
     2.86826981508e-02, &
    -3.09602363175e-02, &
    -9.10483200146e-06, &
     1.34750710690e-04, &
    -7.73175487928e-04, &
     1.85825925010e-03, &
    -1.30529775100e-03  &
    /)
	get_NUFISS2_WABA_DIFF_0 =  (cWABA42(0)*E**(4) + cWABA42(1)*E**(3) + cWABA42(2)*E**(2) + cWABA42(3)*E + cWABA42(4))*B + (cWABA42(5)*E**(4) + cWABA42(6)*E**(3) + cWABA42(7)*E**(2) + cWABA42(8)*E + cWABA42(9))

end function get_NUFISS2_WABA_DIFF_0

double precision function get_NUFISS2_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA43 = (/ & 
     3.86192454068e-10, &
    -5.90196386821e-09, &
     3.32057272307e-08, &
    -8.08830624890e-08, &
     7.08043870466e-08, &
    -1.68079446288e-08, &
     2.48557380150e-07, &
    -1.33650401246e-06, &
     3.03300897056e-06, &
    -2.30264784510e-06, &
     2.53727636939e-07, &
    -3.51331649301e-06, &
     1.69589169606e-05, &
    -3.07216716031e-05, &
     8.78454884984e-06, &
    -1.83660632745e-06, &
     2.31031767988e-05, &
    -8.98103245296e-05, &
     5.43880564094e-05, &
     2.80353772009e-04, &
     8.71731984297e-06, &
    -1.15147039963e-04, &
     4.91335750832e-04, &
    -4.29490282604e-04, &
    -1.98634449318e-03, &
    -2.06541170625e-05, &
     3.23233865148e-04, &
    -1.90857999611e-03, &
     4.83549012930e-03, &
    -4.03896134024e-03  &
    /)
	get_NUFISS2_WABA_DIFF_1 =  (cWABA43(0)*E**(4) + cWABA43(1)*E**(3) + cWABA43(2)*E**(2) + cWABA43(3)*E + cWABA43(4))*B**(5) + (cWABA43(5)*E**(4) + cWABA43(6)*E**(3) + cWABA43(7)*E**(2) + cWABA43(8)*E + cWABA43(9))*B**(4) + (cWABA43(10)*E**(4) + cWABA43(11)*E**(3) + cWABA43(12)*E**(2) + cWABA43(13)*E + cWABA43(14))*B**(3) + (cWABA43(15)*E**(4) + cWABA43(16)*E**(3) + cWABA43(17)*E**(2) + cWABA43(18)*E + cWABA43(19))*B**(2) + (cWABA43(20)*E**(4) + cWABA43(21)*E**(3) + cWABA43(22)*E**(2) + cWABA43(23)*E + cWABA43(24))*B + (cWABA43(25)*E**(4) + cWABA43(26)*E**(3) + cWABA43(27)*E**(2) + cWABA43(28)*E + cWABA43(29))

end function get_NUFISS2_WABA_DIFF_1

double precision function get_NUFISS2_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA44 = (/ & 
    -7.59254401501e-10, &
     1.29629775520e-08, &
    -8.91976709601e-08, &
     2.89849199517e-07, &
    -3.48624072168e-07, &
     8.64385085660e-08, &
    -1.40911681936e-06, &
     9.39516627613e-06, &
    -3.05391998272e-05, &
     3.78306175128e-05, &
    -3.05607803681e-06, &
     4.76047129820e-05, &
    -3.03060094758e-04, &
     9.55649204955e-04, &
    -1.21455139657e-03, &
     3.09822007434e-05, &
    -4.91088042346e-04, &
     3.26117547003e-03, &
    -1.08494453922e-02, &
     9.04122309171e-03  &
    /)
	get_NUFISS2_WABA_DIFF_2 =  (cWABA44(0)*E**(4) + cWABA44(1)*E**(3) + cWABA44(2)*E**(2) + cWABA44(3)*E + cWABA44(4))*B**(3) + (cWABA44(5)*E**(4) + cWABA44(6)*E**(3) + cWABA44(7)*E**(2) + cWABA44(8)*E + cWABA44(9))*B**(2) + (cWABA44(10)*E**(4) + cWABA44(11)*E**(3) + cWABA44(12)*E**(2) + cWABA44(13)*E + cWABA44(14))*B + (cWABA44(15)*E**(4) + cWABA44(16)*E**(3) + cWABA44(17)*E**(2) + cWABA44(18)*E + cWABA44(19))

end function get_NUFISS2_WABA_DIFF_2

double precision function get_BOR1_NO_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA45 = (/ & 
     1.09262267348e-07, &
    -1.64135857007e-06, &
     8.97839903987e-06, &
    -2.18210080263e-05, &
     3.34203735266e-05, &
    -7.28400750664e-09, &
     1.27227883086e-07, &
    -9.08763957343e-07, &
     3.51299962616e-06, &
     8.63120153414e-07  &
    /)
	get_BOR1_NO_XE_WABA_DIFF_0 =  (cWABA45(0)*E**(4) + cWABA45(1)*E**(3) + cWABA45(2)*E**(2) + cWABA45(3)*E + cWABA45(4))*B + (cWABA45(5)*E**(4) + cWABA45(6)*E**(3) + cWABA45(7)*E**(2) + cWABA45(8)*E + cWABA45(9))

end function get_BOR1_NO_XE_WABA_DIFF_0

double precision function get_BOR1_NO_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA46 = (/ & 
     1.18761665288e-11, &
    -2.31492785929e-10, &
     1.63808056374e-09, &
    -4.92091358014e-09, &
     5.07116370927e-09, &
    -3.68845139626e-10, &
     7.39337033260e-09, &
    -5.45362359046e-08, &
     1.74790371991e-07, &
    -2.00243174545e-07, &
     2.58042931859e-09, &
    -5.58144577666e-08, &
     4.51613144403e-07, &
    -1.65424795747e-06, &
     2.35345578333e-06, &
    -2.67477123659e-09, &
     6.36009232428e-08, &
    -5.93919773459e-07, &
     2.84424741979e-06, &
     1.60771265020e-06  &
    /)
	get_BOR1_NO_XE_WABA_DIFF_1 =  (cWABA46(0)*E**(4) + cWABA46(1)*E**(3) + cWABA46(2)*E**(2) + cWABA46(3)*E + cWABA46(4))*B**(3) + (cWABA46(5)*E**(4) + cWABA46(6)*E**(3) + cWABA46(7)*E**(2) + cWABA46(8)*E + cWABA46(9))*B**(2) + (cWABA46(10)*E**(4) + cWABA46(11)*E**(3) + cWABA46(12)*E**(2) + cWABA46(13)*E + cWABA46(14))*B + (cWABA46(15)*E**(4) + cWABA46(16)*E**(3) + cWABA46(17)*E**(2) + cWABA46(18)*E + cWABA46(19))

end function get_BOR1_NO_XE_WABA_DIFF_1

double precision function get_BOR1_NO_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA47 = (/ & 
     2.08113275639e-10, &
    -3.45014660494e-09, &
     1.96955329955e-08, &
    -4.00999051309e-08, &
     2.39834348682e-08, &
    -1.01471029160e-08, &
     1.75272751738e-07, &
    -1.07826345563e-06, &
     2.44526062261e-06, &
     7.23884728255e-06  &
    /)
	get_BOR1_NO_XE_WABA_DIFF_2 =  (cWABA47(0)*E**(4) + cWABA47(1)*E**(3) + cWABA47(2)*E**(2) + cWABA47(3)*E + cWABA47(4))*B + (cWABA47(5)*E**(4) + cWABA47(6)*E**(3) + cWABA47(7)*E**(2) + cWABA47(8)*E + cWABA47(9))

end function get_BOR1_NO_XE_WABA_DIFF_2

double precision function get_BOR2_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA48 = (/ & 
    -7.28498490320e-02, &
     1.00534145185e+00, &
    -4.74707933025e+00, &
     8.32027230919e+00, &
    -1.48651005510e+01, &
     1.81982455590e-03, &
    -1.81906184061e-02, &
     3.44800561248e-02, &
     3.21381700608e-01, &
     1.02178765515e+01  &
    /)
	get_BOR2_XE_WABA_DIFF_0 =  (cWABA48(0)*E**(4) + cWABA48(1)*E**(3) + cWABA48(2)*E**(2) + cWABA48(3)*E + cWABA48(4))*B + (cWABA48(5)*E**(4) + cWABA48(6)*E**(3) + cWABA48(7)*E**(2) + cWABA48(8)*E + cWABA48(9))

end function get_BOR2_XE_WABA_DIFF_0

double precision function get_BOR2_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA49 = (/ & 
    -2.20759749156e-06, &
     4.14031538462e-05, &
    -2.99978541423e-04, &
     1.00055795660e-03, &
    -1.30202364156e-03, &
     5.01510846754e-05, &
    -1.01625138044e-03, &
     8.02187592605e-03, &
    -2.94257577226e-02, &
     4.28385461230e-02, &
     9.38765710799e-05, &
     3.59672516788e-04, &
    -2.04547262645e-02, &
     1.37982566614e-01, &
    -2.97649091794e-01, &
    -4.94023624700e-03, &
     7.77191346573e-02, &
    -4.50147516187e-01, &
     1.14735440244e+00, &
    -1.23202711804e+00, &
     7.83769354266e-03, &
    -1.18569890192e-01, &
     6.50015848906e-01, &
    -1.30908246194e+00, &
     1.18825021345e+01  &
    /)
	get_BOR2_XE_WABA_DIFF_1 =  (cWABA49(0)*E**(4) + cWABA49(1)*E**(3) + cWABA49(2)*E**(2) + cWABA49(3)*E + cWABA49(4))*B**(4) + (cWABA49(5)*E**(4) + cWABA49(6)*E**(3) + cWABA49(7)*E**(2) + cWABA49(8)*E + cWABA49(9))*B**(3) + (cWABA49(10)*E**(4) + cWABA49(11)*E**(3) + cWABA49(12)*E**(2) + cWABA49(13)*E + cWABA49(14))*B**(2) + (cWABA49(15)*E**(4) + cWABA49(16)*E**(3) + cWABA49(17)*E**(2) + cWABA49(18)*E + cWABA49(19))*B + (cWABA49(20)*E**(4) + cWABA49(21)*E**(3) + cWABA49(22)*E**(2) + cWABA49(23)*E + cWABA49(24))

end function get_BOR2_XE_WABA_DIFF_1

double precision function get_BOR2_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA50 = (/ & 
    -1.50370085414e-08, &
     1.72532517452e-07, &
    -7.41334405031e-07, &
     1.90363813706e-06, &
    -1.95842549889e-06, &
     2.29183856787e-06, &
    -2.22580701624e-05, &
     6.88669513780e-05, &
    -1.57682388835e-04, &
     1.66814708904e-04, &
    -1.03710608155e-04, &
     5.59701950518e-04, &
     1.79720194763e-03, &
    -7.58283645817e-03, &
     7.05012652445e-03, &
     1.08875933429e-03, &
     1.77161830630e-02, &
    -2.79012690672e-01, &
     8.53278932289e-01, &
    -8.32871990738e-01, &
     6.06605298987e-03, &
    -4.83757261245e-01, &
     4.89472811552e+00, &
    -1.38565750334e+01, &
     1.77364675242e+01  &
    /)
	get_BOR2_XE_WABA_DIFF_2 =  (cWABA50(0)*E**(4) + cWABA50(1)*E**(3) + cWABA50(2)*E**(2) + cWABA50(3)*E + cWABA50(4))*B**(4) + (cWABA50(5)*E**(4) + cWABA50(6)*E**(3) + cWABA50(7)*E**(2) + cWABA50(8)*E + cWABA50(9))*B**(3) + (cWABA50(10)*E**(4) + cWABA50(11)*E**(3) + cWABA50(12)*E**(2) + cWABA50(13)*E + cWABA50(14))*B**(2) + (cWABA50(15)*E**(4) + cWABA50(16)*E**(3) + cWABA50(17)*E**(2) + cWABA50(18)*E + cWABA50(19))*B + (cWABA50(20)*E**(4) + cWABA50(21)*E**(3) + cWABA50(22)*E**(2) + cWABA50(23)*E + cWABA50(24))

end function get_BOR2_XE_WABA_DIFF_2

double precision function get_REMOV1_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA51 = (/ & 
     3.27795907001e-05, &
    -5.21074137193e-04, &
     3.06277985796e-03, &
    -7.96013035458e-03, &
     8.93845797355e-03, &
    -2.36759069015e-06, &
     3.90712361539e-05, &
    -2.46716019474e-04, &
     7.45110364630e-04, &
    -2.95979524801e-04  &
    /)
	get_REMOV1_WABA_DIFF_0 =  (cWABA51(0)*E**(4) + cWABA51(1)*E**(3) + cWABA51(2)*E**(2) + cWABA51(3)*E + cWABA51(4))*B + (cWABA51(5)*E**(4) + cWABA51(6)*E**(3) + cWABA51(7)*E**(2) + cWABA51(8)*E + cWABA51(9))

end function get_REMOV1_WABA_DIFF_0

double precision function get_REMOV1_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA52 = (/ & 
    -2.18046700708e-09, &
     3.38128732179e-08, &
    -1.94708806054e-07, &
     5.02011231187e-07, &
    -5.14124635276e-07, &
     7.01753957731e-08, &
    -1.06518690739e-06, &
     5.89265284721e-06, &
    -1.41089311684e-05, &
     1.29777335753e-05, &
    -5.85292017904e-07, &
     8.60611324297e-06, &
    -4.42090736000e-05, &
     8.64149328363e-05, &
    -4.23437147914e-05, &
     2.00387100968e-07, &
    -1.76204223719e-06, &
    -6.41706744676e-06, &
     1.23199936985e-04, &
     3.23388518402e-04  &
    /)
	get_REMOV1_WABA_DIFF_1 =  (cWABA52(0)*E**(4) + cWABA52(1)*E**(3) + cWABA52(2)*E**(2) + cWABA52(3)*E + cWABA52(4))*B**(3) + (cWABA52(5)*E**(4) + cWABA52(6)*E**(3) + cWABA52(7)*E**(2) + cWABA52(8)*E + cWABA52(9))*B**(2) + (cWABA52(10)*E**(4) + cWABA52(11)*E**(3) + cWABA52(12)*E**(2) + cWABA52(13)*E + cWABA52(14))*B + (cWABA52(15)*E**(4) + cWABA52(16)*E**(3) + cWABA52(17)*E**(2) + cWABA52(18)*E + cWABA52(19))

end function get_REMOV1_WABA_DIFF_1

double precision function get_REMOV1_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA53 = (/ & 
     1.49477960331e-08, &
    -2.48625017421e-07, &
     1.39020735038e-06, &
    -2.59728648517e-06, &
     1.09762548931e-06, &
    -8.11385054605e-07, &
     1.41640594760e-05, &
    -8.73233043854e-05, &
     1.96102942308e-04, &
     6.16372718537e-04  &
    /)
	get_REMOV1_WABA_DIFF_2 =  (cWABA53(0)*E**(4) + cWABA53(1)*E**(3) + cWABA53(2)*E**(2) + cWABA53(3)*E + cWABA53(4))*B + (cWABA53(5)*E**(4) + cWABA53(6)*E**(3) + cWABA53(7)*E**(2) + cWABA53(8)*E + cWABA53(9))

end function get_REMOV1_WABA_DIFF_2

double precision function get_DIFF2_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA54 = (/ & 
     7.28351279695e-05, &
    -1.03424302738e-03, &
     5.22644506369e-03, &
    -1.10934083297e-02, &
     1.20615403329e-02, &
     3.64359955111e-06, &
    -5.53838834971e-05, &
     2.87576046851e-04, &
    -1.82927103741e-04, &
    -9.77875006470e-03  &
    /)
	get_DIFF2_WABA_DIFF_0 =  (cWABA54(0)*E**(4) + cWABA54(1)*E**(3) + cWABA54(2)*E**(2) + cWABA54(3)*E + cWABA54(4))*B + (cWABA54(5)*E**(4) + cWABA54(6)*E**(3) + cWABA54(7)*E**(2) + cWABA54(8)*E + cWABA54(9))

end function get_DIFF2_WABA_DIFF_0

double precision function get_DIFF2_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA55 = (/ & 
    -1.36318916701e-09, &
     1.28858372044e-08, &
    -6.75645064115e-08, &
     3.66627995480e-07, &
    -6.78460670216e-07, &
    -6.15736943216e-08, &
     1.30080624543e-06, &
    -8.42514979657e-06, &
     1.71966423557e-05, &
    -3.87100676655e-06, &
     1.10119000915e-06, &
    -2.06509431794e-05, &
     1.36126417511e-04, &
    -3.60086017420e-04, &
     2.21088594202e-04, &
     1.69108358284e-07, &
     1.70142265889e-06, &
    -5.55987137752e-05, &
     6.98572504018e-04, &
    -1.05549726127e-02  &
    /)
	get_DIFF2_WABA_DIFF_1 =  (cWABA55(0)*E**(4) + cWABA55(1)*E**(3) + cWABA55(2)*E**(2) + cWABA55(3)*E + cWABA55(4))*B**(3) + (cWABA55(5)*E**(4) + cWABA55(6)*E**(3) + cWABA55(7)*E**(2) + cWABA55(8)*E + cWABA55(9))*B**(2) + (cWABA55(10)*E**(4) + cWABA55(11)*E**(3) + cWABA55(12)*E**(2) + cWABA55(13)*E + cWABA55(14))*B + (cWABA55(15)*E**(4) + cWABA55(16)*E**(3) + cWABA55(17)*E**(2) + cWABA55(18)*E + cWABA55(19))

end function get_DIFF2_WABA_DIFF_1

double precision function get_DIFF2_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA56 = (/ & 
     1.03203103631e-09, &
    -1.57280338340e-08, &
     8.85324104008e-08, &
    -2.18585869652e-07, &
     1.97884826655e-07, &
    -1.57801871788e-07, &
     2.42591490360e-06, &
    -1.37901891829e-05, &
     3.44487954640e-05, &
    -3.16467916612e-05, &
     7.61441916602e-06, &
    -1.18083191851e-04, &
     6.77682464115e-04, &
    -1.71212893362e-03, &
     1.59722133200e-03, &
    -1.09172117669e-04, &
     1.70344787252e-03, &
    -9.83392731636e-03, &
     2.51094182367e-02, &
    -3.36185883349e-02  &
    /)
	get_DIFF2_WABA_DIFF_2 =  (cWABA56(0)*E**(4) + cWABA56(1)*E**(3) + cWABA56(2)*E**(2) + cWABA56(3)*E + cWABA56(4))*B**(3) + (cWABA56(5)*E**(4) + cWABA56(6)*E**(3) + cWABA56(7)*E**(2) + cWABA56(8)*E + cWABA56(9))*B**(2) + (cWABA56(10)*E**(4) + cWABA56(11)*E**(3) + cWABA56(12)*E**(2) + cWABA56(13)*E + cWABA56(14))*B + (cWABA56(15)*E**(4) + cWABA56(16)*E**(3) + cWABA56(17)*E**(2) + cWABA56(18)*E + cWABA56(19))

end function get_DIFF2_WABA_DIFF_2

double precision function get_DIFF1_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA57 = (/ & 
     5.46313891844e-04, &
    -9.05667858107e-03, &
     5.58625206263e-02, &
    -1.51265747584e-01, &
     1.31390374999e-01, &
     1.09258277305e-04, &
    -1.75298847909e-03, &
     1.05834964472e-02, &
    -2.95457315152e-02, &
     4.74678926760e-02  &
    /)
	get_DIFF1_WABA_DIFF_0 =  (cWABA57(0)*E**(4) + cWABA57(1)*E**(3) + cWABA57(2)*E**(2) + cWABA57(3)*E + cWABA57(4))*B + (cWABA57(5)*E**(4) + cWABA57(6)*E**(3) + cWABA57(7)*E**(2) + cWABA57(8)*E + cWABA57(9))

end function get_DIFF1_WABA_DIFF_0

double precision function get_DIFF1_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA58 = (/ & 
    -2.02355291669e-09, &
     2.13089127487e-07, &
    -2.47968622080e-06, &
     1.00388797445e-05, &
    -1.33763977156e-05, &
     2.49145257752e-07, &
    -9.14812354245e-06, &
     9.19784448209e-05, &
    -3.66247420435e-04, &
     5.22279094708e-04, &
    -3.79348358790e-06, &
     9.25918380522e-05, &
    -8.28097137313e-04, &
     3.37084079842e-03, &
    -5.80989405977e-03, &
     2.13610383267e-06, &
    -3.75523455383e-05, &
     4.07889348963e-04, &
    -3.03470101666e-03, &
     2.15864135311e-02  &
    /)
	get_DIFF1_WABA_DIFF_1 =  (cWABA58(0)*E**(4) + cWABA58(1)*E**(3) + cWABA58(2)*E**(2) + cWABA58(3)*E + cWABA58(4))*B**(3) + (cWABA58(5)*E**(4) + cWABA58(6)*E**(3) + cWABA58(7)*E**(2) + cWABA58(8)*E + cWABA58(9))*B**(2) + (cWABA58(10)*E**(4) + cWABA58(11)*E**(3) + cWABA58(12)*E**(2) + cWABA58(13)*E + cWABA58(14))*B + (cWABA58(15)*E**(4) + cWABA58(16)*E**(3) + cWABA58(17)*E**(2) + cWABA58(18)*E + cWABA58(19))

end function get_DIFF1_WABA_DIFF_1

double precision function get_DIFF1_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA59 = (/ & 
     1.45010459724e-08, &
    -2.36011920635e-07, &
     1.41941975763e-06, &
    -3.74728550894e-06, &
     3.65660479891e-06, &
    -1.93194356560e-06, &
     3.14925103671e-05, &
    -1.89688691628e-04, &
     5.01758524791e-04, &
    -4.90603087987e-04, &
     7.99179096150e-05, &
    -1.30473035098e-03, &
     7.87015660819e-03, &
    -2.08659803667e-02, &
     2.04683713304e-02, &
    -9.93839645922e-04, &
     1.62463101593e-02, &
    -9.80680097524e-02, &
     2.60468172427e-01, &
    -2.50353414488e-01  &
    /)
	get_DIFF1_WABA_DIFF_2 =  (cWABA59(0)*E**(4) + cWABA59(1)*E**(3) + cWABA59(2)*E**(2) + cWABA59(3)*E + cWABA59(4))*B**(3) + (cWABA59(5)*E**(4) + cWABA59(6)*E**(3) + cWABA59(7)*E**(2) + cWABA59(8)*E + cWABA59(9))*B**(2) + (cWABA59(10)*E**(4) + cWABA59(11)*E**(3) + cWABA59(12)*E**(2) + cWABA59(13)*E + cWABA59(14))*B + (cWABA59(15)*E**(4) + cWABA59(16)*E**(3) + cWABA59(17)*E**(2) + cWABA59(18)*E + cWABA59(19))

end function get_DIFF1_WABA_DIFF_2

double precision function get_BOR1_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA60 = (/ & 
     5.46334347992e-04, &
    -7.84294011761e-03, &
     3.50602435633e-02, &
    -7.76129925705e-02, &
     7.15607282412e-01, &
    -3.09576791038e-04, &
     5.64038832450e-03, &
    -4.27984850080e-02, &
     1.86698004926e-01, &
    -3.61500734837e-01  &
    /)
	get_BOR1_XE_WABA_DIFF_0 =  (cWABA60(0)*E**(4) + cWABA60(1)*E**(3) + cWABA60(2)*E**(2) + cWABA60(3)*E + cWABA60(4))*B + (cWABA60(5)*E**(4) + cWABA60(6)*E**(3) + cWABA60(7)*E**(2) + cWABA60(8)*E + cWABA60(9))

end function get_BOR1_XE_WABA_DIFF_0

double precision function get_BOR1_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA61 = (/ & 
     9.59637396533e-10, &
    -1.31415535441e-07, &
     1.79124975028e-06, &
    -8.55679144126e-06, &
     1.36452299577e-05, &
     3.69258131338e-07, &
    -2.16574594360e-06, &
    -2.13581930792e-05, &
     1.96065178213e-04, &
    -4.04847188838e-04, &
    -1.69040388980e-05, &
     2.54160226850e-04, &
    -1.33736562121e-03, &
     2.67188922113e-03, &
    -9.94872217497e-04, &
     2.02124067308e-04, &
    -3.50108017962e-03, &
     2.33823197257e-02, &
    -7.35638484765e-02, &
     9.51885324612e-02, &
    -4.36036688178e-04, &
     7.73716692466e-03, &
    -5.55193662390e-02, &
     2.19130738068e-01, &
    -3.81720601157e-01  &
    /)
	get_BOR1_XE_WABA_DIFF_1 =  (cWABA61(0)*E**(4) + cWABA61(1)*E**(3) + cWABA61(2)*E**(2) + cWABA61(3)*E + cWABA61(4))*B**(4) + (cWABA61(5)*E**(4) + cWABA61(6)*E**(3) + cWABA61(7)*E**(2) + cWABA61(8)*E + cWABA61(9))*B**(3) + (cWABA61(10)*E**(4) + cWABA61(11)*E**(3) + cWABA61(12)*E**(2) + cWABA61(13)*E + cWABA61(14))*B**(2) + (cWABA61(15)*E**(4) + cWABA61(16)*E**(3) + cWABA61(17)*E**(2) + cWABA61(18)*E + cWABA61(19))*B + (cWABA61(20)*E**(4) + cWABA61(21)*E**(3) + cWABA61(22)*E**(2) + cWABA61(23)*E + cWABA61(24))

end function get_BOR1_XE_WABA_DIFF_1

double precision function get_BOR1_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA62 = (/ & 
     5.83890908841e-10, &
    -8.98337639740e-09, &
     5.29676475687e-08, &
    -1.48037371159e-07, &
     1.57219035836e-07, &
    -1.20728697450e-07, &
     1.86785599949e-06, &
    -1.10103706608e-05, &
     3.05195074565e-05, &
    -3.20177098528e-05, &
     8.96801119912e-06, &
    -1.40224050754e-04, &
     8.31978230928e-04, &
    -2.30384005512e-03, &
     2.39533903476e-03, &
    -2.73928791458e-04, &
     4.34257670970e-03, &
    -2.61192304468e-02, &
     7.31981730135e-02, &
    -7.60929038575e-02, &
     2.71100107711e-03, &
    -4.32791141126e-02, &
     2.62982960338e-01, &
    -7.50583019602e-01, &
     8.68753159968e-01  &
    /)
	get_BOR1_XE_WABA_DIFF_2 =  (cWABA62(0)*E**(4) + cWABA62(1)*E**(3) + cWABA62(2)*E**(2) + cWABA62(3)*E + cWABA62(4))*B**(4) + (cWABA62(5)*E**(4) + cWABA62(6)*E**(3) + cWABA62(7)*E**(2) + cWABA62(8)*E + cWABA62(9))*B**(3) + (cWABA62(10)*E**(4) + cWABA62(11)*E**(3) + cWABA62(12)*E**(2) + cWABA62(13)*E + cWABA62(14))*B**(2) + (cWABA62(15)*E**(4) + cWABA62(16)*E**(3) + cWABA62(17)*E**(2) + cWABA62(18)*E + cWABA62(19))*B + (cWABA62(20)*E**(4) + cWABA62(21)*E**(3) + cWABA62(22)*E**(2) + cWABA62(23)*E + cWABA62(24))

end function get_BOR1_XE_WABA_DIFF_2

double precision function get_XE_YIELD_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA63 = (/ & 
     1.09260288774e-05, &
    -1.74330786811e-04, &
     1.04671343239e-03, &
    -2.84778429975e-03, &
     3.15380591197e-03, &
    -1.82106700754e-06, &
     2.95255726116e-05, &
    -1.79061972997e-04, &
     4.86457567475e-04, &
    -5.34477996064e-04  &
    /)
	get_XE_YIELD_WABA_DIFF_0 =  (cWABA63(0)*E**(4) + cWABA63(1)*E**(3) + cWABA63(2)*E**(2) + cWABA63(3)*E + cWABA63(4))*B + (cWABA63(5)*E**(4) + cWABA63(6)*E**(3) + cWABA63(7)*E**(2) + cWABA63(8)*E + cWABA63(9))

end function get_XE_YIELD_WABA_DIFF_0

double precision function get_XE_YIELD_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA64 = (/ & 
     5.85336148979e-10, &
    -9.90860671619e-09, &
     6.33915359674e-08, &
    -1.83495115065e-07, &
     2.07130347644e-07, &
    -2.39291509235e-08, &
     4.07346838639e-07, &
    -2.62862403315e-06, &
     7.72102642787e-06, &
    -8.95311737018e-06, &
     2.95847196415e-07, &
    -5.07133370233e-06, &
     3.31254148581e-05, &
    -9.96295197793e-05, &
     1.21508458628e-04, &
    -1.08598752725e-06, &
     1.85551375242e-05, &
    -1.21512513972e-04, &
     3.74066321081e-04, &
    -5.00999687138e-04, &
     5.25819295779e-07, &
    -8.59522720982e-06, &
     5.10011287522e-05, &
    -1.24798904551e-04, &
     7.10349588279e-05  &
    /)
	get_XE_YIELD_WABA_DIFF_1 =  (cWABA64(0)*E**(4) + cWABA64(1)*E**(3) + cWABA64(2)*E**(2) + cWABA64(3)*E + cWABA64(4))*B**(4) + (cWABA64(5)*E**(4) + cWABA64(6)*E**(3) + cWABA64(7)*E**(2) + cWABA64(8)*E + cWABA64(9))*B**(3) + (cWABA64(10)*E**(4) + cWABA64(11)*E**(3) + cWABA64(12)*E**(2) + cWABA64(13)*E + cWABA64(14))*B**(2) + (cWABA64(15)*E**(4) + cWABA64(16)*E**(3) + cWABA64(17)*E**(2) + cWABA64(18)*E + cWABA64(19))*B + (cWABA64(20)*E**(4) + cWABA64(21)*E**(3) + cWABA64(22)*E**(2) + cWABA64(23)*E + cWABA64(24))

end function get_XE_YIELD_WABA_DIFF_1

double precision function get_XE_YIELD_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA65 = (/ & 
    -6.42049075728e-12, &
     1.04389080068e-10, &
    -6.21748053028e-10, &
     1.60342749069e-09, &
    -1.51771569715e-09, &
     1.14253422645e-09, &
    -1.86960644761e-08, &
     1.12398068200e-07, &
    -2.93782947296e-07, &
     2.83474415830e-07, &
    -7.10134041398e-08, &
     1.16410329494e-06, &
    -7.03521966297e-06, &
     1.86046884651e-05, &
    -1.83917223848e-05, &
     1.81002153933e-06, &
    -2.94020509256e-05, &
     1.76347870877e-04, &
    -4.66042587180e-04, &
     4.72592408643e-04, &
    -1.61656587428e-05, &
     2.56451893719e-04, &
    -1.49023533750e-03, &
     3.78474407174e-03, &
    -3.84852123231e-03  &
    /)
	get_XE_YIELD_WABA_DIFF_2 =  (cWABA65(0)*E**(4) + cWABA65(1)*E**(3) + cWABA65(2)*E**(2) + cWABA65(3)*E + cWABA65(4))*B**(4) + (cWABA65(5)*E**(4) + cWABA65(6)*E**(3) + cWABA65(7)*E**(2) + cWABA65(8)*E + cWABA65(9))*B**(3) + (cWABA65(10)*E**(4) + cWABA65(11)*E**(3) + cWABA65(12)*E**(2) + cWABA65(13)*E + cWABA65(14))*B**(2) + (cWABA65(15)*E**(4) + cWABA65(16)*E**(3) + cWABA65(17)*E**(2) + cWABA65(18)*E + cWABA65(19))*B + (cWABA65(20)*E**(4) + cWABA65(21)*E**(3) + cWABA65(22)*E**(2) + cWABA65(23)*E + cWABA65(24))

end function get_XE_YIELD_WABA_DIFF_2

double precision function get_NU_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA66 = (/ & 
    -2.36746590612e-03, &
     3.77580493690e-02, &
    -2.23073611643e-01, &
     5.76769326762e-01, &
    -5.39313110175e-01, &
     1.63895143641e-04, &
    -2.66845669532e-03, &
     1.60100149863e-02, &
    -4.15503257566e-02, &
     3.62385611857e-02  &
    /)
	get_NU_WABA_DIFF_0 =  (cWABA66(0)*E**(4) + cWABA66(1)*E**(3) + cWABA66(2)*E**(2) + cWABA66(3)*E + cWABA66(4))*B + (cWABA66(5)*E**(4) + cWABA66(6)*E**(3) + cWABA66(7)*E**(2) + cWABA66(8)*E + cWABA66(9))

end function get_NU_WABA_DIFF_0

double precision function get_NU_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA67 = (/ & 
     4.03843951402e-09, &
    -6.18959673851e-08, &
     3.48320630570e-07, &
    -8.46591545296e-07, &
     7.35532319758e-07, &
    -2.15840941537e-07, &
     3.31351836299e-06, &
    -1.86519493485e-05, &
     4.51857039752e-05, &
    -3.87508295288e-05, &
     4.02591195778e-06, &
    -6.18652180950e-05, &
     3.47426281592e-04, &
    -8.32833683956e-04, &
     6.90039425206e-04, &
    -2.98668703525e-05, &
     4.59019351286e-04, &
    -2.55873130827e-03, &
     5.96628952437e-03, &
    -4.46721969312e-03, &
     7.95671215064e-05, &
    -1.23905505312e-03, &
     6.95364606314e-03, &
    -1.58829865187e-02, &
     9.63034422516e-03, &
    -7.72476377916e-05, &
     1.25287653044e-03, &
    -7.63871617533e-03, &
     2.11147116462e-02, &
    -2.52020918159e-02  &
    /)
	get_NU_WABA_DIFF_1 =  (cWABA67(0)*E**(4) + cWABA67(1)*E**(3) + cWABA67(2)*E**(2) + cWABA67(3)*E + cWABA67(4))*B**(5) + (cWABA67(5)*E**(4) + cWABA67(6)*E**(3) + cWABA67(7)*E**(2) + cWABA67(8)*E + cWABA67(9))*B**(4) + (cWABA67(10)*E**(4) + cWABA67(11)*E**(3) + cWABA67(12)*E**(2) + cWABA67(13)*E + cWABA67(14))*B**(3) + (cWABA67(15)*E**(4) + cWABA67(16)*E**(3) + cWABA67(17)*E**(2) + cWABA67(18)*E + cWABA67(19))*B**(2) + (cWABA67(20)*E**(4) + cWABA67(21)*E**(3) + cWABA67(22)*E**(2) + cWABA67(23)*E + cWABA67(24))*B + (cWABA67(25)*E**(4) + cWABA67(26)*E**(3) + cWABA67(27)*E**(2) + cWABA67(28)*E + cWABA67(29))

end function get_NU_WABA_DIFF_1

double precision function get_NU_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA68 = (/ & 
    -3.78518368911e-11, &
     6.22463299005e-10, &
    -3.78378871082e-09, &
     1.00463693405e-08, &
    -9.78927268614e-09, &
     1.00902129096e-08, &
    -1.65339503480e-07, &
     1.00224185510e-06, &
    -2.65689291111e-06, &
     2.58930434748e-06, &
    -1.02190714776e-06, &
     1.66862889744e-05, &
    -1.00835983383e-04, &
     2.66697427446e-04, &
    -2.59618170755e-04, &
     4.89711967106e-05, &
    -7.97172057053e-04, &
     4.80306763355e-03, &
    -1.26686393396e-02, &
     1.23009666051e-02, &
    -1.10467019060e-03, &
     1.79491299265e-02, &
    -1.07953084838e-01, &
     2.84185549997e-01, &
    -2.75017166239e-01, &
     9.33377323551e-03, &
    -1.51639600244e-01, &
     9.12543091198e-01, &
    -2.40557817470e+00, &
     2.32536707027e+00  &
    /)
	get_NU_WABA_DIFF_2 =  (cWABA68(0)*E**(4) + cWABA68(1)*E**(3) + cWABA68(2)*E**(2) + cWABA68(3)*E + cWABA68(4))*B**(5) + (cWABA68(5)*E**(4) + cWABA68(6)*E**(3) + cWABA68(7)*E**(2) + cWABA68(8)*E + cWABA68(9))*B**(4) + (cWABA68(10)*E**(4) + cWABA68(11)*E**(3) + cWABA68(12)*E**(2) + cWABA68(13)*E + cWABA68(14))*B**(3) + (cWABA68(15)*E**(4) + cWABA68(16)*E**(3) + cWABA68(17)*E**(2) + cWABA68(18)*E + cWABA68(19))*B**(2) + (cWABA68(20)*E**(4) + cWABA68(21)*E**(3) + cWABA68(22)*E**(2) + cWABA68(23)*E + cWABA68(24))*B + (cWABA68(25)*E**(4) + cWABA68(26)*E**(3) + cWABA68(27)*E**(2) + cWABA68(28)*E + cWABA68(29))

end function get_NU_WABA_DIFF_2

double precision function get_BOR2_NO_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA69 = (/ & 
     1.81830904266e-07, &
    -4.43899216591e-06, &
     4.14578123624e-05, &
    -1.68050441542e-04, &
    -2.97740834370e-06, &
    -7.60791080478e-11, &
    -3.76408400907e-08, &
     9.19439891376e-07, &
    -5.30161491533e-06, &
     5.61167028186e-04  &
    /)
	get_BOR2_NO_XE_WABA_DIFF_0 =  (cWABA69(0)*E**(4) + cWABA69(1)*E**(3) + cWABA69(2)*E**(2) + cWABA69(3)*E + cWABA69(4))*B + (cWABA69(5)*E**(4) + cWABA69(6)*E**(3) + cWABA69(7)*E**(2) + cWABA69(8)*E + cWABA69(9))

end function get_BOR2_NO_XE_WABA_DIFF_0

double precision function get_BOR2_NO_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA70 = (/ & 
    -3.64821958113e-03, &
     7.01165544422e-02, &
    -4.77680690513e-01, &
     1.38505948405e+00, &
    -1.45453000362e+00, &
    -1.59814851255e-01, &
     3.25548713611e+00, &
    -2.36671850807e+01, &
     7.31717773470e+01, &
    -8.11813604900e+01, &
     3.64848133444e-03, &
    -7.01200953818e-02, &
     4.77697872700e-01, &
    -1.38509563593e+00, &
     1.45511687222e+00, &
    -2.87737809319e-03, &
     4.15478348822e-02, &
    -2.16958817055e-01, &
     4.91677213822e-01, &
    -4.25654199660e-01  &
    /)
	get_BOR2_NO_XE_WABA_DIFF_1 =  (cWABA70(0)*E**(4) + cWABA70(1)*E**(3) + cWABA70(2)*E**(2) + cWABA70(3)*E + cWABA70(4))*exp(B*(cWABA70(5)*E**(4) + cWABA70(6)*E**(3) + cWABA70(7)*E**(2) + cWABA70(8)*E + cWABA70(9))) + (cWABA70(10)*E**(4) + cWABA70(11)*E**(3) + cWABA70(12)*E**(2) + cWABA70(13)*E + cWABA70(14))*exp(B*(cWABA70(15)*E**(4) + cWABA70(16)*E**(3) + cWABA70(17)*E**(2) + cWABA70(18)*E + cWABA70(19)))

end function get_BOR2_NO_XE_WABA_DIFF_1

double precision function get_BOR2_NO_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:14) :: cWABA71 = (/ & 
     3.96987443564e-11, &
    -1.37603197638e-09, &
     1.39088410454e-08, &
    -4.81153686816e-08, &
     5.33281683364e-08, &
    -7.81739427344e-10, &
     7.55783985977e-08, &
    -9.68675877529e-07, &
     3.58843207675e-06, &
    -4.01062177925e-06, &
    -7.35945657722e-08, &
     1.67544376519e-08, &
     1.00981208787e-05, &
    -3.97095113345e-05, &
     4.71818452600e-04  &
    /)
	get_BOR2_NO_XE_WABA_DIFF_2 =  (cWABA71(0)*E**(4) + cWABA71(1)*E**(3) + cWABA71(2)*E**(2) + cWABA71(3)*E + cWABA71(4))*B**(2) + (cWABA71(5)*E**(4) + cWABA71(6)*E**(3) + cWABA71(7)*E**(2) + cWABA71(8)*E + cWABA71(9))*B + (cWABA71(10)*E**(4) + cWABA71(11)*E**(3) + cWABA71(12)*E**(2) + cWABA71(13)*E + cWABA71(14))

end function get_BOR2_NO_XE_WABA_DIFF_2

double precision function get_KAPPA_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA72 = (/ & 
     9.10580603244e-15, &
    -1.45935721356e-13, &
     8.69422353084e-13, &
    -2.28637764873e-12, &
     2.26765536976e-12, &
    -1.81995803703e-16, &
     3.51895853340e-15, &
    -2.47487344495e-14, &
     7.61476803680e-14, &
    -9.20935174915e-14  &
    /)
	get_KAPPA_WABA_DIFF_0 =  (cWABA72(0)*E**(4) + cWABA72(1)*E**(3) + cWABA72(2)*E**(2) + cWABA72(3)*E + cWABA72(4))*B + (cWABA72(5)*E**(4) + cWABA72(6)*E**(3) + cWABA72(7)*E**(2) + cWABA72(8)*E + cWABA72(9))

end function get_KAPPA_WABA_DIFF_0

double precision function get_KAPPA_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA73 = (/ & 
    -7.31344444092e-21, &
     4.87400540827e-20, &
     1.26412642538e-19, &
    -1.41766773151e-18, &
     2.38138319980e-18, &
     7.22216093874e-19, &
    -8.27589676194e-18, &
     2.93303376050e-17, &
    -2.54830683507e-17, &
    -2.12221255740e-17, &
    -2.27426406736e-17, &
     3.09980421472e-16, &
    -1.52047780790e-15, &
     3.17051204213e-15, &
    -2.41108661530e-15, &
     2.90659047630e-16, &
    -4.29695438112e-15, &
     2.34953170831e-14, &
    -5.67417541656e-14, &
     5.22996057810e-14, &
    -1.36960263650e-15, &
     2.10298393052e-14, &
    -1.20140669737e-13, &
     3.05027428193e-13, &
    -2.98733319706e-13, &
     1.48854723344e-15, &
    -2.29106761193e-14, &
     1.30599783422e-13, &
    -3.25895672584e-13, &
     2.94839043571e-13  &
    /)
	get_KAPPA_WABA_DIFF_1 =  (cWABA73(0)*E**(4) + cWABA73(1)*E**(3) + cWABA73(2)*E**(2) + cWABA73(3)*E + cWABA73(4))*B**(5) + (cWABA73(5)*E**(4) + cWABA73(6)*E**(3) + cWABA73(7)*E**(2) + cWABA73(8)*E + cWABA73(9))*B**(4) + (cWABA73(10)*E**(4) + cWABA73(11)*E**(3) + cWABA73(12)*E**(2) + cWABA73(13)*E + cWABA73(14))*B**(3) + (cWABA73(15)*E**(4) + cWABA73(16)*E**(3) + cWABA73(17)*E**(2) + cWABA73(18)*E + cWABA73(19))*B**(2) + (cWABA73(20)*E**(4) + cWABA73(21)*E**(3) + cWABA73(22)*E**(2) + cWABA73(23)*E + cWABA73(24))*B + (cWABA73(25)*E**(4) + cWABA73(26)*E**(3) + cWABA73(27)*E**(2) + cWABA73(28)*E + cWABA73(29))

end function get_KAPPA_WABA_DIFF_1

double precision function get_KAPPA_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA74 = (/ & 
     6.70131344321e-23, &
    -1.04929424248e-21, &
     6.05725041781e-21, &
    -1.53425091483e-20, &
     1.44856814486e-20, &
    -1.58066418061e-20, &
     2.52595609767e-19, &
    -1.48632860136e-18, &
     3.82956191438e-18, &
    -3.66536958033e-18, &
     1.34435892926e-18, &
    -2.19248787004e-17, &
     1.31407287142e-16, &
    -3.44088663404e-16, &
     3.33721127802e-16, &
    -4.88741718424e-17, &
     8.18175188324e-16, &
    -5.01605877109e-15, &
     1.34010867712e-14, &
    -1.32412212942e-14, &
     6.90818324768e-16, &
    -1.21204158580e-14, &
     7.71045132936e-14, &
    -2.12644911535e-13, &
     2.17511365387e-13, &
    -2.73602373341e-15, &
     5.38649399260e-14, &
    -3.68524898619e-13, &
     1.06795554825e-12, &
    -1.16273201995e-12  &
    /)
	get_KAPPA_WABA_DIFF_2 =  (cWABA74(0)*E**(4) + cWABA74(1)*E**(3) + cWABA74(2)*E**(2) + cWABA74(3)*E + cWABA74(4))*B**(5) + (cWABA74(5)*E**(4) + cWABA74(6)*E**(3) + cWABA74(7)*E**(2) + cWABA74(8)*E + cWABA74(9))*B**(4) + (cWABA74(10)*E**(4) + cWABA74(11)*E**(3) + cWABA74(12)*E**(2) + cWABA74(13)*E + cWABA74(14))*B**(3) + (cWABA74(15)*E**(4) + cWABA74(16)*E**(3) + cWABA74(17)*E**(2) + cWABA74(18)*E + cWABA74(19))*B**(2) + (cWABA74(20)*E**(4) + cWABA74(21)*E**(3) + cWABA74(22)*E**(2) + cWABA74(23)*E + cWABA74(24))*B + (cWABA74(25)*E**(4) + cWABA74(26)*E**(3) + cWABA74(27)*E**(2) + cWABA74(28)*E + cWABA74(29))

end function get_KAPPA_WABA_DIFF_2

double precision function get_ABS1_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA75 = (/ & 
    -4.00610977727e-06, &
     6.67686543193e-05, &
    -4.17001847616e-04, &
     1.17205451093e-03, &
    -8.57349671914e-04, &
     1.63849593601e-07, &
    -7.78644950522e-07, &
    -9.72566185704e-06, &
     1.09986213525e-04, &
    -1.03080266631e-03  &
    /)
	get_ABS1_WABA_DIFF_0 =  (cWABA75(0)*E**(4) + cWABA75(1)*E**(3) + cWABA75(2)*E**(2) + cWABA75(3)*E + cWABA75(4))*B + (cWABA75(5)*E**(4) + cWABA75(6)*E**(3) + cWABA75(7)*E**(2) + cWABA75(8)*E + cWABA75(9))

end function get_ABS1_WABA_DIFF_0

double precision function get_ABS1_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA76 = (/ & 
    -1.86664936475e-10, &
     3.17086342394e-09, &
    -1.96186412560e-08, &
     5.13306358695e-08, &
    -4.50355758248e-08, &
     1.09447587599e-08, &
    -1.85281911903e-07, &
     1.15865238502e-06, &
    -3.14511387476e-06, &
     3.04275855082e-06, &
    -2.01596621269e-07, &
     3.42735386362e-06, &
    -2.18244389130e-05, &
     6.20852601507e-05, &
    -6.77854278247e-05, &
     1.24300299538e-06, &
    -2.13215175432e-05, &
     1.39090194631e-04, &
    -4.21796555248e-04, &
     5.68968125873e-04, &
    -1.62397079251e-06, &
     2.70760114032e-05, &
    -1.70164330173e-04, &
     5.14235142993e-04, &
    -1.40387853985e-03  &
    /)
	get_ABS1_WABA_DIFF_1 =  (cWABA76(0)*E**(4) + cWABA76(1)*E**(3) + cWABA76(2)*E**(2) + cWABA76(3)*E + cWABA76(4))*B**(4) + (cWABA76(5)*E**(4) + cWABA76(6)*E**(3) + cWABA76(7)*E**(2) + cWABA76(8)*E + cWABA76(9))*B**(3) + (cWABA76(10)*E**(4) + cWABA76(11)*E**(3) + cWABA76(12)*E**(2) + cWABA76(13)*E + cWABA76(14))*B**(2) + (cWABA76(15)*E**(4) + cWABA76(16)*E**(3) + cWABA76(17)*E**(2) + cWABA76(18)*E + cWABA76(19))*B + (cWABA76(20)*E**(4) + cWABA76(21)*E**(3) + cWABA76(22)*E**(2) + cWABA76(23)*E + cWABA76(24))

end function get_ABS1_WABA_DIFF_1

double precision function get_ABS1_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA77 = (/ & 
    -1.79846951043e-12, &
     3.61202132573e-11, &
    -2.51987820726e-10, &
     7.12756777717e-10, &
    -7.24508669733e-10, &
     5.02664223322e-10, &
    -9.46233593747e-09, &
     6.35559594656e-08, &
    -1.76733273651e-07, &
     1.77919050669e-07, &
    -4.58774419954e-08, &
     8.33597554172e-07, &
    -5.47984226781e-06, &
     1.50972290619e-05, &
    -1.51362060701e-05, &
     1.65032359397e-06, &
    -2.93672386931e-05, &
     1.90687030735e-04, &
    -5.22784909913e-04, &
     5.24029163684e-04, &
    -1.94020239451e-05, &
     3.41206742038e-04, &
    -2.20049254741e-03, &
     6.00184746873e-03, &
    -6.04699183018e-03  &
    /)
	get_ABS1_WABA_DIFF_2 =  (cWABA77(0)*E**(4) + cWABA77(1)*E**(3) + cWABA77(2)*E**(2) + cWABA77(3)*E + cWABA77(4))*B**(4) + (cWABA77(5)*E**(4) + cWABA77(6)*E**(3) + cWABA77(7)*E**(2) + cWABA77(8)*E + cWABA77(9))*B**(3) + (cWABA77(10)*E**(4) + cWABA77(11)*E**(3) + cWABA77(12)*E**(2) + cWABA77(13)*E + cWABA77(14))*B**(2) + (cWABA77(15)*E**(4) + cWABA77(16)*E**(3) + cWABA77(17)*E**(2) + cWABA77(18)*E + cWABA77(19))*B + (cWABA77(20)*E**(4) + cWABA77(21)*E**(3) + cWABA77(22)*E**(2) + cWABA77(23)*E + cWABA77(24))

end function get_ABS1_WABA_DIFF_2

double precision function get_ABS2_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA78 = (/ & 
     1.36579565194e-04, &
    -2.07042252933e-03, &
     1.09995331804e-02, &
    -2.08630353824e-02, &
     3.44733091716e-02, &
    -6.01057761277e-06, &
     9.41538495988e-05, &
    -5.62758459184e-04, &
     1.24223477593e-03, &
    -1.60462593983e-02  &
    /)
	get_ABS2_WABA_DIFF_0 =  (cWABA78(0)*E**(4) + cWABA78(1)*E**(3) + cWABA78(2)*E**(2) + cWABA78(3)*E + cWABA78(4))*B + (cWABA78(5)*E**(4) + cWABA78(6)*E**(3) + cWABA78(7)*E**(2) + cWABA78(8)*E + cWABA78(9))

end function get_ABS2_WABA_DIFF_0

double precision function get_ABS2_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA79 = (/ & 
    -6.61603526101e-09, &
     9.63037915028e-08, &
    -4.85573942451e-07, &
     8.94025043226e-07, &
    -1.90137998220e-07, &
     2.72739317689e-07, &
    -4.07481017766e-06, &
     2.13943740412e-05, &
    -4.24959797806e-05, &
     1.33717428158e-05, &
    -3.54845946671e-06, &
     5.52355116781e-05, &
    -3.09838838526e-04, &
     7.01164184612e-04, &
    -3.83631459106e-04, &
     1.41546492046e-05, &
    -2.34911579862e-04, &
     1.46058786914e-03, &
    -4.03525683253e-03, &
     4.36945242632e-03, &
     4.61218619660e-06, &
    -6.54437853744e-05, &
     3.23065652497e-04, &
    -9.30697757594e-04, &
    -1.40555170427e-02  &
    /)
	get_ABS2_WABA_DIFF_1 =  (cWABA79(0)*E**(4) + cWABA79(1)*E**(3) + cWABA79(2)*E**(2) + cWABA79(3)*E + cWABA79(4))*B**(4) + (cWABA79(5)*E**(4) + cWABA79(6)*E**(3) + cWABA79(7)*E**(2) + cWABA79(8)*E + cWABA79(9))*B**(3) + (cWABA79(10)*E**(4) + cWABA79(11)*E**(3) + cWABA79(12)*E**(2) + cWABA79(13)*E + cWABA79(14))*B**(2) + (cWABA79(15)*E**(4) + cWABA79(16)*E**(3) + cWABA79(17)*E**(2) + cWABA79(18)*E + cWABA79(19))*B + (cWABA79(20)*E**(4) + cWABA79(21)*E**(3) + cWABA79(22)*E**(2) + cWABA79(23)*E + cWABA79(24))

end function get_ABS2_WABA_DIFF_1

double precision function get_ABS2_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA80 = (/ & 
     1.31521271900e-12, &
    -2.21522207220e-11, &
     1.25115960606e-10, &
    -2.61747120687e-10, &
     1.87872804924e-10, &
    -3.56141396037e-10, &
     5.97203014752e-09, &
    -3.39752791936e-08, &
     7.27579241193e-08, &
    -5.44191157491e-08, &
     3.76209067631e-08, &
    -6.28910266089e-07, &
     3.60526443414e-06, &
    -7.87689071825e-06, &
     6.07564604252e-06, &
    -1.92454726147e-06, &
     3.21680742882e-05, &
    -1.86344632755e-04, &
     4.15569024200e-04, &
    -3.29177481709e-04, &
     4.70980877465e-05, &
    -7.89772573562e-04, &
     4.64317600932e-03, &
    -1.06020441836e-02, &
     8.61850726575e-03, &
    -4.35763219683e-04, &
     7.33793501685e-03, &
    -4.38584676621e-02, &
     1.02174506567e-01, &
    -8.71226490037e-02  &
    /)
	get_ABS2_WABA_DIFF_2 =  (cWABA80(0)*E**(4) + cWABA80(1)*E**(3) + cWABA80(2)*E**(2) + cWABA80(3)*E + cWABA80(4))*B**(5) + (cWABA80(5)*E**(4) + cWABA80(6)*E**(3) + cWABA80(7)*E**(2) + cWABA80(8)*E + cWABA80(9))*B**(4) + (cWABA80(10)*E**(4) + cWABA80(11)*E**(3) + cWABA80(12)*E**(2) + cWABA80(13)*E + cWABA80(14))*B**(3) + (cWABA80(15)*E**(4) + cWABA80(16)*E**(3) + cWABA80(17)*E**(2) + cWABA80(18)*E + cWABA80(19))*B**(2) + (cWABA80(20)*E**(4) + cWABA80(21)*E**(3) + cWABA80(22)*E**(2) + cWABA80(23)*E + cWABA80(24))*B + (cWABA80(25)*E**(4) + cWABA80(26)*E**(3) + cWABA80(27)*E**(2) + cWABA80(28)*E + cWABA80(29))

end function get_ABS2_WABA_DIFF_2

double precision function get_XE2_MAC_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA81 = (/ & 
    -1.18369442232e-05, &
     2.47739006403e-04, &
    -2.52885115376e-03, &
     1.67631873036e-02, &
    -8.81957301163e-04, &
     3.64183620983e-24, &
    -6.60393765981e-23, &
     4.80006894380e-22, &
    -2.27967018680e-21, &
     1.25059739233e-20  &
    /)
	get_XE2_MAC_WABA_DIFF_0 =  (cWABA81(0)*E**(4) + cWABA81(1)*E**(3) + cWABA81(2)*E**(2) + cWABA81(3)*E + cWABA81(4))*B + (cWABA81(5)*E**(4) + cWABA81(6)*E**(3) + cWABA81(7)*E**(2) + cWABA81(8)*E + cWABA81(9))

end function get_XE2_MAC_WABA_DIFF_0

double precision function get_XE2_MAC_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA82 = (/ & 
     1.29691954096e-11, &
    -2.08580051351e-10, &
     1.25506377440e-09, &
    -3.37989873460e-09, &
     3.51181606935e-09, &
    -6.14816001427e-10, &
     9.84346540634e-09, &
    -5.88720434003e-08, &
     1.57223871704e-07, &
    -1.60302263204e-07, &
     9.96713425363e-09, &
    -1.57622921722e-07, &
     9.25277367377e-07, &
    -2.39768285114e-06, &
     2.29019186981e-06, &
    -6.28958613871e-08, &
     9.64839943598e-07, &
    -5.36418217987e-06, &
     1.24259873409e-05, &
    -8.47817249540e-06, &
     1.16445610195e-07, &
    -1.65826373362e-06, &
     7.57387952982e-06, &
    -6.51971649496e-06, &
    -2.94354647288e-05, &
     9.75999702638e-08, &
    -1.79364906143e-06, &
     1.43991562264e-05, &
    -5.95910007281e-05, &
     6.34659968253e-06  &
    /)
	get_XE2_MAC_WABA_DIFF_1 =  (cWABA82(0)*E**(4) + cWABA82(1)*E**(3) + cWABA82(2)*E**(2) + cWABA82(3)*E + cWABA82(4))*B**(5) + (cWABA82(5)*E**(4) + cWABA82(6)*E**(3) + cWABA82(7)*E**(2) + cWABA82(8)*E + cWABA82(9))*B**(4) + (cWABA82(10)*E**(4) + cWABA82(11)*E**(3) + cWABA82(12)*E**(2) + cWABA82(13)*E + cWABA82(14))*B**(3) + (cWABA82(15)*E**(4) + cWABA82(16)*E**(3) + cWABA82(17)*E**(2) + cWABA82(18)*E + cWABA82(19))*B**(2) + (cWABA82(20)*E**(4) + cWABA82(21)*E**(3) + cWABA82(22)*E**(2) + cWABA82(23)*E + cWABA82(24))*B + (cWABA82(25)*E**(4) + cWABA82(26)*E**(3) + cWABA82(27)*E**(2) + cWABA82(28)*E + cWABA82(29))

end function get_XE2_MAC_WABA_DIFF_1

double precision function get_XE2_MAC_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:19) :: cWABA83 = (/ & 
     3.76662707537e-11, &
    -5.88680876100e-10, &
     3.17336113641e-09, &
    -6.57986665046e-09, &
     4.49162085005e-09, &
    -5.45706191998e-09, &
     8.82480516825e-08, &
    -4.99567559828e-07, &
     1.11523394779e-06, &
    -8.41635208276e-07, &
     2.26981647575e-07, &
    -3.78548782307e-06, &
     2.23769212885e-05, &
    -5.32496825582e-05, &
     4.31194778273e-05, &
    -2.62343515819e-06, &
     4.44193643066e-05, &
    -2.66034981971e-04, &
     6.45273864690e-04, &
    -6.58947571732e-04  &
    /)
	get_XE2_MAC_WABA_DIFF_2 =  (cWABA83(0)*E**(4) + cWABA83(1)*E**(3) + cWABA83(2)*E**(2) + cWABA83(3)*E + cWABA83(4))*B**(3) + (cWABA83(5)*E**(4) + cWABA83(6)*E**(3) + cWABA83(7)*E**(2) + cWABA83(8)*E + cWABA83(9))*B**(2) + (cWABA83(10)*E**(4) + cWABA83(11)*E**(3) + cWABA83(12)*E**(2) + cWABA83(13)*E + cWABA83(14))*B + (cWABA83(15)*E**(4) + cWABA83(16)*E**(3) + cWABA83(17)*E**(2) + cWABA83(18)*E + cWABA83(19))

end function get_XE2_MAC_WABA_DIFF_2

double precision function get_K_INF_NO_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA84 = (/ & 
    -5.46200228817e-05, &
     9.10341970811e-04, &
    -6.11255819275e-03, &
     2.33653900041e-02, &
    -7.39514427762e-02, &
    -2.18548029379e-05, &
     4.88080456649e-04, &
    -3.17527802175e-03, &
    -5.30662441461e-03, &
     2.17771400096e-01  &
    /)
	get_K_INF_NO_XE_WABA_DIFF_0 =  (cWABA84(0)*E**(4) + cWABA84(1)*E**(3) + cWABA84(2)*E**(2) + cWABA84(3)*E + cWABA84(4))*B + (cWABA84(5)*E**(4) + cWABA84(6)*E**(3) + cWABA84(7)*E**(2) + cWABA84(8)*E + cWABA84(9))

end function get_K_INF_NO_XE_WABA_DIFF_0

double precision function get_K_INF_NO_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA85 = (/ & 
     6.59689399166e-09, &
    -1.12523044624e-07, &
     7.15850018212e-07, &
    -2.00369772261e-06, &
     2.02016474904e-06, &
    -3.32804358824e-07, &
     5.73387685355e-06, &
    -3.69813264377e-05, &
     1.05621229351e-04, &
    -1.10051394236e-04, &
     5.39854344199e-06, &
    -9.38906462653e-05, &
     6.13281737579e-04, &
    -1.78496874249e-03, &
     1.91701335935e-03, &
    -2.74916417896e-05, &
     4.76760576958e-04, &
    -3.08549483787e-03, &
     8.78013267547e-03, &
    -8.75825779798e-03, &
     2.01201735707e-06, &
     5.57237315105e-05, &
    -1.59565842276e-03, &
     1.31190265676e-02, &
    -4.49716684969e-02, &
    -4.33877958654e-05, &
     8.59057481470e-04, &
    -5.62019564700e-03, &
     2.08823413767e-03, &
     2.09891371747e-01  &
    /)
	get_K_INF_NO_XE_WABA_DIFF_1 =  (cWABA85(0)*E**(4) + cWABA85(1)*E**(3) + cWABA85(2)*E**(2) + cWABA85(3)*E + cWABA85(4))*B**(5) + (cWABA85(5)*E**(4) + cWABA85(6)*E**(3) + cWABA85(7)*E**(2) + cWABA85(8)*E + cWABA85(9))*B**(4) + (cWABA85(10)*E**(4) + cWABA85(11)*E**(3) + cWABA85(12)*E**(2) + cWABA85(13)*E + cWABA85(14))*B**(3) + (cWABA85(15)*E**(4) + cWABA85(16)*E**(3) + cWABA85(17)*E**(2) + cWABA85(18)*E + cWABA85(19))*B**(2) + (cWABA85(20)*E**(4) + cWABA85(21)*E**(3) + cWABA85(22)*E**(2) + cWABA85(23)*E + cWABA85(24))*B + (cWABA85(25)*E**(4) + cWABA85(26)*E**(3) + cWABA85(27)*E**(2) + cWABA85(28)*E + cWABA85(29))

end function get_K_INF_NO_XE_WABA_DIFF_1

double precision function get_K_INF_NO_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA86 = (/ & 
    -1.10847164032e-11, &
     1.80807514568e-10, &
    -9.75344778264e-10, &
     1.83556695781e-09, &
    -1.04730424066e-09, &
     2.86103573170e-09, &
    -4.74412632792e-08, &
     2.63036214042e-07, &
    -5.22687285239e-07, &
     3.31239615524e-07, &
    -2.80644035732e-07, &
     4.74163744275e-06, &
    -2.70307082580e-05, &
     5.62848957480e-05, &
    -3.86034488302e-05, &
     1.28706000178e-05, &
    -2.22060226414e-04, &
     1.30111971700e-03, &
    -2.81202007692e-03, &
     2.04235728725e-03, &
    -2.73023568014e-04, &
     4.82062606655e-03, &
    -2.89998687042e-02, &
     6.41938237603e-02, &
    -4.82538474175e-02, &
     2.16702974783e-03, &
    -3.93302124150e-02, &
     2.43496100557e-01, &
    -5.43567225477e-01, &
     4.03742621656e-01  &
    /)
	get_K_INF_NO_XE_WABA_DIFF_2 =  (cWABA86(0)*E**(4) + cWABA86(1)*E**(3) + cWABA86(2)*E**(2) + cWABA86(3)*E + cWABA86(4))*B**(5) + (cWABA86(5)*E**(4) + cWABA86(6)*E**(3) + cWABA86(7)*E**(2) + cWABA86(8)*E + cWABA86(9))*B**(4) + (cWABA86(10)*E**(4) + cWABA86(11)*E**(3) + cWABA86(12)*E**(2) + cWABA86(13)*E + cWABA86(14))*B**(3) + (cWABA86(15)*E**(4) + cWABA86(16)*E**(3) + cWABA86(17)*E**(2) + cWABA86(18)*E + cWABA86(19))*B**(2) + (cWABA86(20)*E**(4) + cWABA86(21)*E**(3) + cWABA86(22)*E**(2) + cWABA86(23)*E + cWABA86(24))*B + (cWABA86(25)*E**(4) + cWABA86(26)*E**(3) + cWABA86(27)*E**(2) + cWABA86(28)*E + cWABA86(29))

end function get_K_INF_NO_XE_WABA_DIFF_2

double precision function get_XE2_MIC_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA87 = (/ & 
     3.64306448856e+02, &
    -7.14009166629e+03, &
     5.53258951082e+04, &
    -2.05348007252e+05, &
     8.33846310891e+04, &
    -1.82108163395e+02, &
     2.98171825765e+03, &
    -1.79995771292e+04, &
     4.51636051118e+04, &
    -1.11390639460e+04  &
    /)
	get_XE2_MIC_WABA_DIFF_0 =  (cWABA87(0)*E**(4) + cWABA87(1)*E**(3) + cWABA87(2)*E**(2) + cWABA87(3)*E + cWABA87(4))*B + (cWABA87(5)*E**(4) + cWABA87(6)*E**(3) + cWABA87(7)*E**(2) + cWABA87(8)*E + cWABA87(9))

end function get_XE2_MIC_WABA_DIFF_0

double precision function get_XE2_MIC_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA88 = (/ & 
    -3.81597091687e-02, &
     6.59974999648e-01, &
    -4.29665091761e+00, &
     1.25482652302e+01, &
    -1.39970522981e+01, &
     1.34256189377e+00, &
    -2.32271664629e+01, &
     1.52035580193e+02, &
    -4.50345169780e+02, &
     5.18286797815e+02, &
    -1.42843877977e+01, &
     2.45007648567e+02, &
    -1.60037447269e+03, &
     4.79170433666e+03, &
    -5.74768285065e+03, &
     5.96355702851e+01, &
    -9.77752513558e+02, &
     6.06746399536e+03, &
    -1.71665343919e+04, &
     1.97756528869e+04, &
    -1.04771839602e+02, &
     1.65526818949e+03, &
    -9.56733877368e+03, &
     2.16483512184e+04, &
     1.34161221429e+04  &
    /)
	get_XE2_MIC_WABA_DIFF_1 =  (cWABA88(0)*E**(4) + cWABA88(1)*E**(3) + cWABA88(2)*E**(2) + cWABA88(3)*E + cWABA88(4))*B**(4) + (cWABA88(5)*E**(4) + cWABA88(6)*E**(3) + cWABA88(7)*E**(2) + cWABA88(8)*E + cWABA88(9))*B**(3) + (cWABA88(10)*E**(4) + cWABA88(11)*E**(3) + cWABA88(12)*E**(2) + cWABA88(13)*E + cWABA88(14))*B**(2) + (cWABA88(15)*E**(4) + cWABA88(16)*E**(3) + cWABA88(17)*E**(2) + cWABA88(18)*E + cWABA88(19))*B + (cWABA88(20)*E**(4) + cWABA88(21)*E**(3) + cWABA88(22)*E**(2) + cWABA88(23)*E + cWABA88(24))

end function get_XE2_MIC_WABA_DIFF_1

double precision function get_XE2_MIC_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA89 = (/ & 
     2.61586951017e-04, &
    -4.18890923825e-03, &
     2.43668060301e-02, &
    -6.06382661314e-02, &
     5.72450634414e-02, &
    -5.72468030749e-02, &
     9.17637620405e-01, &
    -5.34578048880e+00, &
     1.32927392196e+01, &
    -1.24378860938e+01, &
     4.62598106257e+00, &
    -7.43631266466e+01, &
     4.35412092545e+02, &
    -1.08787673192e+03, &
     1.01468333511e+03, &
    -1.56072044208e+02, &
     2.51728369895e+03, &
    -1.48439187672e+04, &
     3.74286291286e+04, &
    -3.50134389898e+04, &
     1.76835322608e+03, &
    -2.85577034389e+04, &
     1.69077839999e+05, &
    -4.28775662944e+05, &
     4.19588834450e+05  &
    /)
	get_XE2_MIC_WABA_DIFF_2 =  (cWABA89(0)*E**(4) + cWABA89(1)*E**(3) + cWABA89(2)*E**(2) + cWABA89(3)*E + cWABA89(4))*B**(4) + (cWABA89(5)*E**(4) + cWABA89(6)*E**(3) + cWABA89(7)*E**(2) + cWABA89(8)*E + cWABA89(9))*B**(3) + (cWABA89(10)*E**(4) + cWABA89(11)*E**(3) + cWABA89(12)*E**(2) + cWABA89(13)*E + cWABA89(14))*B**(2) + (cWABA89(15)*E**(4) + cWABA89(16)*E**(3) + cWABA89(17)*E**(2) + cWABA89(18)*E + cWABA89(19))*B + (cWABA89(20)*E**(4) + cWABA89(21)*E**(3) + cWABA89(22)*E**(2) + cWABA89(23)*E + cWABA89(24))

end function get_XE2_MIC_WABA_DIFF_2

double precision function get_SM2_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA90 = (/ & 
     5.12379243102e-04, &
    -1.79695272401e+01, &
     3.56901056617e+02, &
    -2.43410493496e+03, &
    -8.25150543571e+02, &
     3.82427287478e+00, &
    -6.16982723450e+01, &
     3.70910813268e+02, &
    -1.03628387257e+03, &
     1.87672056641e+03  &
    /)
	get_SM2_XE_WABA_DIFF_0 =  (cWABA90(0)*E**(4) + cWABA90(1)*E**(3) + cWABA90(2)*E**(2) + cWABA90(3)*E + cWABA90(4))*B + (cWABA90(5)*E**(4) + cWABA90(6)*E**(3) + cWABA90(7)*E**(2) + cWABA90(8)*E + cWABA90(9))

end function get_SM2_XE_WABA_DIFF_0

double precision function get_SM2_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA91 = (/ & 
     3.01264901024e-04, &
    -4.32347765545e-03, &
     2.05453728941e-02, &
    -2.82227209947e-02, &
    -2.79412672914e-02, &
    -1.58765559725e-02, &
     2.36010615913e-01, &
    -1.20240994956e+00, &
     2.07551922275e+00, &
     4.94792434717e-01, &
     2.73950944919e-01, &
    -4.23370546767e+00, &
     2.31457611482e+01, &
    -4.78284617154e+01, &
     1.14367378441e+01, &
    -1.61200079965e+00, &
     2.60256915705e+01, &
    -1.53538039010e+02, &
     3.75193075083e+02, &
    -2.44699929187e+02, &
     1.33086508777e+00, &
    -2.14867118718e+01, &
     1.30661129782e+02, &
    -3.96786477392e+02, &
     1.20709525373e+03  &
    /)
	get_SM2_XE_WABA_DIFF_1 =  (cWABA91(0)*E**(4) + cWABA91(1)*E**(3) + cWABA91(2)*E**(2) + cWABA91(3)*E + cWABA91(4))*B**(4) + (cWABA91(5)*E**(4) + cWABA91(6)*E**(3) + cWABA91(7)*E**(2) + cWABA91(8)*E + cWABA91(9))*B**(3) + (cWABA91(10)*E**(4) + cWABA91(11)*E**(3) + cWABA91(12)*E**(2) + cWABA91(13)*E + cWABA91(14))*B**(2) + (cWABA91(15)*E**(4) + cWABA91(16)*E**(3) + cWABA91(17)*E**(2) + cWABA91(18)*E + cWABA91(19))*B + (cWABA91(20)*E**(4) + cWABA91(21)*E**(3) + cWABA91(22)*E**(2) + cWABA91(23)*E + cWABA91(24))

end function get_SM2_XE_WABA_DIFF_1

double precision function get_SM2_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:24) :: cWABA92 = (/ & 
    -4.86867344926e-07, &
     5.19721814933e-06, &
    -2.60349801082e-05, &
     9.66145593972e-05, &
    -9.74927937340e-05, &
     9.95040568285e-05, &
    -1.07295017627e-03, &
     5.62525448425e-03, &
    -2.24637802355e-02, &
     2.59958652015e-02, &
    -3.65772139775e-03, &
     1.42936075567e-02, &
     5.52510183080e-03, &
     5.52240406161e-01, &
    -9.83472314576e-01, &
    -3.67797100364e-02, &
     2.27812503453e+00, &
    -1.88926329161e+01, &
     3.76861341416e+01, &
    -2.38445869088e+01, &
     1.21491236063e+00, &
    -3.94155246784e+01, &
     3.07249580170e+02, &
    -7.12463121335e+02, &
     1.02904389439e+03  &
    /)
	get_SM2_XE_WABA_DIFF_2 =  (cWABA92(0)*E**(4) + cWABA92(1)*E**(3) + cWABA92(2)*E**(2) + cWABA92(3)*E + cWABA92(4))*B**(4) + (cWABA92(5)*E**(4) + cWABA92(6)*E**(3) + cWABA92(7)*E**(2) + cWABA92(8)*E + cWABA92(9))*B**(3) + (cWABA92(10)*E**(4) + cWABA92(11)*E**(3) + cWABA92(12)*E**(2) + cWABA92(13)*E + cWABA92(14))*B**(2) + (cWABA92(15)*E**(4) + cWABA92(16)*E**(3) + cWABA92(17)*E**(2) + cWABA92(18)*E + cWABA92(19))*B + (cWABA92(20)*E**(4) + cWABA92(21)*E**(3) + cWABA92(22)*E**(2) + cWABA92(23)*E + cWABA92(24))

end function get_SM2_XE_WABA_DIFF_2

double precision function get_K_INF_XE_WABA_DIFF_0(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:9) :: cWABA93 = (/ & 
     3.09594920067e-04, &
    -6.76247350709e-03, &
     5.87209561442e-02, &
    -2.21750880948e-01, &
    -9.54403513778e-02, &
    -2.18548029379e-05, &
     4.88080456649e-04, &
    -3.17527802175e-03, &
    -5.30662441461e-03, &
     2.17771400096e-01  &
    /)
	get_K_INF_XE_WABA_DIFF_0 =  (cWABA93(0)*E**(4) + cWABA93(1)*E**(3) + cWABA93(2)*E**(2) + cWABA93(3)*E + cWABA93(4))*B + (cWABA93(5)*E**(4) + cWABA93(6)*E**(3) + cWABA93(7)*E**(2) + cWABA93(8)*E + cWABA93(9))

end function get_K_INF_XE_WABA_DIFF_0

double precision function get_K_INF_XE_WABA_DIFF_1(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA94 = (/ & 
     4.85740088945e-09, &
    -8.37027317404e-08, &
     5.38469847739e-07, &
    -1.52432546266e-06, &
     1.54502724955e-06, &
    -2.47181819314e-07, &
     4.30992837045e-06, &
    -2.81740728967e-05, &
     8.16526622855e-05, &
    -8.60502436046e-05, &
     3.92775278286e-06, &
    -6.93723983807e-05, &
     4.61106938864e-04, &
    -1.36846179891e-03, &
     1.49584836988e-03, &
    -1.70156903188e-05, &
     3.02820923171e-04, &
    -2.01151779753e-03, &
     5.86231906348e-03, &
    -5.84957937021e-03, &
    -2.76281595598e-05, &
     5.39374778656e-04, &
    -4.49418755165e-03, &
     2.04698240007e-02, &
    -5.06072979334e-02, &
    -4.60734492927e-06, &
     1.89938499002e-04, &
    -1.28383228299e-03, &
    -9.59148615887e-03, &
     2.10421889767e-01  &
    /)
	get_K_INF_XE_WABA_DIFF_1 =  (cWABA94(0)*E**(4) + cWABA94(1)*E**(3) + cWABA94(2)*E**(2) + cWABA94(3)*E + cWABA94(4))*B**(5) + (cWABA94(5)*E**(4) + cWABA94(6)*E**(3) + cWABA94(7)*E**(2) + cWABA94(8)*E + cWABA94(9))*B**(4) + (cWABA94(10)*E**(4) + cWABA94(11)*E**(3) + cWABA94(12)*E**(2) + cWABA94(13)*E + cWABA94(14))*B**(3) + (cWABA94(15)*E**(4) + cWABA94(16)*E**(3) + cWABA94(17)*E**(2) + cWABA94(18)*E + cWABA94(19))*B**(2) + (cWABA94(20)*E**(4) + cWABA94(21)*E**(3) + cWABA94(22)*E**(2) + cWABA94(23)*E + cWABA94(24))*B + (cWABA94(25)*E**(4) + cWABA94(26)*E**(3) + cWABA94(27)*E**(2) + cWABA94(28)*E + cWABA94(29))

end function get_K_INF_XE_WABA_DIFF_1

double precision function get_K_INF_XE_WABA_DIFF_2(B, E)
  double precision, intent(in) :: B, E
  double precision, dimension(0:29) :: cWABA95 = (/ & 
    -5.81189226249e-12, &
     9.58045477405e-11, &
    -4.72353546468e-10, &
     5.48732567895e-10, &
     1.75691388114e-10, &
     1.65338209323e-09, &
    -2.79681406175e-08, &
     1.47886346218e-07, &
    -2.29030828552e-07, &
     5.35476504821e-08, &
    -1.76031025653e-07, &
     3.05234207819e-06, &
    -1.70376729290e-05, &
     3.08745039902e-05, &
    -1.46945092980e-05, &
     8.66103569321e-06, &
    -1.53893802589e-04, &
     8.97310809085e-04, &
    -1.78862706305e-03, &
     1.08496563735e-03, &
    -1.95983157096e-04, &
     3.56729459168e-03, &
    -2.15570790652e-02, &
     4.54364354987e-02, &
    -3.08580804071e-02, &
     1.66065363019e-03, &
    -3.10141083197e-02, &
     1.93828064316e-01, &
    -4.19782851755e-01, &
     2.91294892580e-01  &
    /)
	get_K_INF_XE_WABA_DIFF_2 =  (cWABA95(0)*E**(4) + cWABA95(1)*E**(3) + cWABA95(2)*E**(2) + cWABA95(3)*E + cWABA95(4))*B**(5) + (cWABA95(5)*E**(4) + cWABA95(6)*E**(3) + cWABA95(7)*E**(2) + cWABA95(8)*E + cWABA95(9))*B**(4) + (cWABA95(10)*E**(4) + cWABA95(11)*E**(3) + cWABA95(12)*E**(2) + cWABA95(13)*E + cWABA95(14))*B**(3) + (cWABA95(15)*E**(4) + cWABA95(16)*E**(3) + cWABA95(17)*E**(2) + cWABA95(18)*E + cWABA95(19))*B**(2) + (cWABA95(20)*E**(4) + cWABA95(21)*E**(3) + cWABA95(22)*E**(2) + cWABA95(23)*E + cWABA95(24))*B + (cWABA95(25)*E**(4) + cWABA95(26)*E**(3) + cWABA95(27)*E**(2) + cWABA95(28)*E + cWABA95(29))

end function get_K_INF_XE_WABA_DIFF_2


end module nuclear_data_WABA
