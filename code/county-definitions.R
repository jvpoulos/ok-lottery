## County definitions

# OK lottery counties

# Wichita & Caddo: 0110 Blaine; 0150 Caddo; 0170 Canadian; 0390 Custer; 1490 Washita; 8050 Wichita Res (NB: Canadian also contains land run )

# Comanche, Kiowa, and Apache: 0310 Comanche; 0750 Kiowa; 8030 Kiowa/Comanche/Apache Res (NB: Comanche also contains land by sealed bid (big pasture))

ok.lottery <- c(110,150,170,390,1490,310,750,8030,8050)

# OK allotment counties

# Osage:  1130 Osage; 8040 Osage Res
# Kaw:  0710 Kay; 8020 Kaw res
# Ponca: 0710 Kay; 1030 Noble; 8060 Ponca/Otoe Res
# Tonkawa: 0710 Kay
# Oto-Missouri: 1030 Noble; 1170 Pawnee
# Pawnee: 1170 Pawnee

ok.allotment <- c(1130,710,1030,1170,8020,8040,8060)

# OK sealed bid

# Big pasture: 0310 Comanche; 0330 Cotton; 1410 Tillman 

ok.sealed <- c(330,1410) #(NB: Comanche counted as land lottery lottery)

# OK land run

# Unassigned lands: 0170 Canadian; 0270 Cleveland; 0730 Kingfisher; 0830 Logan; 1090 Oklahoma; 1190 Payne 
# Iowa / Sac & Fox / - Pottawatomie & Shawnee /Kickapoo: 1250 Pottawattamie; 0270 Cleveland; 0810 Lincoln; 1190 Payne; 1090 Oklahoma
# Cheyenne & Arapaho: 0430 Dewey; 0390 Custer; 1290 Roger Mills; 0090 Beckham 
# Cherokee Outlet: 0530 Grant; 0470 Garfield; 0030 Alfalfa; 0930 Major; 1510 Woods; 1530 Woodward; 0590 Harper; 0450 Ellis

ok.run <- c(270,730,830,1090,1190,1250,810,430,390,1290,90,530,470,30,930,1510,1530,590,450) # (NB: Canadian counted as land lottery )