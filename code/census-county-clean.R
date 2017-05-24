#####################
### County-level censuses   ###
#####################

# Load data, subset to OK (53), TX (49), KS (32)

years <- c(seq(1890,1940,10))

census.county <- sapply(years, function(i) {
  census.county.i <- read.csv(paste0(data.directory,"census-county/",i,".csv"), stringsAsFactors=FALSE)
  census.county.i <- census.county.i[census.county.i$state==53 | census.county.i$state==49 | census.county.i$state==32,] # ICPSR codes
  census.county.i <- census.county.i[census.county.i$county!=0,] # remove state total
#  census.county.i$year <- i # add year variable
}
)
names(census.county) <- years

# OK lottery counties

# Wichita & Caddo: 0110 Blaine; 0150 Caddo; 0170 Canadian; 0390 Custer; 1490 Washita (NB: Canadian also contains land run )

# Comanche, Kiowa, and Apache: 0310 Comanche; 0750 Kiowa (NB: Comanche also contains land by sealed bid (big pasture))

ok.lottery <- c(110,150,170,390,1490,310,750)

# OK allotment counties

# Osage:  1130 Osage
# Kaw:  0710 Kay
# Ponca: 0710 Kay; 1030 Noble
# Tonkawa: 0710 Kay
# Oto-Missouri: 1030 Noble; 1170 Pawnee
# Pawnee: 1170 Pawnee

ok.allotment <- c(1130,710,1030,1170)

# OK sealed bid

# Big pasture: 0310 Comanche; 0330 Cotton; 1410 Tillman

ok.sealed <- c(310,330,1410)

# OK land run

# Unassigned lands: 0170 Canadian; 0270 Cleveland; 0730 Kingfisher; 0830 Logan; 1090 Oklahoma; 1190 Payne
# Iowa / Sac & Fox / - Pottawatomie & Shawnee /Kickapoo: 1250 Pottawattamie; 0270 Cleveland; 0810 Lincoln; 1190 Payne; 1090 Oklahoma
# Cheyenne & Arapaho: 0430 Dewey; 0390 Custer; 1290 Roger Mills; 0090 Beckham 
# Cherokee Outlet: 0530 Grant; 0470 Garfield; 0030 Alfalfa; 0930 Major; 1510 Woods; 1530 Woodward; 0590 Harper; 0450 Ellis

ok.run <- c(170,270,730,830,1090,1190,1250,810,430,390,1290,90,530,470,30,930,1510,1530,590,450)
