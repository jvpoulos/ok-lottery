#####################################
### Load and clean participant data ###
#####################################

## Load and clean lawton
lawtonA <- read.csv(paste0(data.directory,"lawton-A.csv"),
                    header=TRUE, sep = ",",
                    stringsAsFactors=FALSE)
lawtonB <- read.csv(paste0(data.directory,"lawton-B.csv"),
                    header=TRUE, sep = ",",
                    stringsAsFactors=FALSE)

lawtonA$Drawing..[!is.na(lawtonA$Drawing) & lawtonA$Drawing..<1] <- NA # # make missign draw # missing

# Clean Lawton A
lawtonA <- CleanLawton(lawtonA)

# Manually fix names
lawtonA$middle.name[lawtonA$Drawing==2 & lawtonA$first==""] <- "N."
lawtonA$first[lawtonA$Drawing==2 & lawtonA$first==""] <- "Mattie"
lawtonA$middle.name[lawtonA$Drawing==4572 & lawtonA$first==""] <- NA
lawtonA$first[lawtonA$Drawing==4572 & lawtonA$first==""] <- "Ashburry"

lawtonA$middle.name[lawtonA$middle.name==""] <- NA

lawtonA$Surname[lawtonA$Surname=="" & lawtonA$Drawing==430] <- "BURDETT"

# Manually fix state
lawtonA$State.or.Territory <- gsub("Acres","Indian Territory",lawtonA$State.or.Territory)
lawtonA$State.or.Territory <- gsub("D C  Also Lists Lawton  OT As Town","Washington, D.C.",lawtonA$State.or.Territory)
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="Indian"] <- "Indian Territory"
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="Indian Territory  Indian Territory"] <- "Indian Territory"
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="Indian Trritory"] <- "Indian Territory"
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="Indtan Territory"] <- "Indian Territory"
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="NANA"] <- NA
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="Kasnas"] <- "Kansas"
lawtonA$State.or.Territory[lawtonA$State.or.Territory=="D C  Alabama"] <- "Alabama"

# Clean Lawton B
lawtonB <- CleanLawton(lawtonB)

# Manually fix names
lawtonB$Surname[lawtonB$Surname=="" & lawtonB$Drawing==5429] <- "LINDLEY"
lawtonB$first[lawtonB$Surname=="" & lawtonB$Drawing==5906] <- "JAMES"
lawtonB$middle.name[lawtonB$Surname=="" & lawtonB$Drawing==5906] <- "W."
lawtonB$Surname[lawtonB$Surname=="" & lawtonB$Drawing==5906] <- "BLACK"

# Manually fix state
lawtonB$State.or.Territory[lawtonB$State.or.Territory==""] <- NA

# Append lawton A and B
lawton <- rbind(lawtonA,lawtonB)

lawton$comply <- ifelse(!is.na(lawton$Reg..),1,0) # create compliance indicator

# Verify no drawing # duplicates
lawton$Drawing..[!is.na(lawton$Drawing..) & duplicated(lawton$Drawing..)]

## Load and clean el reno
elreno <- read.csv(paste0(data.directory,"el-reno-homesteader.csv"),
                   header=TRUE, 
                   sep = ",",
                   stringsAsFactors=FALSE)

# Remove soldier's declatory filings
elreno <- elreno[grep("Soldier",elreno$Additional.Info, invert=TRUE),] 

# Watson list has 7731 excluding SDF (313)
# append those missing 

elreno <- rbind(elreno, c(940, "Elmer W. Whitlow", "Commerce", "Texas",""))
elreno <- rbind(elreno, c(3698, "Cyrus Roberts", "Alvord", "Texas",""))
elreno <- rbind(elreno, c(3721, "Chas. H. Smith", "Kinsley", "Kansas",""))
elreno <- rbind(elreno, c(3739, "Chas. W. Wiles", "Carney", "O.T.",""))
elreno <- rbind(elreno, c(3794, "Chas. J. W. Zimmerman", "Blackwell", "O.T.",""))
elreno <- rbind(elreno, c(3917, "H. Land", "El Reno", "O.T.",""))
elreno <- rbind(elreno, c(4053, "Chas. E. Smith", "Oklahoma City", "O.T.",""))
elreno <- rbind(elreno, c(4149, "Chas. Van Meter", "Leon", "Iowa",""))
elreno <- rbind(elreno, c(4179, "Chas. s. Frazier", "Eldrid", "Kansas",""))
elreno <- rbind(elreno, c(5667, "Edgar F. Brown,", "Combs", "O.T.",""))
elreno <- rbind(elreno, c(6475, "James C. Rice,", "Harrison", "O.T.",""))
elreno <- rbind(elreno, c(6476, "Andrew Guthrie", "Harrison", "O.T.",""))
elreno <- rbind(elreno, c(6615, "Nobleboy Conklin", "Cropper", "O.T.",""))
elreno <- rbind(elreno, c(7115, "Chas. H. Smith,", "Hobart", "O.T.",""))
elreno <- rbind(elreno, c(7510, "Abraham Zahb", "Hydro", "O.T.",""))

# Clean el reno
elreno <- CleanElreno(elreno)

# Verify no drawing # duplicates
elreno$Number <- as.numeric(elreno$Number)

elreno$Number[!is.na(elreno$Number) & duplicated(elreno$Number)]
