Capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

SimpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

FreqFunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

StFirst <- function(first) {
  first  <-gsub("PAT$","PATRICK", first)
  first  <-gsub("DANL$","DANIEL", first)
  first  <-gsub("RICHD$","RICHARD", first)
  first  <-gsub("CHAS$","CHARLES", first)
  first  <-gsub("JOS$","JOSEPH", first)
  first  <-gsub("BENJ$","BENJAMIN", first)
  first  <-gsub("SAML$","SAMUEL", first)
  first  <-gsub("SAM$","SAMUEL", first)
  first  <-gsub("ROBT$","ROBERT", first)
  first  <-gsub("GEO$","GEORGE", first)
  first  <-gsub("JNO$","JOHN", first)
  first  <-gsub("JAS$","JAMES", first)
  first  <-gsub("THOS$","THOMAS", first)
  first  <-gsub("WM$","WILLIAM", first)
  first  <-gsub("ARTHER$","ARTHUR", first)
  first  <-gsub("WESLY$","WESLEY", first)
  first  <-gsub("CHRIS$","CHRISTOPHER", first)
  first  <-gsub("CHARLEY$","CHARLES", first)
  first  <-gsub("ANT$","ANTHONY", first)
  first  <-gsub("SANDY$","WILLIAM", first)
  first  <-gsub("ELIGAH$","ELIJAH", first)
  first  <-gsub("BENJM$","BENJAMIN", first)
  first  <-gsub("ZACH$","ZACHARY", first)
  first  <-gsub("ELIJA$","ELIJAH", first)
  first  <-gsub("SAMUL$","SAMUEL", first)
  first  <-gsub("ISAMUEL$","SAMUEL", first)
  first  <-gsub("JN$","JOHN", first)
  first  <-gsub("WILIE$","WILLIAM", first)
  first  <-gsub("SAME$","SAMUEL", first)
  first  <-gsub("FREDK$","FREDERICK", first)
  first  <-gsub("JIM$","JAMES", first)
  first  <-gsub("HARVY$","HARVEY", first)
  first  <-gsub("MICH$","MITCHELL", first)
  first  <-gsub("MITCHELL$","MITCHELL", first)
  first  <-gsub("MAT$","MATTHEW", first)
  first  <-gsub("ISAC$","ISAAC", first)
  first  <-gsub("NATHL$","NATHANIEL", first)
  first  <-gsub("TIM$","TIMOTHY", first)
  first  <-gsub("CH$","CHARLES", first)
  first  <-gsub("THEO$","THEODORE", first)
  first  <-gsub("MICHL$","MICHAEL", first)
  first  <-gsub("RUSSEL$","RUSSELL", first)
  first  <-gsub("NIELL$","NIEL", first)
  first  <-gsub("RICH$","RICHARD", first)
  first  <-gsub("ALX$","ALEX", first)
  first  <-gsub("FREDERIC$","FREDERICK", first)
  first  <-gsub("ROB$","ROBERT", first)
  first  <-gsub("JEFF$","JEFFERY", first)
  first  <-gsub("AUG$","AUGUSTUS", first)
  first  <-gsub("SOL$","SOLOMON", first)
  first  <-gsub("NAT$","NATHANIEL", first)
  first  <-gsub("WASH$","WASHINGTON", first)
  first  <-gsub("WILLIAN$","WILLIAM", first)
  first  <-gsub("JAME$","JAMES", first)
  first  <-gsub("ARCHD$","ARCHIBALD", first)
  first  <-gsub("GEORG$","GEORGE", first)
  first  <-gsub("ARCH$","ARCHIBALD", first)
  first  <-gsub("DAN$","DANIEL", first)
  first  <-gsub("WILL$","WILLIAM", first)
  first  <-gsub("BENJA$","BENJAMIN", first)
  first  <-gsub("MICHEL$","MICHAEL", first)
  first  <-gsub("JOE$","JOSEPH", first)
  first  <-gsub("THO$","THOMAS", first)
  first  <-gsub("ALEXR$","ALEXANDER", first)
  first  <-gsub("JO$","JOHN", first)
  first  <-gsub("JANES$","JAMES", first)
  first  <-gsub("MIKE$","MICHAEL", first)
  first  <-gsub("BENJN$","BENJAMIN", first)
  first  <-gsub("CHS$","CHRIS", first)
  first  <-gsub("JOHNATHAN$","JONATHAN", first)
  first  <-gsub("JACK$","JOHN", first)
  first  <-gsub("EDWD$","EDWARD", first)
}

StCounty <- function(county) {
  county <- gsub("[[:punct:]]", " ", county)
  county <- trimws(proper(Capwords(gsub("[^[:alpha:] ]", "",county))))
  county <- gsub("Petersburg  Independent City", "Petersburg", county)
  county <- gsub("Portsmouth  Independent City", "Portsmouth", county)
  county <- gsub("Alexandria  Independent City", "Alexandria", county)
  county <- gsub("Beckham.*", "Beckham", county)
  county <- gsub("Blaine.*", "Blaine", county)
  county <- gsub("Canadian.*", "Canadian", county)
  county <- gsub("Cleveland.*", "Cleveland", county)
  county <- gsub("Dewey.*", "Dewey", county)
  county <- gsub("Ellis.*", "Ellis", county)
  county <- gsub("Garfield.*", "Garfield", county)
  county <- gsub("Greer.*", "Greer", county)
  county <- gsub("Harper.*", "Harper", county)
  county <- gsub("Jackson.*", "Jackson", county)
  county <- gsub("Kay.*", "Kay", county)
  county <- gsub("Kingfisher.*", "Kingfisher", county)
  county <- gsub("Kiowa.*", "Kiowa", county)
  county <- gsub("Logan.*", "Logan", county)
  county <- gsub("Major.*", "Major", county)
  county <- gsub("Osage.*", "Osage", county)
  county <- gsub("Pontotoc.*", "Pontotoc", county)
  county <- gsub("Pottawatomi.*", "Pottawatomie", county)
  county <- gsub("Woods.*", "Woods", county)
  county <- gsub("Roger.*", "Rorger Mills", county)
}

StState <- function(state) {
  state <- gsub("[[:punct:]]", " ", state)
  state <- trimws(proper(Capwords(gsub("[^[:alpha:] ]", "",state))))
  state[state==""] <- NA # make blank missing
  state <- gsub("Al.*", "Alabama", state)
  state <- gsub("Ariz.*", "Arizona", state)
  state <- gsub("Ark.*", "Arkansas", state)
  state <- gsub("Aransas", "Arkansas", state)
  state <- gsub("Austria.*", "Austria", state)
  state <- gsub("At Sea.*", "At Sea", state)
  state <- gsub("Cal.*", "California", state)
  state <- gsub("Canada.*", "Canada", state)
  state <- gsub("Ontario.*", "Canada", state)
  state <- gsub("Colo.*", "Colorado", state)
  state <- gsub("Ga.*", "Georgia", state)
  state <- gsub("Wales*", "England", state)
  state <- gsub("Wurtt*", "Germany", state)
  state <- gsub("Bavaria.*", "Germany", state)
  state <- gsub("Bohemia.*", "Bohemia", state)
  state <- gsub("Indian territory.*", "Indian Territory", state)
  state <- gsub("Indain Territory", "Indian Territory", state)
  state <- gsub("I T", "Indian Territory", state)
  state <- gsub("Cher.*", "Indian Territory", state)
  state <- gsub("Ok.*", "Oklahoma Territory", state)
  state <- gsub("O T", "Oklahoma Territory", state)
  state <- gsub("Chactaw.*", "Indian Territory", state)
  state <- gsub("Choc.*", "Indian Territory", state)
  state <- gsub("Chick.*", "Indian Territory", state)
  state <- gsub("Creek.*", "Indian Territory", state)
  state <- gsub("Kaw.*", "Indian Territory", state)
  state <- gsub("I T", "Indian Territory", state)
  state <- gsub("Indian T.*", "Indian Territory", state)
  state <- gsub("Indian .*", "Indian Territory", state)
  state <- gsub("Native.*", "Indian Territory", state)
  state <- gsub("Cincinnati", "Ohio", state)
  state <- gsub("Con.*", "Connecticut", state)
  state <- gsub("Ia$", "Iowa", state)
  state <- gsub("Ill.*", "Illinois", state)
  state <- gsub("Ileinison", "Illinois", state)
  state <- gsub("Ilenoise", "Illinois", state)
  state <- gsub("Iindia", "Indiana", state)
  state <- gsub("Iindiana", "Indiana", state)
  state <- gsub("Imdiiana", "Indiana", state)
  state <- gsub("Ind$", "Indiana", state)
  state <- gsub("Kan.*", "Kansas", state)
  state <- gsub("Kent.*", "Kentucky", state)
  state <- gsub("Ky", "Kentucky", state)
  state <- gsub("Flor.*", "Florida", state)
  state <- gsub("Gem.*", "Germany", state)
  state <- gsub("Ger.*", "Germany", state)
  state <- gsub("Saxony*", "Germany", state)
  state <- gsub("Silesia.*", "Germany", state)
  state <- gsub("Switzerland*", "Switzerland", state)
  state <- gsub("Syria.*", "Syria", state)
  state <- gsub("Hesse.*", "Germany", state)
  state <- gsub("Hungary.*", "Hungary", state)
  state <- gsub("Geo.*", "Georgia", state)
  state <- gsub("La$", "Louisiana", state)
  state <- gsub("Boston.*", "Massachusetts", state)
  state <- gsub("Mass.*", "Massachusetts", state)
  state <- gsub("Md.*", "Maryland", state)
  state <- gsub("Mich.*", "Michigan", state)
  state <- gsub("Minn.*", "Minnesota", state)
  state <- gsub("Miss$", "Mississippi", state)
  state <- gsub("Mo$", "Missouri", state)
  state <- gsub("N  J", "New Jersey", state)
  state <- gsub("N  M", "New Mexico", state)
  state <- gsub("Brook.*", "New York", state)
  state <- gsub("N  Y", "New York", state)
  state <- gsub("N Y", "New Jersey", state)
  state <- gsub("Missouri.*", "Missouri", state)
  state <- gsub("Moravia.*", "Moravia", state)
  state <- gsub("Neb.*", "Nebraska", state)
  state <- gsub("Pa.*", "Pennsylvania", state)
  state <- gsub("Poland.*", "Poland", state)
  state <- gsub("Prussia.*", "Prussia", state)
  state <- gsub("Russia.*", "Russia", state)
  state <- gsub("S  D", "South Dakota", state)
  state <- gsub("Va$", "Virginia", state)
  state <- gsub("W  Va", "West Virginia", state)
  state <- gsub("W  Virginia", "West Virginia", state)
  state <- gsub("Tex.*", "Texas", state)
  state <- gsub("Dallas", "Texas", state)
  state <- gsub("Ten.*", "Tennessee", state)
  state <- gsub("E Tennessee", "Tennessee", state)
  state <- gsub("East Tennessee", "Tennessee", state)
  state <- gsub("NANA", NA, state)
  state <- gsub("Unknown", NA, state)
  state <- gsub("Unreadavle", NA, state)
}

CleanCensus <- function(census) {
  # Clean age
  
  # Remove if month, week, or day in string
  drop.age <- c(grep("m",  census$self_residence_info_age ,ignore.case=TRUE),
                grep("w",  census$self_residence_info_age ,ignore.case=TRUE),
                grep("d",  census$self_residence_info_age ,ignore.case=TRUE))
  
  if(!is.integer(drop.age)){
  census <- census[!rownames(census) %in% drop.age]
  }

  #replace each fraction with its decimal form
  census$self_residence_info_age = gsub("1/12", ".08333333", census$self_residence_info_age)
  census$self_residence_info_age = gsub("2/12", ".1666667", census$self_residence_info_age)
  census$self_residence_info_age = gsub("3/12", ".25", census$self_residence_info_age)
  census$self_residence_info_age = gsub("4/12", ".3333333", census$self_residence_info_age)
  census$self_residence_info_age = gsub("5/12", ".4166667", census$self_residence_info_age)
  census$self_residence_info_age = gsub("6/12", ".5", census$self_residence_info_age)
  census$self_residence_info_age = gsub("7/12", ".5833333", census$self_residence_info_age)
  census$self_residence_info_age = gsub("8/12", ".6666667", census$self_residence_info_age)
  census$self_residence_info_age = gsub("9/12", ".75", census$self_residence_info_age)
  census$self_residence_info_age = gsub("10/12", ".8333333", census$self_residence_info_age)
  census$self_residence_info_age = gsub("11/12", ".9166667", census$self_residence_info_age)
  
  census$self_residence_info_age <- gsub("[^0-9.]", '', census$self_residence_info_age) # rm nonnumeric
  
  census$self_residence_info_age <- as.numeric(census$self_residence_info_age) # convert to numeric
  
  census$self_residence_info_age[census$self_residence_info_age > 100] <- NA # missing if over 100
  
  # Subset to individuals 18+
  census <- subset(census, self_residence_info_age>=18 & !is.na(self_residence_info_age)) # rm NAs
  
  # Clean & subset birth year
  if(census$self_residence_date_year==1900){
  census$birth.year <- as.numeric(str_sub(census$self_birth_date_empty, start= -4))
  
  census <- subset(census, !is.na(birth.year)) # rm NAs
  }
  if(census$self_residence_date_year==1910){
    census$birth.year <- as.numeric(str_sub(census$general_fs_birth_year, start= -4))
    
    census <- subset(census, !is.na(birth.year)) # rm NAs
  }
  

  # Remove non-alphabetic characters from surname and make all uppercase
  census$self_empty_name_surname<- trimws(toupper(gsub("[^[:alnum:] ]", "",census$self_empty_name_surname)))
  census$self_empty_name_given<- trimws(toupper(gsub("[^[:alnum:] ]", "",census$self_empty_name_given)))
  
  # Create other common colnames
  census$surname <- census$self_empty_name_surname
  census$first <- census$self_empty_name_given
  
  census$state <- census$self_residence_place_state
  census$birthplace <- census$self_birth_place_empty
  census$county <- census$self_residence_place_county
  
  # Split first and middle name
  census$first <- trimws(word(census$self_empty_name_given, 1))
  
  census$middle.name <- NA
  census$middle.name[vapply(strsplit(census$self_empty_name_given, "\\W+"), length, integer(1))>=2] <- trimws(word(census$self_empty_name_given[vapply(strsplit(census$self_empty_name_given, "\\W+"), length, integer(1))>=2], 2))
  
  # Drop obs with missing names
  census$surname.length <- nchar(census$surname)
  census$first.length <- nchar(census$first)
  census <- subset(census, surname.length>2 & first.length>0)
  
  # Standardize county
  census$county <- StCounty(census$county)
  
  census <- subset(census, nchar(county)>3 & !is.na(county)) # drop obs with missing
  
  # Standardize birthplace
  census$birthplace <- StState(census$birthplace)
  
  census <- subset(census, nchar(birthplace)>3 & !is.na(birthplace)) # drop obs with missing
  
  # Standardize first
  census$first <- StFirst(census$first)
  
  # Create soundex of first and surnames
  census$sound.surname <- soundex(census$surname)
  census$sound.first <- soundex(census$first)
  
  # Convert to dataframe
  census <- data.frame(census)
  return(census)
}

CleanLawton <- function(lawton){
  # Remove non-alphabetic characters from name and make all uppercase
  lawton$Surname<- trimws(toupper(gsub("[^[:alpha:] ]", "",lawton$Surname))) 
  lawton$Given.Name <- trimws(toupper(gsub("[^[:alpha:] ]", "",lawton$Given.Name))) 
  
  # Split first and middle name
  lawton$first <- trimws(unlist(lapply(strsplit(lawton$Given.Name," "), function(x) x[1])))
  lawton$middle.name <- trimws(unlist(lapply(strsplit(lawton$Given.Name," "), function(x) x[2])))
  
  # Standardize first names
  lawton$first <- StFirst(lawton$first)
  
  # Create soundex of first and surnames
  lawton$sound.surname <- soundex(lawton$Surname)
  lawton$sound.first <- soundex(lawton$first)
  
  # Name lengths
  lawton$surname.length <- nchar(lawton$Surname)
  lawton$first.length <- nchar(lawton$first)
  
  # Standardize state
  lawton$State.or.Territory <- StState(lawton$State.or.Territory)
  
  # Standardize place of residence
  lawton$Place.of.Residence <- StCounty(lawton$Place.of.Residence)
  
  # Convert filing dates to POSIXct
  lawton$Filing.Date <- as.POSIXct(paste(lawton$Filing.Date, "1901"),format="%B %d %Y",tz="UTC")
  
  # Calculate days from first day of grant claiming
  lawton$time.lapse <- as.numeric(round(difftime(lawton$Filing.Date, 
                                                as.POSIXct("August 6 1901",format="%B %d %Y",tz="UTC"), units = "days")))
  return(lawton)
}

CleanElreno <- function(elreno){
  # Remove non-alphabetic characters from name and make all uppercase
  elreno$Name<- trimws(toupper(gsub("[^[:alpha:] ]", "",elreno$Name))) 
  
  # Split first and middle name
  elreno$first <- trimws(unlist(lapply(strsplit(elreno$Name," "), function(x) x[1])))
  elreno$middle.name <- trimws(unlist(lapply(strsplit(elreno$Name," "), function(x) x[2])))
  elreno$surname <- trimws(unlist(lapply(strsplit(elreno$Name," "), function(x) x[3])))
  
  elreno$surname[is.na(elreno$surname)] <- elreno$middle.name[is.na(elreno$surname)] # correct for those w/o middle name
  elreno$middle.name[elreno$middle.name==elreno$surname] <- NA
  
  # Name lengths
  elreno$surname.length <- nchar(elreno$surname)
  elreno$first.length <- nchar(elreno$first)
  
  # Standardize first names
  elreno$first <- StFirst(elreno$first)
  
  # Create soundex of first and surnames
  elreno$sound.surname <- soundex(elreno$surname)
  elreno$sound.first <- soundex(elreno$first)
  
  # Standardize state
  elreno$State <- StState(elreno$State)
  
  # Standardize city
  elreno$City <- StCounty(elreno$City)
  return(elreno)
}

CleanSales <- function(sales){

  # Split first and middle name
  sales$first <- trimws(unlist(lapply(strsplit(sales$Names,","), function(x) x[2])))
  sales$surname <- trimws(unlist(lapply(strsplit(sales$Names,","), function(x) x[1])))
  sales$middle.name <- trimws(unlist(lapply(strsplit(sales$first," "), function(x) x[2])))
  sales$first <- trimws(unlist(lapply(strsplit(sales$first," "), function(x) x[1])))
  
  # Name lengths
  sales$surname.length <- nchar(sales$surname)
  sales$first.length <- nchar(sales$first)
  
  # Standardize first names
  sales$first <- StFirst(sales$first)
  
  # Create soundex of first and surnames
  sales$sound.surname <- soundex(sales$surname)
  sales$sound.first <- soundex(sales$first)
  
  # Standardize county
  sales$County <- StCounty(sales$County)
  return(sales)
}