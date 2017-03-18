Capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
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
  first  <-gsub("EDW$","EDWIN", first)
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
  first  <-gsub("ED$","EDWARD", first)
  first  <-gsub("BEN$","BENJAMIN", first)
  first  <-gsub("REUBENJAMIN$","REUBEN", first)
  first  <-gsub("ALFREDWARD$","ALFRED", first)
}

StCounty <- function(county) {
  county <- gsub("[[:punct:]]", " ", county)
  county <- trimws(Capwords(gsub("[^[:alpha:] ]", "",county)))
  county <- gsub("Petersburg  Independent City", "Petersburg", county)
  county <- gsub("Portsmouth  Independent City", "Portsmouth", county)
  county <- gsub("Alexandria  Independent City", "Alexandria", county)
}

StState <- function(state) {
  # for 1901 lottery lists
  state <- gsub("[[:punct:]]", " ", state)
  state <- trimws(Capwords(gsub("[^[:alpha:] ]", "",state)))
  state <- gsub("Indian territory.*", "Indian Territory", state)
  state <- gsub("Indain Territory", "Indian Territory", state)
  state <- gsub("Ok.*", "Oklahoma Territory", state)
  state <- gsub("Ill.*", "Illinois", state)
  state <- gsub("ILLinois", "Illinois", state)
  state <- gsub("Kandas", "Kansas", state)
  state <- gsub("Kansas.*", "Kansas", state)
  state <- gsub("Kentucky.*", "Kentucky", state)
  state <- gsub("Missouri.*", "Missouri", state)
  state <- gsub("Texas.*", "Texas", state)
}

CleanIpums <- function(ipums,one.perc=FALSE,complete=TRUE) {
  if(one.perc){
    # Subset to individuals with nonzero and nonmissing real property 
    ipums <- subset(ipums, realprop>0)
    
    # Remove non-alphabetic characters from name and make all uppercase
    ipums$surname<- trimws(toupper(gsub("[^[:alpha:] ]", "",ipums$namelast))) 
    ipums$first <- trimws(toupper(gsub("[^[:alpha:] ]", "",ipums$namefrst))) 
  }
  
  # Trim spaces in name
  ipums$surname <- gsub(" ","",ipums$surname)
  ipums$first <- gsub("  ", " ",ipums$first)
  
  # Split first and middle name
  ipums$first <- trimws(unlist(lapply(strsplit(ipums$first," "), function(x) x[1])))
  ipums$middle.name <- trimws(unlist(lapply(strsplit(ipums$first," "), function(x) x[2])))
  
  # Standardize first name
  ipums$first <- StFirst(ipums$first)
  
  # Drop obs with missing names
  ipums$surname.length <- nchar(ipums$surname)
  ipums$first.length <- nchar(ipums$first)
  ipums <- subset(ipums, surname.length>2 & first.length>0)
  
  if(complete){

  # Standardize county
  ipums$county <- StCounty(ipums$county)
  }
  
  # Create soundex of first and surnames
  ipums$sound.surname <- soundex(ipums$surname)
  ipums$sound.first <- soundex(ipums$first)
  
  # Create first name initial
  ipums$first.initial <- substring(ipums$first, 1, 1) 
  
  return(ipums)
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