#####################################
### Link participant records to GLO Records###
#####################################

# Import patents data
unzip(paste0(data.directory,'patents.csv.zip'),exdir=data.directory)
patents <- read.csv(paste0(data.directory,"patents.csv"), stringsAsFactors = FALSE)

## Link participants to GLO sales records

# Create participant state code

hs$state_code <- state.abb[hs$state]
hs$state_code[hs$state=="Oklahoma Territory" | hs$state=="Indian Territory"] <- "OK"

# Merge by name and state

link.patents <- merge(hs,
                    patents,
                    by = c("surname", "middle.name", "first","state_code"),
                    all.x = TRUE)

link.patents <- link.patents[!duplicated(link.patents$hs.id), ] # remove dups

# Make binary response =1 if sale/homestead record

link.patents$sale <- ifelse(is.na(link.patents$sales) | link.patents$sales==0,0,1)
link.patents$homestead <- ifelse(is.na(link.patents$homesteads) | link.patents$homesteads==0,0,1)

# Make NA counts 0

outcome.vars <- c(grep("total_acres",colnames(link.patents), value=TRUE),grep("homesteads",colnames(link.patents), value=TRUE),grep("sales",colnames(link.patents), value=TRUE))

for(x in outcome.vars){
  link.patents[,x][is.na(link.patents[,x])] <- 0
}

link.patents$sales[is.na(link.patents$sales)] <- 0
link.patents$homesteads[is.na(link.patents$homesteads)] <- 0
link.patents$total_acres[is.na(link.patents$total_acres)] <- 0

# Drop link vars
drops <- c("sound.surname.x","sound.first.x","surname.length","first.length","sound.surname.y","sound.first.y","id")

link.patents <- link.patents[!names(link.patents) %in% drops]

# Save workspace
save.image(paste0(data.directory,"sales-link.RData"))