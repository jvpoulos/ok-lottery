#####################################
### Link participant records to GLO Records###
#####################################

## Link participants to GLO sales records
# Merge by surname, middle, and first name

link.sales <- merge(hs,
                    sales,
                    by = c("surname", "middle.name", "first"),
                    all.x = TRUE)

link.sales <- link.sales[!duplicated(link.sales$hs.id), ] # remove dups

# Make binary response =1 if sale record

link.sales$sale <- ifelse(!is.na(link.sales$Date),1, 0)

# Make NA n.sales 0

link.sales$n.sales[is.na(link.sales$n.sales)] <- 0

# Drop link vars
drops <- c("sound.surname.x","sound.first.x","surname.length","first.length","sound.surname.y","sound.first.y")

link.sales <- link.sales[!names(link.sales) %in% drops]

# Save workspace
save.image(paste0(data.directory,"sales-link.RData"))

