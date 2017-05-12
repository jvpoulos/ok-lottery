#####################################
### Link participant records to GLO Records###
#####################################

## Link participants to GLO sales records
# Merge by surname, middle, and first name 

link.sales <- merge(hs,
                    sales,
                    by=c("surname","middle.name","first"),
                    all.x=TRUE)

link.sales <- link.sales[!duplicated(link.sales$hs.id),] # remove dups

# Make binary response =1 if sale record

link.sales$sale <- ifelse(!is.na(link.sales$Date),1, 0)