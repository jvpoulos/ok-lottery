###############################################################
### GLO Sales (1901-1910) from OK, Kansas, Missouri, Texas  ###
###############################################################

# Load sample and clean
sales <- read.csv(
  paste0(data.directory, "glo-sales.csv"),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# Rm mismatched
sales <- sales[sales$Accession != "WHATLEY, SAMUEL J", ]
sales <- sales[sales$Accession != "SCOTT, RUFE", ]
sales <- sales[sales$Accession != "DONOHOE, JAMES W", ]
sales <- sales[sales$Accession != "HAWKINS, ROBERT H", ]
sales <- sales[sales$Accession != "OK", ]
sales <- sales[sales$Names != "5th PM", ]
sales <- sales[sales$Names != "6th PM", ]
sales <- sales[sales$Names != "Indian", ]

# Fill in NAs with previous row
sales <- sales %>%
  do(na.locf(.))

# Keep the first Aliquot
sales <- sales[!duplicated(sales$Accession), ]

# Count # purchases by buyer
sales <-
  transform(sales, sales.id = paste(sales$Names, sales$State)) # Create unique buyer ID

sales <- sales %>%
  group_by(sales.id) %>%
  mutate(n.sales = n())

# Keep one record per buyer
sales <- sales[!duplicated(sales$sales.id),]

sales <- sales[ , !(names(sales) %in% c("sales.id"))] # rm sales ID

# Clean
sales <- CleanSales(sales)