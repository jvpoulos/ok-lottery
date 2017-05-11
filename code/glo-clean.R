#####################
### GLO Sales (1901-1910)   ###
#####################

## 100% Sample
# Load sample and clean
sales <- read.csv(paste0(data.directory,"glo-sales.csv"),
                  header=TRUE, 
                  sep = ",",
                  stringsAsFactors = FALSE) 

# Keep first sale
sales <- sales[sales$Names != "" & sales$County != "",]

# Clean
c.1900.s <- CleanSales(sales)