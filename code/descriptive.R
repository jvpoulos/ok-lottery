#####################################
### Descriptive statistics         ###
#####################################

# by gender
print(tableNominal(cbind(link.patents,state.dummies,loc.dummies)[c("female",binary.covars, binary.outcomes)], group=link.patents$female, cumsum=FALSE, longtable = FALSE, prec=3))

# by quintile
print(tableNominal(cbind(link.patents,state.dummies,loc.dummies)[c("female",binary.covars, binary.outcomes)], group=link.patents$quintile, cumsum=FALSE, longtable = FALSE, prec=3))

# census table

tableNominal(data.frame(census.covars.1900), cumsum=FALSE, longtable = FALSE, prec=3)
tableNominal(data.frame(cbind(census.covars, "own"=link.1900.1910.subs$own, "farm"=link.1900.1910.subs$farm)), cumsum=FALSE, longtable = FALSE, prec=3)

## Estimate dose-response correlations (land patents)

print(cor.test(link.patents$sale, link.patents$draw))

print(cor.test(link.patents$homestead, link.patents$draw))

print(cor.test(link.patents$sale[link.patents$female==1], link.patents$draw[link.patents$female==1]))
print(cor.test(link.patents$homestead[link.patents$female==1], link.patents$draw[link.patents$female==1]))

## Estimate dose-response correlations (census outcomes)

print(cor.test(link.1900.1910.subs$farm, link.1900.1910.subs$draw))

print(cor.test(link.1900.1910.subs$own, link.1900.1910.subs$draw))