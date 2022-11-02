library(plm)

data("Produc", package = "plm")

###subset to make it easier to compare
Produc_sub <- Produc[as.numeric(Produc$state) %in% c(1,2,3,4),]
Produc_sub$state <- droplevels(Produc_sub$state)

##### Manually Implementing CCE Estimator
####create x-sectional averages
Produc_sub$mean_gsp <- aggregate(Produc_sub$gsp, by=list(Produc_sub$year), FUN="mean")[,2]
Produc_sub$mean_unemp <- aggregate(Produc_sub$unemp, by=list(Produc_sub$year), FUN="mean")[,2]

library(dummies)
# ### generate matrix of interactions so that each unit can have different coefficients
i_mean_gsp <- Produc_sub$mean_gsp*dummy(Produc_sub$state)
i_mean_unem <- Produc_sub$mean_unemp*dummy(Produc_sub$state)

m1 <- plm(gsp ~ + unemp + i_mean_gsp + i_mean_unem, data = Produc_sub, model="within", effect="twoways", index=c("state", "year"))
summary(m1)

##### Compare to package implementing CCE estimator
ccepmod <- pcce(gsp ~ + unemp, data = Produc_sub, model="p", index=c("state", "year"))
summary(ccepmod)

### Coefficients are numerically identical

