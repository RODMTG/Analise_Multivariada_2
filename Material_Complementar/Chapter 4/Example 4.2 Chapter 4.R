# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 4. Tests of significance with multivariate data
#
# Example 4.2 Testing variation in female sparrows
sparrows <- read.csv("Bumpus_sparrows.csv",header=TRUE)
attach(sparrows)
# F-test for Total length (not recommended)
var.test(Total_length[Survivorship=="S"],Total_length[Survivorship=="NS"])
# Levene's tests based on absolute differences around means using t-tests
# Standarizing the sparrows data set with scale()
matstand <- scale(sparrows[,2:6])
# To standardize with decostand (vegan package) remove the # in next 3 lines
# library(vegan)
# decostand is a function in vegan that standardizes data
# matstand <- as.matrix(decostand(sparrows[,2:6],method="standardize"))  
matsurv <- matstand[Survivorship == "S",]
matnosurv <- matstand[Survivorship == "NS",]
vecmediansurv <- apply(matsurv, 2, median)
vecmediannosurv <- apply(matnosurv, 2, median)
matabsdevsurv <- abs(matsurv - matrix(rep(vecmediansurv,nrow(matsurv)),
                                      nrow=nrow(matsurv), byrow=TRUE))
matabsdevnosurv <- abs(matnosurv - matrix(rep(vecmediannosurv,nrow(matnosurv)),
                                          nrow=nrow(matnosurv), byrow=TRUE))
matabsdev.all <- rbind(matabsdevsurv,matabsdevnosurv)
matabsdev.all <- data.frame(Survivorship, matabsdev.all)
t.test(matabsdev.all$Total_length[Survivorship == "S"], 
       matabsdev.all$Total_length[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$Alar_extent[Survivorship == "S"], 
       matabsdev.all$Alar_extent[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$L_beak_head[Survivorship == "S"], 
       matabsdev.all$L_beak_head[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$L_humerous[Survivorship == "S"], 
       matabsdev.all$L_humerous[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
t.test(matabsdev.all$L_keel_sternum[Survivorship == "S"], 
       matabsdev.all$L_keel_sternum[Survivorship == "NS"], alternative="less",
       var.equal = TRUE)
#
# leveneTest function from car package
# Remark 1: R produces an F statistic, instead of a t statistic. 
# As the degrees of freedom for Survivorship (group) is equal to 1, 
# the relation F = t^2 holds. Thus, on page 62, t=-1.20 , then t^2=1.464
# This is not too far from the F value = 1.447 produced by R 
# (the difference due to rounding errors)
# Remark 2: leveneTest() produces a two-sided test
# Before running library(car) make sure you have installed the package
library(car)
leveneTest(Total_length ~ Survivorship, data=sparrows)
# The remaining Levene's tests follows
leveneTest(Alar_extent ~ Survivorship, data=sparrows)
leveneTest(L_beak_head ~ Survivorship, data=sparrows)
leveneTest(L_humerous ~ Survivorship, data=sparrows)
leveneTest(L_keel_sternum ~ Survivorship, data=sparrows)
#
# Hotelling's test for the mean vectors of standardized variables follows
library(Hotelling)
# Standarizing the sparrows data set with scale()
matstand <- scale(sparrows[,2:6])
matstand.all <- data.frame(Survivorship, matstand)
colnames(matstand.all) <- colnames(sparrows[1:6]) 
t2testsparr <- hotelling.test(Total_length+Alar_extent+L_beak_head+
                              L_humerous+L_keel_sternum ~ 
                              Survivorship,data=matstand.all) 
# T2 statistic is located in the first element of the list stat 
cat("T2 statistic =",t2testsparr$stat[[1]],"\n")
# Output of the function hotelling.test is given
print(t2testsparr) 
# 
# Van Valen's test. Equivalent to the comparison of mean absolute median
# diferences between two groups. In the sparrows' example, the Van Valen's test
# is one-sided (Mean dij for survivors < Mean dij for non-survivors)
# dij is the norm of the individual vector i composed by the absolute
# deviations computed for all the variables in sample j.
# These norms define the second column of the data frame d.all
# 
# Standarizing the sparrows data set with scale()
matstand <- scale(sparrows[,2:6])
# To standardize with decostand (vegan package) remove the # in next 3 lines
# library(vegan)
# decostand is a function in vegan that standardizes data
# matstand <- as.matrix(decostand(sparrows[,2:6],method="standardize"))  
matsurv <- matstand[Survivorship == "S",]
matnosurv <- matstand[Survivorship == "NS",]
vecmediansurv <- apply(matsurv, 2, median)
vecmediannosurv <- apply(matnosurv, 2, median)
matabsdevsurv <- abs(matsurv - matrix(rep(vecmediansurv,nrow(matsurv)),
                     nrow=nrow(matsurv), byrow=TRUE))
matabsdevnosurv <- abs(matnosurv - matrix(rep(vecmediannosurv,nrow(matnosurv)),
                     nrow=nrow(matnosurv), byrow=TRUE))
matabsdev.all <- rbind(matabsdevsurv,matabsdevnosurv)
matabsdev.all <- data.frame(Survivorship, matabsdev.all)
d.all <- data.frame(Survivorship,sqrt(rowSums(matabsdev.all[,-1]^2)))
colnames(d.all)[2] <- "dij"   
with(d.all, t.test(dij[Survivorship=="S"], dij[Survivorship=="NS"],
                   var.equal=TRUE, alternative="less"))
sprintf("d-values for Survivors: Mean = %2.3f, Variance = %2.3f",
         mean(d.all$dij[Survivorship=="S"]),var(d.all$dij[Survivorship=="S"]))
sprintf("d-values for Non-survivors: Mean = %2.3f, Variance = %2.3f",
         mean(d.all$dij[Survivorship=="NS"]),var(d.all$dij[Survivorship=="NS"]))
#
detach(sparrows)
#
# End of the script   