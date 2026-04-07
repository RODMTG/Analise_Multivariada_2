# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 4. Tests of significance with multivariate data
#
# Example 4.1 Testing mean values for Bumpus' female sparrows
#
sparrows <- read.csv("Bumpus_sparrows.csv",header=TRUE)
#
# t-tests, one by one. Survivors vs. Non survivors
with(data=sparrows,t.test(Total_length[Survivorship=="S"],
                          Total_length[Survivorship=="NS"],var.equal=TRUE))
with(data=sparrows,t.test(Alar_extent[Survivorship=="S"],
                          Alar_extent[Survivorship=="NS"],var.equal=TRUE))
with(data=sparrows,t.test(L_beak_head[Survivorship=="S"],
                          L_beak_head[Survivorship=="NS"],var.equal=TRUE))
with(data=sparrows,t.test(L_humerous[Survivorship=="S"],
                          L_humerous[Survivorship=="NS"],var.equal=TRUE))
with(data=sparrows,t.test(L_keel_sternum[Survivorship=="S"],
                          L_keel_sternum[Survivorship=="NS"],var.equal=TRUE))
# Hotelling's T2 test. Comparing multivariate means 
# between survivor and non-survivor sparrows
# Uses libraries "Hotelling"
# Before running library(Hotelling) make sure you have installed the package
library(Hotelling)  
#  Hotelling's test is generated through a formula. The result is a list
t2testsparr <- hotelling.test(Total_length + Alar_extent + L_beak_head + 
                              L_humerous + L_keel_sternum ~ Survivorship, data=sparrows)
#  T2 statistic is located in the first element of the list "stat" 
cat("T2 statistic =",t2testsparr$stat[[1]],"\n")
# Output of the function hotelling.test is given
t2testsparr
detach(sparrows)
#
# End of the script
