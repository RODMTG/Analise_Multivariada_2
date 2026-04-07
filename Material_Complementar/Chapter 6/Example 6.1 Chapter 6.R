# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 6. Principal components analysis
#
# Example 6.1 Body measurements of female sparrows
sparrows <- read.csv("Bumpus_sparrows.csv",header=TRUE)
attach(sparrows)

# Correlations between the five measurements (Table 6.1)
cor(sparrows[-1])
# Using prcomp to compute the principal components (eigenvalues and eigenvectors)
# With scale=TRUE, variable means are set to zero, and variances set to one
# This is equivalent to:
# prcomp(sparrows[,-1],scores=TRUE,center=TRUE,scale.=TRUE)
# sample scores stored in sparrows_pca$x
# singular values (square roots of eigenvalues) stored in sparrow_pca$sdev
# loadings (eigenvectors) are stored in sparrows_pca$rotation
# variable means stored in sparrows_pca$center
# variable standard deviations stored in sparrows_pca$scale
sparrows_pca <- prcomp(sparrows[,-1],scale=TRUE)
summary(sparrows_pca)
#
# A table containing eigenvalues and %'s accounted, follows
(eigen_sparrows <- sparrows_pca$sdev^2)     # Eigenvalues are sdev^2
names(eigen_sparrows) <- paste("PC",1:5,sep="")
eigen_sparrows
sumlambdas <- sum(eigen_sparrows)
propvar <- eigen_sparrows/sumlambdas
cumvar_sparrows <- cumsum(propvar)
matlambdas <- rbind(eigen_sparrows,propvar,cumvar_sparrows)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
# Printing table of eigenvalues and their contribution to the total variance
round(matlambdas,4)                         
#
# Showing the eigenvectors (see Table 6.3)
sparrows_pca$rotation
# The print method below is the same as sparrows_pca$sdev and 
# sparrows_pca$rotation printed together
print(sparrows_pca)                          
# Sample scores stored in sparrow_pca$x (see page 108)
sparrows_pca$x
# Identifying the scores by their survival status
sparrtyp_pca <- cbind(data.frame(Survivorship),sparrows_pca$x)
sparrtyp_pca
#
# Further analyses described on pages 110 and 111
# Means of scores for all the PC's classified by Survival status (see Table 6.4)
tabmeansPC <- aggregate(sparrtyp_pca[,2:6],by=list(Survivorship=sparrows$Survivorship),mean)
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$Survivorship)),]
tabfmeans <- t(tabmeansPC[,-1])
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans
# Standard deviations of scores for all the PC's classified by Survival status
# (see Table 6.4)
tabsdsPC <- aggregate(sparrtyp_pca[,2:6],by=list(Survivorship=sparrows$Survivorship),sd)
tabsdsPC <- tabsdsPC[rev(order(tabsdsPC$Survivorship)),]
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds
# t tests 
t.test(PC1~sparrows$Survivorship,data=sparrtyp_pca)
t.test(PC2~sparrows$Survivorship,data=sparrtyp_pca)
t.test(PC3~sparrows$Survivorship,data=sparrtyp_pca)
t.test(PC4~sparrows$Survivorship,data=sparrtyp_pca)
t.test(PC5~sparrows$Survivorship,data=sparrtyp_pca)
# F ratio tests
var.test(PC1~sparrows$Survivorship,data=sparrtyp_pca)
var.test(PC2~sparrows$Survivorship,data=sparrtyp_pca)
var.test(PC3~sparrows$Survivorship,data=sparrtyp_pca)
var.test(PC4~sparrows$Survivorship,data=sparrtyp_pca)
var.test(PC5~sparrows$Survivorship,data=sparrtyp_pca)
# Levene's tests (one-sided)
library(car)
(LTPC1 <- leveneTest(PC1~sparrows$Survivorship,data=sparrtyp_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~sparrows$Survivorship,data=sparrtyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~sparrows$Survivorship,data=sparrtyp_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(PC4~sparrows$Survivorship,data=sparrtyp_pca))
(p_PC4_1sided <- LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(PC5~sparrows$Survivorship,data=sparrtyp_pca))
(p_PC5_1sided <- LTPC5[[3]][1]/2)
# Plotting the scores for the first and second components
# This plot is similar to Figure 6.1
plot(sparrtyp_pca$PC1, sparrtyp_pca$PC2, 
     pch=ifelse(sparrtyp_pca$Survivorship == "S",1,16),
     xlab="PC1", ylab="PC2", main="Figure 6.1")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Survivor","Nonsurvivor"), pch=c(1,16))
#
detach(sparrows)

