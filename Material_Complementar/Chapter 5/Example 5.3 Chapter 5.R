# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 5. Measuring and testing multivariate distances
#
# Example 5.3: More on distances between samples of Egyptian skulls
#
#              Mantel tests: 1) for the comparison between Penrose distances
#              and time distances, and 2) for the comparison between Mahalanobis
#              distances and time distances
#
skulls <- read.csv("Egyptian skulls.csv",header=TRUE)
attach(skulls)
#  Creates a list containing the means of each variable, by Period
means.df <- aggregate(as.matrix(skulls[,2:5]),list(Period), mean)
# Creates a list carrying the covariance matrix for all the variables, by Period
# Remark: Compared to Table 5.3, the ordering of samples here is:  3, 1, 2, 4, 5
covs.list <- by(skulls[,2:5],Period,cov)
covs.list  
n <- nrow(skulls)       ## Number of sampling units
p <- ncol(skulls) - 1   ## Number of variables
m <- nlevels(Period)    ## Number of groups
Period.list <- table(Period)  # Number of observations per Period
V.pool <- boxM(skulls[,2:5], Period)$pooled  # Pooled Covariance matrix (Table 5.3)
V.pool   
P <- matrix(rep(0,m*m),nrow=m,ncol=m)  # Initializing Penrose's distance matrix
for (j in 1:m) 
    { for (i in 1:m) 
          { if (i==j) {P[i,i]=0 }  else
               { for (k in 1:p) {
                      P[i,j] <- P[i,j]+(((means.df[i,(k+1)]-means.df[j,(k+1)])^2)/(p*V.pool[k,k]))
                                }
               }
          }
    }
# In order to get the same ordering of rows and columns as in Table 5.4
# the next four lines must be run once
P <- P[,c(2,3,1,4,5)]
P <- P[c(2,3,1,4,5),]
colnames(P) <- levels(Period)[c(2,3,1,4,5)]
rownames(P) <- levels(Period)[c(2,3,1,4,5)]
(P.Dist <- as.dist(P)) # Penrose distances (Table 5.4 (a))
#
#
# Mahalanobis distances, using function D2.dist from package "stats"
mahalanobis.dist <- as.matrix(D2.dist(means.df[,2:5], V.pool))
mahalanobis.dist <- mahalanobis.dist[c(2,3,1,4,5),]
mahalanobis.dist <- mahalanobis.dist[, c(2,3,1,4,5)]
colnames(mahalanobis.dist)=levels(Period)[c(2,3,1,4,5)]
rownames(mahalanobis.dist)=levels(Period)[c(2,3,1,4,5)]
(D.Dist <- as.dist(mahalanobis.dist)) # Mahalanobis distances (Table 5.4 (b))
#
# Creating the time matrix
T.Dist <- matrix(c(0.0,0.70,2.15,3.80,4.15,
                   0.70,0.0,1.45,3.10,3.45,
                   2.15,1.45,0.0,1.65,2.00,
                   3.80,3.10,1.65,0.0,0.35,
                   4.15,3.45,2.00,0.35,0.0),nrow=5,ncol=5)
colnames(T.Dist)=levels(Period)[c(2,3,1,4,5)]
rownames(T.Dist)=levels(Period)[c(2,3,1,4,5)]
T.Dist <- as.dist(T.Dist)
T.Dist
#
# Mantel test (generating all the elements of the randomization distribution)
# Function mantel from package "vegan"
# Before running library(vegan) make sure you have installed the package
library(vegan)
# Mantel test between Penrose and time distances with function mantel from vegan package
mantel(P.Dist, T.Dist)
# Mantel test between Mahalanobis and time distances with function mantel from vegan package
mantel(D.Dist, T.Dist)
#
# Mantel test (sampling the randomization distribution)
# Function mantel.rtest from package "ade4"
# Before running library(ade4) make sure you have installed the package
library(ade4)
# Mantel test between Penrose and time distances with function mantel.rtest from ade4 package
r1 <- mantel.rtest(P.Dist,T.Dist, nrepet = 9999)                                            
plot(r1, main="Mantel's test of Penrose distances vs. Time differences")
r1
# Mantel test between Mahalanobis and time distances with function mantel.rtest from ade4 package
r2 <- mantel.rtest(D.Dist,T.Dist, nrepet = 9999)                                            
plot(r2, main="Mantel's test of Mahalanobis distances vs. Time differences")
r2
