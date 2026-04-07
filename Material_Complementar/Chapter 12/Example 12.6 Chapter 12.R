# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.6: Burials in Bannadi (again)
#
# Non metric multidimensional scaling
burials <- read.csv("Bannadi burials.csv",header=TRUE)
attach(burials)
library(vegan)
# Creating an Euclidean distance matrix of the original data
Euclidean.burials <- dist(burials[3:40])   
stress.vec <- rep(NA,5)
MDSdim2burials <- metaMDS(Euclidean.burials,autotransform=FALSE,trymax=100)
stress.vec[2] <- MDSdim2burials$stress
MDSdim3burials <- metaMDS(Euclidean.burials,autotransform=FALSE,k=3,trymax=150)
stress.vec[3] <- MDSdim3burials$stress
MDSdim4burials <- metaMDS(Euclidean.burials,autotransform=FALSE,k=4,trymax=200)
stress.vec[4] <- MDSdim4burials$stress
MDSdim5burials <- metaMDS(Euclidean.burials,autotransform=FALSE,k=5,trymax=250)
stress.vec[5] <- MDSdim5burials$stress
plot(c(2:5),stress.vec[2:5],type="b",xlab="Dimension",ylab="Stress",col="blue",
     main="Scree plot \n 25 Burials in Bannadi, Thailand")
matscores.dim3 <- scores(MDSdim3burials,display="sites")
matscores.dim3
# 3 MDS axes are needed, according to the Scree plot      
# Plotting all the possible pairs for the first 3 axis of NMDS
# (in order to get a plot similar to Figure 12.6) 
dev.new()
rownames(matscores.dim3) <- burials$Burial
par(mfrow = c(2, 2))
par(cex = 0.6)
par(mar = c(3, 3, 1, 1), oma = c(2, 2, 2, 2))
for (j in 2:3) {
    for (i in 1:2) {
if (i != j) { plot(matscores.dim3[,i],matscores.dim3[,j],type="n")}
    else {plot(10,10, type="n",xaxt="n",yaxt="n", bty="n")
         }
text(matscores.dim3[,i],matscores.dim3[,j],rownames(matscores.dim3))
}
}
mtext("Dim-1",side=1,line=-0.4, outer=TRUE, at=c(0.27,-1), cex=0.9)
mtext("Dim-2",side=1,line=-0.4, outer=TRUE, at=c(0.77,-1), cex=0.9)
mtext("Dim-3",side=2,line=-0.4, outer=TRUE, at=c(-1,0.27), cex=0.9)
mtext("Dim-2",side=2,line=-0.4, outer=TRUE, at=c(-1,0.77), cex=0.9)
#
dev.off()
#
detach(burials)
#
# End of script
