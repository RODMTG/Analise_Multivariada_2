# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.1: Plant species in the Steneryd Nature Reserve (again)
#
# Non metric multidimensional scaling
steneryd.read <- read.csv("Plants Steneryd Reserve.csv",header=TRUE)
library(vegan)
steneryd1 <- data.frame(steneryd.read[,2:18])
steneryd <- data.frame(t(data.frame(steneryd1)))
colnames(steneryd) <- steneryd.read[,1]
rownames(steneryd) <- as.character(1:17)
attach(steneryd)
stress.vec=rep(NA,5)
MDSdim1steneryd <- metaMDS(steneryd,distance="euclidean",autotransform=FALSE,k=1,trymax=50)
stress.vec[1] <- MDSdim1steneryd$stress
MDSdim2steneryd <- metaMDS(steneryd,distance="euclidean",autotransform=FALSE,trymax=100)
stress.vec[2] <- MDSdim2steneryd$stress
MDSdim3steneryd <- metaMDS(steneryd,distance="euclidean",autotransform=FALSE,k=3,trymax=150)
stress.vec[3] <- MDSdim3steneryd$stress
MDSdim4steneryd=metaMDS(steneryd,distance="euclidean",autotransform=FALSE,k=4,trymax=200)
stress.vec[4] <- MDSdim4steneryd$stress
MDSdim5steneryd <- metaMDS(steneryd,distance="euclidean",autotransform=FALSE,k=5,trymax=250)
stress.vec[5] <- MDSdim5steneryd$stress
plot(c(1:5),stress.vec,type="b",xlab="Dimension",ylab="Stress",col="blue",
     main="Scree plot \n 25 plant species on 17 plots in Steneryd Reserve Nature, Sweden")
matscores.dim3 <- scores(MDSdim3steneryd,display="sites")
# 3 MDS axes are needed, according to the Scree plot      
# Plotting all the possible pairs for the first 3 axis of NMDS
# (in order to get a plot similar to Figure 12.5)
dev.new()
# For comparison purposes with Figure 12.5, the signs of the third dimension are switched
matscores.dim3[,3] <- -matscores.dim3[,3]
matscores.dim3
rownames(matscores.dim3) <- paste("P",1:nrow(matscores.dim3),sep="")
par(mfrow = c(2, 2))
par(cex = 0.7)
par(mar = c(3, 3, 1, 1), oma = c(2, 2, 2, 2))
for (j in 2:3) {
    for (i in 1:2) {
if (i != j) { plot(matscores.dim3[,i],matscores.dim3[,j],type="n")}
    else {plot(0,0, type="n",xaxt="n",yaxt="n", bty="n")
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
detach(steneryd)
#
# End of script
