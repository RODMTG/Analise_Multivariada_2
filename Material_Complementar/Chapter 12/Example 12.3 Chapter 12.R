# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.3: Plant species in the Steneryd Nature Reserve (revisited)
#
# Principal coordinates analysis using cmdscale (Classic MultiDimensional scaling)
steneryd.read <- read.csv("Plants Steneryd Reserve.csv",header=TRUE)
steneryd1 <- data.frame(steneryd.read[,2:18])
steneryd <- data.frame(t(data.frame(steneryd1)))
colnames(steneryd) <- steneryd.read[,1]
rownames(steneryd) <- as.character(1:17)
attach(steneryd)
# Creating a Manhattan distance matrix
Manhattan.steneryd <- dist(steneryd,method="manhattan")
#   
pcoa.steneryd <- cmdscale(Manhattan.steneryd, eig = TRUE, x.ret=TRUE)
pcoa.steneryd
# Showing non-negative eigenvalues
sum.eigpcoa <- sum(pcoa.steneryd$eig[pcoa.steneryd$eig >=0])
perc.var <- (pcoa.steneryd$eig[pcoa.steneryd$eig >= 0]/sum.eigpcoa)*100
matlambdas <- cbind(1:length(which(pcoa.steneryd$eig >= 0)),pcoa.steneryd$eig[pcoa.steneryd$eig >= 0],perc.var)
colnames(matlambdas) <- c("Coordinate","Non negative eigenvalue","% of Total")
(mateigen.csv <- round(matlambdas,1))
write.csv(mateigen.csv,"Non negative eigenvalues PCoA Steneryd.csv",row.names=FALSE)   
# Plotting two PCO axes
plot(pcoa.steneryd$points[,1],pcoa.steneryd$points[,2],xlab="PCO1",ylab="PCO2",
     main="Principal Coordinates Analysis.\n Plants on 17 plots in Steneryd Nature Reserve")
text(pcoa.steneryd$points[,1],pcoa.steneryd$points[,2],labels=rownames(steneryd),pos=3,cex=0.8)
# Plotting the first 2 PCs for the 17 plots and plot number
library(lattice)
dev.new()
mat.plotPCOs <- cbind(as.numeric(rownames(steneryd)),pcoa.steneryd$points[,1:2])
colnames(mat.plotPCOs)=c("Plot","PCO1","PCO2")
splom(mat.plotPCOs,pch=16,col="red")
#
detach(steneryd)
#
# End of script
#
