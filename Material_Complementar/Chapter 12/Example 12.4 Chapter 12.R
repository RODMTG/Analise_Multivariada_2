# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.2: Burials in Bannadi (revisited)
#
# Principal coordinates analysis using cmdscale (Classic MultiDimensional scaling)
# 
burials <- read.csv("Bannadi burials.csv",header=TRUE)
attach(burials)
# Creating an Euclidean distance matrix of the original data
Euclidean.burials <- dist(burials[3:40],method="euclidean")
# cmscale implicitly double-centers the resulting similarity matrix   
(pcoa.burials <- cmdscale(Euclidean.burials, k = 4, eig = TRUE)) 
round(pcoa.burials$eig,2) 
# Showing and saving eigenvalues
sum.eigpcoa <- sum(pcoa.burials$eig)
perc.var <- (pcoa.burials$eig/sum.eigpcoa)*100
cumvar.bannadi <- cumsum(perc.var)
matlambdas <- cbind(1:length(pcoa.burials$eig),pcoa.burials$eig,perc.var,cumvar.bannadi)
colnames(matlambdas) <- c("Coordinate","Non negative eigenvalue","% of Total","Cumulative %")
(mateigen.csv <- round(matlambdas,1))
write.csv(mateigen.csv,"Non negative eigenvalues PCoA Steneryd.csv",row.names=FALSE)                                     # Eigenvalues
# Plotting the first 4 principal coordinates for the 47 Bannadi graves, 
# the total number of goods and the type of remains
library(lattice)
dev.new()
totno.goods=rowSums(burials[3:40])
# Unlike the sign changes indicated in the text, the results produced by R
# require to switch the signs of the second and third coordinates so as to make 
# them have positive values for burial B48 which contained the largest
# number of different types of grave goods.
mat.plotPCOs <- cbind(totno.goods,burials[,2],pcoa.burials$points[,1],
                   -pcoa.burials$points[,2:3],pcoa.burials$points[,4])
colnames(mat.plotPCOs) <- c("No. goods","Type","PCO1","PCO2","PCO3","PCO4")
splom(mat.plotPCOs,pch=16,col="red")
#
detach(burials)
#
# End of script
#