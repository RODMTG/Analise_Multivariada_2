# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 9. Cluster analysis
#
# Example 9.1: Clustering of European countries
#
# Hirerarchic cluster analysis, Nearest-neighbor
#
employ <- read.csv("Euroemp.csv",header=TRUE,row.names=1)
attach(employ)
# Standardizing the data with scale()
matstd.employ <- scale(employ[,2:10])
# Saving the standardized data in a csv file (Table 9.5)  
write.csv(round(matstd.employ,2),"Std percentages European employment.csv")
# Creating a (Euclidean) distance matrix of the standardized data
dist.employ <- dist(matstd.employ, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)          
clusemploy.nn <- hclust(dist.employ, method = "single")        
# Create extra margin room in the dendrogram, on the bottom (Countries labels)
par(mar=c(8, 4, 4, 2) + 0.1)
# Object "clusemploy.nn" is converted into a object of class "dendrogram" 
# in order to allow better flexibility in the (vertical) dendrogram plotting.
plot(as.dendrogram(clusemploy.nn),ylab="Distance between countries",ylim=c(0,6),
        main="Dendrogram. People employed in nine industry groups \n  from European countries")
dev.new()
# Create extra margin room on the right hand side in order to plot a horizontal dendrogram
par(mar=c(5, 4, 4, 7) +0.1)
# Plotting horizontal dendrogram (horiz=TRUE). 
plot(as.dendrogram(clusemploy.nn), xlab= "Distance between countries", xlim=c(6,0),
      horiz = TRUE,main="Dendrogram. People employed in nine industry groups \n  from European countries")
#
detach(employ)
#
# End of script
