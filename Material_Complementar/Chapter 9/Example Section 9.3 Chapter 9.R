# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 9. Cluster analysis
#
# Example from section 9.3 Hierarchic methods
# 
# Reading the lower triangular (distance) matrix, Table 9.1 (with the main 
# diagonal containing zeros). The first row and first column contain the names 
# of the five objects. Cells above the diagonal are blanks, thus option 
# "fill=TRUE" add blank fields (NA)
dist.mat5 <- read.csv("Distances Table 9.1.csv", fill=TRUE, header=TRUE, row.names=1)
colnames(dist.mat5) <- rownames(dist.mat5)
dist.mat5 <- as.dist(dist.mat5)
dist.mat5
# hclust is the R command for hierarchic agglomerative clustering
# Default method is "complete linkage". Here we use method = "single" (linkage) (= nearest neighbor)
mat5.nn <- hclust(dist.mat5, method = "single")
# Generating the dendrograms in Figure 9.1
# The default is to plot an inverted tree with the root at the top, and branches hanging down.
# You can force the branches down to the base line giving the hang argument a negative value (e.g. -1):
plot(mat5.nn, hang=-1,xlab="Object",ylab="Distance",
                 main="Dendrogram. Nearest neighbor linkage")
# Now we generate a dendrogram with the default method= complete (= farthest neighbor linkage)
dev.new()
mat5.fn <- hclust(dist.mat5)
plot(mat5.fn,hang=-1,xlab="Object",ylab="Distance",
                 main="Dendrogram. Farthest neighbor linkage")
# Finally we create a dendrogram witg the method = average (group average linkage)
dev.new()
mat5.avl <- hclust(dist.mat5,method="average")
plot(mat5.avl,hang=-1,xlab="Object",ylab="Distance",
                 main="Dendrogram. Group average linkage")
#
# End of script
#