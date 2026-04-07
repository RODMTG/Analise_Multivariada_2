# Multivariate Statistical Methods: A Primer with R. 4th Edition
# Examples given in Appendix to Chapter 1. The Material of Multivariate Analysis
#
# Section A.4
# Assignment operator <- 
S <- 12
# Section A.5
# Creating a numeric vector using the concatenation operator c()
precip <- c(43, 20, 28, 28, 28, 15, 21, 10, 10, 19, 22, 58, 34, 21, 42, 50)
# Getting the number of elements in a vector
length(precip)
# Creating a numeric vector using c()
COLONY <- c("SS", "SB", "WSB", "JRC", "JRH", "SJ", "CR", "UO", "LO", "DP", "PZ", "MC", "IF", "AF", "GH", "GL")
# Giving names to the elements of a numeric vector, the names taken from
# a character vector of the same length
names(precip) <- COLONY
precip
# Indexing vectors: selecting one vector element
precip[13]
# Section A.6
RICHNESS <- c(2, 2, 3, 2, 2, 1, 5, 1)
RICHNESS
# Creating a matrix from a vector, columnwise (default), 
# specifying the number of rows only. 
RICHMAT <- matrix(RICHNESS, nrow = 4)
RICHMAT
# To be more specific, the number of columns can be declared
RICHMAT <- matrix(RICHNESS, nrow = 4, ncol=2)
RICHMAT 
# Indexing in matrices
RICHMAT[3, 2]
RICHMAT[2,]
RICHMAT[,1]
# Alternative way to create a matrix: defining separate vectors of the same
# length and binding them by column with cbind 
richy1 <- c(2, 2, 3, 2)
richy2 <- c(2, 1, 5, 1)
RICHMAT1 <- cbind(richy1, richy2)
RICHMAT1
# Indexing in matrices
RICHMAT1[,1]
RICHMAT1[,"richy1"]
# Asking for names of columns and rows in a matrix
colnames(RICHMAT1)
rownames(RICHMAT1)
# Defining names of columns
rownames(RICHMAT1) <- c("S1","S2","S3","S4")
#
# Section A.7
# Data frame creation (importing data from a tab delimited file)
euro.emp <- read.table("Euroemp.txt", header=TRUE, row.names=1)
# The class of euro.emp is "data.frame"
class(euro.emp)
# Displaying the contents the data frame containing the imported data
euro.emp 
# Displaying the first six rows of the data frame euro.emp
head(euro.emp)
# Showing the structure of the data frame euro.emp
str(euro.emp)
# Displaying the contents of the numeric vector AGR, one of the variables
# in the data frame euro.emp
euro.emp$AGR
# The following line would produce an error message (the name of the 
# data frame was not specified)
AGR
# The class of the variable Group within euro.emp is "factor"
class(euro.emp$Group)
# The mode of the (factor) variable Group within euro.emp is "numeric"
mode(euro.emp$Group)
#
# Importing data from a csv (comma separated values) file
# The file has a header (the first row has variable names) and
# the first column carries the names of the sampling units
euro.empcsv <- read.table("Euroemp.csv", header=TRUE, row.names=1, sep=",")
# Equivalently
euro.empcsv <- read.csv("Euroemp.csv", header=TRUE, row.names=1)
# 
# Section A.8
# Indexing elements in a data frame
# See explanations of each command in pages 26-27
euro.emp$AGR[3:8]
euro.emp$AGR[c(1, 3, 5)]
euro.emp[27:30,]
euro.emp[c("UK","Romania"),]
euro.emp[,c(2,5)]
euro.emp[,c("Group","MIN")]
euro.emp[euro.emp$Group == "Other",]