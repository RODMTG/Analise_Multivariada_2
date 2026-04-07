# Multivariate Statistical Methods: A Primer with R. 4th Edition
# Figures displayed in Chapter 3. Displaying multivariate data
# 
# Figure 3.1. Using plot() and indexing
sparrows <- read.csv("Bumpus_sparrows.csv",header=TRUE)
attach(sparrows)
#
plot(Total_length, Alar_extent, pch=c(1,16)[as.numeric(Survivorship)],
     xlab="Total length (mm)", ylab="Alar extent (mm)")
legend(158, 233, legend=c("Survivor","Non survivor"), pch=c(16,1), horiz=TRUE)
# 
# Figure 3.2. Producing a 3D Scatterplot. Bumpus data
# Uses scatterplot3d library
# Before running library(scatterplot3d) make sure you have installed the package
library(scatterplot3d)
s3d <- scatterplot3d(Alar_extent,Total_length,L_beak_head,
       pch=c(1,16)[as.numeric(Survivorship)],
       xlab="Alar extent", ylab="", angle=45,
       zlab="Length of beak and head", lty.hide=2,
       type="h", y.margin.add=0.1, font.axis=2, font.lab=2)
mtext("Total length",side=1, adj=1, line=-4.5, font=2)
legend(s3d$xyz.convert(238, 160, 34.1),c("Non-survivor","Survivor"),
       pch=c(1,16), text.font=2) # gives the correct symbol for legend points
# 
# Figure 3.3.  Draftsman's plot for Bumpus data 
# First version: using the pairs() function
# Including the bird number as one column in the data frame sparrows
sparr.birds <- data.frame(rownames(sparrows),sparrows[,2:6])
sparr.birds
# Creating a vector of variable labels
labs.diagonal <- c("Bird","Total length","Alar extent",
                   "L. beak & head","L. humerous","L. keel & sternum")
# Invoking the pairs function, and identifying points by Survivorship status
pairs(sparr.birds, labels=labs.diagonal, pch=c(1,16)[as.numeric(Survivorship)],
       font.labels=2)
# Setting up the plotting parameters to display legend
par(xpd=NA)
legend(-0.003,1.07,c("Non-survivor","Survivor"),
       pch=c(1,16), cex=0.7, text.font=2) # gives the legend points the correct symbol
#
# Figure 3.3. Second version.
# Using SciViews package, in order to avoid programming of the function
# controlling the plots on the diagonal.
# Before running library(SciViews) make sure you have installed the package
library(SciViews)
pairs(sparr.birds, diag.panel = panel.boxplot, labels=labs.diagonal, 
      pch=c(1,16)[as.numeric(Survivorship)], font.labels=2)
# Setting up the plotting parameters to display legend
par(xpd=NA)
legend(-0.003,1.07,c("Non-survivor","Survivor"),
       pch=c(1,16), cex=0.7, text.font=2) # gives the legend points the correct symbol
#
# Figure 3.3. Third version.     
# Uses car library
# Boxplots for each Survivorship group are added to the diagonal
# Before running library(car) make sure you have installed the package
library(car)
scatterplotMatrix(~ rownames.sparrows.+Total_length+Alar_extent+L_beak_head+L_humerous+
     L_keel_sternum | Survivorship, data=sparr.birds, var.labels=labs.diagonal,
     cex.labels=0.7, diagonal="boxplot",smooth=FALSE,reg.line=FALSE,
     pch=c(1,16),col=rep("black",2), legend.plot=FALSE)
# Setting up the plotting parameters to display legend
par(xpd=NA)
legend(-0.003,1.07,c("Non-survivor","Survivor"),
       pch=c(1,16), cex=0.7, text.font=2) # gives the legend points the correct symbol
# Figure 3.3. Fourth version. 
# The lattice package provides options to condition the scatterplot matrix on a factor.
# Before running library(lattice) make sure you have installed the package
library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
super.sym$superpose.symbol$pch <- c(1,16,rep(1,5))
super.sym$superpose.symbol$col <- rep("#000000",7)
trellis.par.set(super.sym)
splom(~sparr.birds, groups = Survivorship, data = sparr.birds, ps=0.5, varname.cex = .5,
      panel = panel.superpose,
      key = list(columns = 2, 
                 points = list(pch = super.sym$pch[1:2], col=super.sym$col[1:2]),
                 text = list(c("Non-survivor", "Survivor"))))
#
# Figure 3.3 Fifth version
# The GGally package provides functions ggpairs and ggscatmat
# Pearson's correlations are shown above the diagonal
# Bird numbers have been omitted
# Before running library(GGally) make sure you have installed the package
library(GGally)
ggscatmat(sparrows, columns=2:6, color="Survivorship")
#
detach(sparrows)
#
# 
# Figure 3.4 (a) Chernoff faces of canine data 
# First version. It uses function "face2" from library "TeachingDemos". 
# This function reads a numerical matrix.
# Rows represent subjects/observations, columns represent variables.
# Features are assigned through the option "which="
#
# The features are: 1 Width of center 2 Top vs. Bottom width (height of split) 3 Height of Face 4 Width of top half of face
#                   5 Width of bottom half of face 6 Length of Nose 7 Height of Mouth 8 Curvature of Mouth (abs < 9)
#                   9 Width of Mouth 10 Height of Eyes 11 Distance between Eyes (.5-.9) 12 Angle of Eyes/Eyebrows
#                  13 Circle/Ellipse of Eyes 14 Size of Eyes 15 Position Left/Right of Eyeballs/Eyebrows
#                  16 Height of Eyebrows 17 Angle of Eyebrows 18 Width of Eyebrows
#
# The face plotting routine needs the data values to be between 0 and 1 (inclusive).
# The scale option controls how scaling will be done on mat:
# "columns" scales each column to range from 0 to 1,
# "all" scales the entire dataset to vary from 0 to 1,
# "center" scales each column so that the mean of the column becomes 0.5 and all other values are between 0 and 1,
# "none" does no scaling assuming that the data has already been scaled.
canines <- read.csv("Canines Means.csv",header=TRUE, row.names=1)
attach(canines)
canines
# Before running library(TeachingDemos) make sure you have installed the package
library(TeachingDemos)
canmat <- data.matrix(canines)
faces2(canmat,which=c(14,6,18,11,5,8),labels=row.names(canines), nrows=2, ncols=4,scale="center")
# 
# Figure 3.4 (a). Chernoff faces of canine data
# Second version. It uses function "faces" from "aplpack" package
# This function reads a numerical matrix.
# Rows represent subjects/observations, columns represent variables.
# All the features are used, so the variables may be represented several times
# The features paramters of this implementation are:
# 1-height of face, 2-width of face, 3-shape of face, 4-height of mouth, 5-width of mouth, 6-curve of smile,
# 7-height of eyes, 8-width of eyes, 9-height of hair, 10-width of hair, 11-styling of hair, 12-height of nose,
# 13-width of nose, 14-width of ears, 15-height of ears.
library(aplpack)
fa <- faces(canines,labels=rownames(canines),plot=FALSE)
plot(fa,face.type=2)
#
# Figure 3.4 (b). Stars plot in R
# Command: stars and segments (uses "graphics" library)
# Drawing stars
stars(canines,nrow=2,ncol=6,labels=row.names(canines),key.loc=c(10,0.5))
# Drawing segments   
stars(canines,draw.segments=TRUE,nrow=2,ncol=6,labels=row.names(canines),
      key.loc=c(10,0.5))
#
# Figure 3.5 Profiles of variables for the canine data
Group <- rownames(canines)
ord.vec <- order(apply(canines,2,mean))
canines.order <- canines[,ord.vec]
plot(ord.vec, seq(min(canines.order),max(canines.order),length=length(ord.vec)),
     type='n', xaxt="n",
     ylab="Mandible measurement (mm)")
labx <- c("Mandible breadth", "Mandible height above 1st molar",
          "Length of 1st molar", "Breadth of 1st molar", 
          "Length from 1st to 3d molar", "Length 1st to 4th molar")
op <- par(cex=0.50)
axis(1,at=1:6, labels=labx)
par(op)
# Black lines
for (ii in 1:nrow(canines.order)){                    
lines((1:length(ord.vec)), canines.order[ii,], pch=(1:nrow(canines.order))[ii],lty=(1:nrow(canines.order))[ii], type="b")
}
r <- rank(Mol1_Breadth)
legend(1,45,Group[order(Mol1_Breadth)],pch=r)
#
# Figure 3.6 Multiple barplots
vars <- t(data.matrix(canines))
namesbars <- data.matrix(Group)
labx <- c("Mandible breadth", "Mandible height above 1st molar",
          "Length of 1st molar", "Breadth of 1st molar", 
          "Length from 1st to 3d molar", "Length 1st to 4th molar")
barplot(vars,names.arg=namesbars,col=c("black","blue","magenta","green","red","yellow"),
        beside=TRUE,ylim=c(0,62), cex.names=0.7, ylab="Mandible measurement (mm)")
# "beside=TRUE" puts bars together
box()
legend("topright",legend=labx,fill=c("black","blue","magenta","green","red","yellow"),
       cex=0.8)
detach(canines)
