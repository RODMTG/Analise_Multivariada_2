# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 12. Ordination
#
# Example 12.7: Plant species in the Steneryd Nature Reserve (yet again)
#
# Correspondence analysis using ca from library "ca"
steneryd.read <- read.csv("Plants Steneryd Reserve.csv",header=TRUE,row.names=1)
library(ca)
attach(steneryd.read)
colnames(steneryd.read) <- paste("S",1:ncol(steneryd.read),sep="")
steneryd.ca <- ca(steneryd.read)
# Eignevalues and correlations between species and site scores
summary(steneryd.ca)
tabeigen <- cbind(1:length(steneryd.ca$sv),round(steneryd.ca$sv^2,3),round(steneryd.ca$sv,2))
colnames(tabeigen) <- c("No.","Eigenvalue","Corr. sps.-sites")
tabeigen
# Setting up labels of sites and species for plotting the first two CA axes
name.gen <- rep(" ",nrow(steneryd.read))
name.spe <- rep(" ",nrow(steneryd.read))
name.plot <- rep(" ",nrow(steneryd.read))
for (i in (1:nrow(steneryd.read))) {name.gen[i] <- substr(strsplit(rownames(steneryd.read)[i]," ")[[1]][1],1,3)
                                    name.spe[i] <- substr(strsplit(rownames(steneryd.read)[i]," ")[[1]][2],1,3)
                                    name.plot[i] <- paste(name.gen[i],name.spe[i])
                                    }
# Ordination plot (Axis CA1 and CA2)
plot(-steneryd.ca$colcoord[,1],-steneryd.ca$colcoord[,2], type="n", pch=16,
      xlab="Dimension 1", ylab="Dimension 2", xlim=c(-2,2), ylim=c(-1.4, 2.6), col="black", main="")
abline(h=0, lty="dotted")
abline(v=0, lty="dotted")
text(-steneryd.ca$colcoord[,1],-steneryd.ca$colcoord[,2], labels=rownames(steneryd.ca$colcoord), cex=0.7)  
text(-steneryd.ca$rowcoord[,1],-steneryd.ca$rowcoord[,2],labels=name.plot,cex=0.7)
#
detach(steneryd.read)
#
# End of script
#
