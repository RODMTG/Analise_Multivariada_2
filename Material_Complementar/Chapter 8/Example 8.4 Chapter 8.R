# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 8. Discriminant function analysis
#
# Example 8.4: Comparisons of two samples of Egyptian skulls
# Discriminant analysis of the Egyptian skulls data by logistic regression
# Periods: "Early predynastic" and "Romand period"
#
skulls1 <- read.csv("Egyptian skulls.csv",header=TRUE)
# Selecting a subset of skulls. Periods = "Early predynastic" and "Roman"
skulls.sub <- skulls1[c(1:30,121:150),]                                         
rownames(skulls.sub) <- c(1:60)
attach(skulls.sub)
skulls.sub
# Converting the variable "Period" into a binary variable
# 0 = Roman period, 1 = Early predynastic 
Periods.EPR <- ifelse(skulls.sub$Period == "Early predynastic",1,0)
# Intercept model (no covariates)                                                                                          
logitn.skulls <- glm(Periods.EPR ~ 1, family=binomial(link="logit"))            
# Logistic regression model with the covariates included
# Invoking glm with biomial distr. for the error and logit link
logit.skulls <- glm(Periods.EPR ~ Maximum.breadth+Basibregmatic.height +
                             Basialveolar.length +
                             Nasal.height, family=binomial(link="logit"))
# Testing by Chi-square the global significance of the model with 4 covariates, X1-X4     
anova(logitn.skulls,logit.skulls, test="Chisq")
# Summary of glm (logit model), z values displayed                               
(summ.logitsk <- summary.glm(logit.skulls))
# Computing chi-squared statistic = z^2                                     
chisq.statsk <- summ.logitsk$coefficients[,3]^2
# Replacing the z-values column with chi-squared values                                 
summ.logitsk$coefficients[,3] <- chisq.statsk                                   
colnames(summ.logitsk$coefficients)[3]="Chi-squared"
colnames(summ.logitsk$coefficients)[4]="P(> Chi^2)"
# Table showing estimates and chi-squared values
summ.logitsk
# Estimation of probabilities for every skull                                                                 
estim.probs <- predict(logit.skulls,type="response")
# Data frame: Period and estimated probability                            
mat.probs <- cbind(data.frame(Period),estim.probs)                              
rownames(mat.probs)=c(1:60)
mat.probs
# Mean probability of skulls from the Early predynastic period
meanp.early <- mean(estim.probs[c(1:30)])
# Mean probability of skulls from the Roman period                                       
meanp.roman <- 1-meanp.early
# Labels of points in the plot of skulls in data order vs. probability                                                    
point.skulls <- ifelse(mat.probs$Period=="Early predynastic","P","R")
# Colors of points           
colvec <- c(rep("red",30),rep("blue",30))                                       
# Plot: skulls in data order vs prob.
plot(c(1:60),estim.probs,pch=point.skulls,col=colvec,las=1,
     xlab="Skulls in data order",ylab="Probability",xaxt="n",
     main="Fitted probabilities by logistic regression \n for 30 predynastic (P) and 30 Roman (R) skulls")
legend(45,0.95,lty=1,c("Early predynastic","Roman"),col=c("red","blue"),
       title="Average probability",cex=0.7)
# Showing mean probability of skulls from the Early predynastic period       
segments(1,meanp.early,30,meanp.early,col="red")
# Showing mean probability of skulls from the Roman period                             
segments(31,meanp.roman,60,meanp.roman,col="blue")                           
abline(h=0.5,lty="dashed")
mtext("0.5     ",side=2,at=c(-3,0.5),las=1)
#
detach(skulls.sub)
#
# End of script
                                       