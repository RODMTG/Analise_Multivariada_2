# Multivariate Statistical Methods: A Primer. 4th Edition
# Chapter 8. Discriminant function analysis
#
# Example 8.3: Storm survival of female sparrows (reconsidered)
#
sparrows <- read.csv("Bumpus_sparrows.csv",header=TRUE)
attach(sparrows)
(n1 <- table(Survivorship)["S"])
(n2 <- table(Survivorship)["NS"])
# Converting the variable "Survivorship" into a binary variable
# 0 = Non-survivor, 1 = Survivor
Group.surv <- ifelse(Survivorship == "S",1,0)                                                                                
table(Group.surv)
# Invoking glm with biomial distr. for the error and logit link
# Intercept model (no covariates)
logitn.sparr <- glm(Group.surv ~ 1,family=binomial(link="logit"), data=sparrows)
# Logistic regression model with the covariates included
logit.sparr <- glm(Group.surv ~ . -Survivorship, family=binomial(link="logit"), data=sparrows)
# Testing by Chi-square the global significance of the logistic model with 5 covariates, X1-X5 
anova(logitn.sparr,logit.sparr,test="Chisq")
# Summary of glm (logit model), z values displayed                     
(summ.logit <- summary.glm(logit.sparr))
# Computing chi-squared statistic = z^2                         
chisq.stat <- summ.logit$coefficients[,3]^2
# Replacing the z-values column with chi-squared values                      
summ.logit$coefficients[,3] <- chisq.stat                        
colnames(summ.logit$coefficients)[3]="Chi-squared"
colnames(summ.logit$coefficients)[4]="P(> Chi^2)"
# Table showing estimates and chi-squared values
summ.logit                                                       
#
detach(sparrows)
#
# End of script