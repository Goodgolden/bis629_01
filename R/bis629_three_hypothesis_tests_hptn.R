#install.packages("tidyverse")
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(MASS)
library(geepack)

#You will need to merge the data with network summary (ns) information with the main dataset
#based on visit number (visnum) and uid. As there are duplicates by visnum in the ns datset,
#you need to first select the record that you need.



# Treat =1 for having intervention
# change to your own path
path = "MY PATH"
#Now Load the file
base_hptn_30<-load(hptn.lec1,file=path)
base_hptn_30 = hptn.lec1
# for wald test 
gee.fit <- geeglm(cotton~ Treat, data = base_hptn_30,id=uid, family = binomial(link="logit"))
fit1=gee.fit
summary(fit1)
X =matrix(c(rep(1,696), base_hptn_30$Treat),nrow=696,ncol=2 )
b0 = -0.02222
b1 =  -0.31425
beta = matrix(c(b0,b1),ncol=1)
p =   exp(X%*%beta)/(1+exp(X%*%beta)) #  exp(b0+b1*X)/(1+exp(b0+b1*X))
W = diag(0,696)
diag(W)= p*(1-p)
var_b = solve(t(X)%*%W%*%X)
Z_2_wald = (b1^2)/var_b[2,2]

p_val_wald =1-pchisq(Z_2_wald , 1, ncp = 0, lower.tail = TRUE, log.p = FALSE)
#Z_2_wald  = 4.2279
#0.03976

#for score test and likelihood test
glm.fit0 <- glm(cotton~1, data = base_hptn_30, family = binomial(link="logit"))
fit0=glm.fit0
summary(fit0)

fit2 <- glm(cotton~ Treat, data = base_hptn_30, family = binomial(link="logit"))
anova( fit2, test ="Rao") 
#4.24, 0.04

Y = base_hptn_30$cotton
b0s= -0.173  
p0 =  exp(b0s)/(1+exp(b0s))
W2= diag(p0*(1-p0),696)
U = t(X) %*% (Y-p0)
I =    t(X)%*%W2%*%X
Z_2_score =  as.numeric( t(U) %*%  solve(I)%*%U)  

p_val_score =1-pchisq(Z_2_score, 1, ncp = 0, lower.tail = TRUE, log.p = FALSE)

# Z_2_score = 4.237
# p_val_score = 0.0395


# for liklihood test 
anova(glm.fit, test ="LRT")

# 4.24,  0.039
b0s=-0.173 
LL_0 = sum(Y *b0s -log(1+exp(b0s)))


LL_mle = sum( Y * (b0+b1*X[,1]) -log(1+exp(b0+b1*X[,2])))

Z_2_lrt = 2*(LL_mle - LL_0)
p_val_lrt =1-pchisq(  Z_2_lrt , 1, ncp = 0, lower.tail = TRUE, log.p = FALSE)

# Z_2_lrt = 4.242
# p_val_lrt=0.0394

anova(fit2, test ="LRT")

