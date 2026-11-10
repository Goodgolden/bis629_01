library(dplyr)
library(geepack)
library(lme4)
load("E:/Dropbox/BIS Adv ImpSci course/2023 BIS629 Course Material/Lecture Notes/week 3-Cluster-randomized trials/sampel code/HPTN-Cluster-level.RData")
#E:\Dropbox\BIS Adv ImpSci course\2023 BIS629 Course Material\Lecture Notes\week 3-Cluster-randomized trials\sampel code
######## now we start cluster level analysis by hand########
### first we calculate the group means and variance for each network:
### notice HPTN outcomes is binary outcome, 
### so need to find hat{p}_ik and sigma^2_ik = hat{p}_ik(1-hat{p}_ik)/m_ik
### p_ik = sum((Y_ik=1))/m_ik

m = table(base_hptn_30$NKID) 

# find p_ik for each network, we calculate them in two split  arm
# 
data_t =  base_hptn_30 %>%  filter(Treat==1)
data_c =  base_hptn_30 %>%  filter(Treat==0)

count_t = data_t %>% 
  group_by(NKID) %>% 
  summarise(cotton = sum(cotton))
count_c = data_c %>% 
  group_by(NKID) %>% 
  summarise(cotton = sum(cotton))

#then we calculate sigma^2_ik=p_ik(1-p_ik)/m
m_t= table(data_t$NKID)  # cluster size 
m_c= table(data_c$NKID)


p_t = count_t$cotton/m_t # p_1k
p_c = count_c$cotton/m_c #p_2k

#since hptn data need to use p_bar, so we need to obtain the mean value
# for MaxART data, we don't need to do this
p.t.m = sum(p_t)/112 
p.c.m = sum(p_c)/120

# obtain the sigma^2_ik 
var_t=p.t.m*(1-p.t.m)/m_t
var_c=p.c.m*(1-p.c.m)/m_c

# obtain the weights
w_t = (1/var_t)/sum((1/var_t))
w_c = (1/var_c)/sum((1/var_c))

# 
Var.beta =sum((as.numeric(w_t)^2)*as.numeric(var_t))+sum((as.numeric(w_c)^2)*as.numeric(var_c))

beta = sum(w_t*p.t.m)-sum(w_c*p.c.m)

Z_1 = beta^2/Var.beta 

p_val_c =1-pchisq(Z_1, 1, ncp = 0, lower.tail = TRUE, log.p = FALSE)

#fixed cluster size

####CRT with GEE#####

# obtain the estimates using identity link
base_hptn_30<-base_hptn_30[order(base_hptn_30$NKID),] 
fit_CRT <- geeglm(cotton~ Treat, id = NKID, data = base_hptn_30, #family = binomial(link="logit"),
                  corstr = "exchangeable")

summary(fit_CRT)


#UNCLUSTER
fit_uncluster <- lm(cotton~ Treat, data = base_hptn_30)
summary(fit_uncluster)
p2=0.494
p1 = 0.494 - 0.0778
p1/p2

###Logit link

fit_CRT <- geeglm(cotton~ Treat, id = NKID, data = base_hptn_30, family = binomial(link="logit"),
                  corstr = "exchangeable")

summary(fit_CRT)

