

#p_x => p(X=1)
# n : sample size

#####wald
#
# b0 the estimate of beta_0|H_A
# b1 the beta^*|H_A
#

#power
p_x = 0.483;b0 = -0.022 ; b1=log(0.75); n=696
# I^{-1}(\beta^*):
Ib1 = (1/(1-p_x))*((1+exp(b0))^2/(exp(b0)))+
  (1/p_x)*((1+exp(b0+b1))^2/(exp(b0+b1)))
Ib1 
lambda_w = n*(b1^2)/Ib1
lambda_w 
power = 1-pchisq(3.84, 1, ncp = lambda_w,lower.tail = TRUE,)
power 

#sample size
library(rootSolve)


p_x = 0.483;b0 =-0.022;b1 = log(0.73)
Ib1 = (1/(1-p_x))*((1+exp(b0))^2/(exp(b0)))+
  (1/p_x)*((1+exp(b0+b1))^2/(exp(b0+b1)))
n_80=7.85/((b1^2)/Ib1)
n_90 = 10.5/((b1^2)/Ib1)
n_80
n_90

#MDE


library(rootSolve)


#way2
p_x = 0.483;b0 =-0.022; n=696

power.eq <-function(b1,para){
  p_x = para[1]
  b0=para[2]
  n=para[3]
  Ib1 = (1/(1-p_x))*((1+exp(b0))^2/(exp(b0)))+
    (1/p_x)*((1+exp(b0+b1))^2/(exp(b0+b1)))
  lambda_weq = n*(b1^2)/Ib1-7.85
}
find.para<-multiroot(power.eq , start =-0.1, para = c(0.483,-0.022,696),positive=FALSE)


p_x = 0.483;b0 =-0.022; n=696

power.eq <-function(b1,para){
  p_x = para[1]
  b0=para[2]
  n=para[3]
  Ib1 = (1/(1-p_x))*((1+exp(b0))^2/(exp(b0)))+
    (1/p_x)*((1+exp(b0+b1))^2/(exp(b0+b1)))
  lambda_weq = n*(b1^2)/Ib1-10.5
}
find.para<-multiroot(power.eq , start =0.1, para = c(0.483,-0.022,696), positive = TRUE)





#####Score Test####
#
# b00 the estimate of beta_0|H_0 from null model
# b0a the estimate of beta_0|H_A
#b1 the value of beta^*|H_A

#power
p_x = 0.483;b00 = -0.173;b0a = -0.022;b1=log(0.5) ; n =696#
p_0 =exp(b0a)/(1+exp(b0a)); # p_y0 =exp(b00)/(1+exp(b00));
p_1 = exp(b0a+b1)/(1+exp(b0a+b1))
lambda_s = n*(p_x*(1-p_x)*(p_1-p_0)^2)/(p_x*p_0*(1-p_0)+ (1-p_x)*p_1*(1-p_1))
lambda_s 
power = 1-pchisq(3.84, 1, ncp = lambda_s, lower.tail = TRUE, log.p = FALSE)
power


#sample size
library(rootSolve)

p_x = 0.483;b00 = -0.173;b0a = -0.022;b1=log(0.75) #
p_y0 = exp(b0a)/(1+exp(b0a))
p_y1 = exp(b0a+b1)/(1+exp(b0a+b1))
n_80=7.85/((p_x*(1-p_x)*(p_y1-p_y0)^2)/
             (p_x*p_y0*(1-p_y0)+ (1-p_x)*p_y1*(1-p_y1)))

n_90= 10.5/((p_x*(1-p_x)*(p_y1-p_y0)^2)/
              (p_x*p_y0*(1-p_y0)+ (1-p_x)*p_y1*(1-p_y1)))
n_80
n_90





#MDE
library(rootSolve)
p_x = 0.483;b00=-0.173;b0a = -0.022;n=696; 

power.eql <-function(b1,para){
  b00 = para[1];b0a=para[2]; n=para[3]; p_x= para[4]
  p_y0 = exp(b0a)/(1+exp(b0a))
  p_y1 = exp(b0a+b1)/(1+exp(b0a+b1))
  
  lambda_0=   n*(p_x*(1-p_x)*(p_y1-p_y0)^2)/
    (p_x*p_y0*(1-p_y0)+ (1-p_x)*p_y1*(1-p_y1))-7.85
  
  # power = power
}
find.para.80<-multiroot(power.eql, start =-1, 
                     para = c(-0.173,-0.022,696,0.483),  positive = FALSE)




power.eql <-function(b1,para){
  b00 = para[1];b0a=para[2]; n=para[3]; p_x= para[4]
  p_y0 = exp(b0a)/(1+exp(b0a))
  p_y1 = exp(b0a+b1)/(1+exp(b0a+b1))
  
  lambda_0=   n*(p_x*(1-p_x)*(p_y1-p_y0)^2)/
    (p_x*p_y0*(1-p_y0)+ (1-p_x)*p_y1*(1-p_y1))-10.5
}
find.para.90<-multiroot(power.eql, start =-1, 
                        para = c(-0.173,-0.022,696,0.483),  positive = FALSE)







#LRT
#power

p_x = 0.483; b0a = -0.022; b1=log(0.75); n=696
p0 =exp(b0a)/(1+exp(b0a)); p1 =  exp(b0a+b1)/(1+exp(b0a+b1))
p_star = (1-p_x)*p0+p_x*p1; b_tilde = log(p_star /(1-p_star ))
b_tilde  # for b_tilde you can use b_0|H_0(b00=-0.173) directly estimated from null model of HPTN data
D_star =  2*( p_x*(p1*(b0a+b1)-p_star*b_tilde -
                     log( (1+exp(b0a+b1))/(1+exp(b_tilde))   ) )
              +(1-p_x)* (p0*b0a-p_star*b_tilde- 
                           log((1+exp(b0a))/(1+exp(b_tilde)) )   ))
D_star 
M_t = (p_x*(1-p1)*p1 +(1-p_x)*p0*(1-p0))/
  (p_star *(1-p_star )  )  
M_t
lambda = n*D_star+1-M_t  
lambda 
power=1-pchisq(3.84, 1, ncp = lambda,lower.tail = TRUE)
power





#sample size


p_x = 0.483; b0a = -0.022; b1=log(0.75); n=696
p0 =exp(b0a)/(1+exp(b0a)); p1 =  exp(b0a+b1)/(1+exp(b0a+b1))
p_star = (1-p_x)*p0+p_x*p1; b_tilde = log(p_star /(1-p_star ))
D_star =  2*( p_x*(p1*(b0a+b1)-p_star*b_tilde -  log( (1+exp(b0a+b1))/(1+exp(b_tilde))   ) )
              +(1-p_x)* (p0*b0a-p_star*b_tilde-   log((1+exp(b0a))/(1+exp(b_tilde)) )   ))
D_star 
M_t = (p_x*(1-p1)*p1 +(1-p_x)*p0*(1-p0))/(p_star *(1-p_star )  )  
M_t
#power = 80%
n_80=(7.85-1+M_t)/D_star
n_80
#power = 90%
n_90=(10.5-1+M_t)/D_star
n_90



#MDE
library(rootSolve)
p_x = 0.483; b0a = -0.022; n=696
power.eql <-function(b1,para){
  b0a=para[1]; n=para[2]; p_x= para[3]
  p0 =exp(b0a)/(1+exp(b0a)); p1 =  exp(b0a+b1)/(1+exp(b0a+b1))
  p_star = (1-p_x)*p0+p_x*p1; b_tilde = log(p_star /(1-p_star ))
  D_star =  2*( p_x*(p1*(b0a+b1)-p_star*b_tilde -log( (1+exp(b0a+b1))/(1+exp(b_tilde))   ) )
                +(1-p_x)* (p0*b0a-p_star*b_tilde-    log((1+exp(b0a))/(1+exp(b_tilde)) ) ))
  M_t = (p_x*(1-p1)*p1 +(1-p_x)*p0*(1-p0))/(p_star *(1-p_star )  )
  lambda_leq =  n*D_star+1-M_t  -7.85
}
find.para.80<-multiroot(power.eql, start =-0.1, 
                     para = c(b0a,n,p_x), positive = FALSE)
find.para.80$root






power.eql <-function(b1,para){
  b0a=para[1]; n=para[2]; p_x= para[3]
  p0 =exp(b0a)/(1+exp(b0a)); p1 =  exp(b0a+b1)/(1+exp(b0a+b1))
  p_star = (1-p_x)*p0+p_x*p1; b_tilde = log(p_star /(1-p_star ))
  D_star =  2*( p_x*(p1*(b0a+b1)-p_star*b_tilde -log( (1+exp(b0a+b1))/(1+exp(b_tilde))   ) )
                +(1-p_x)* (p0*b0a-p_star*b_tilde-    log((1+exp(b0a))/(1+exp(b_tilde)) ) ))
  M_t = (p_x*(1-p1)*p1 +(1-p_x)*p0*(1-p0))/(p_star *(1-p_star )  )
  lambda_leq =  n*D_star+1-M_t  -10.5
}
find.para.90<-multiroot(power.eql, start =-0.1, 
                        para = c(b0a,n,p_x), positive = FALSE)
find.para.90$root


# code for creating lambda_cc.png by Chao Cheng 09.22

fx=function(x, lambda) dchisq(x, 1, ncp = lambda, log = FALSE)
power.eql<-function(lambda,power=0.8) integrate(fx,0,3.84,lambda=lambda)$value - (1-power)
###### (1) lambda for 80% power
rootSolve::multiroot(power.eql , start =10, positive = TRUE, power=0.8)$root
# [1] 7.846775
###### (2) lambda for 90% power
rootSolve::multiroot(power.eql , start =10, positive = TRUE, power=0.9)$root
# [1] 10.5049


# code for creating chi-square_cc.png by Chao Cheng 09.22
x=seq(0,40,by=0.01)
plot(x,dchisq(x, 1, ncp = 0, log = FALSE),type="l",xlab="x",ylim=c(0,0.12),lwd=2,
     ylab=expression(f(x,k,lambda)),
     main = expression("Probability density function of"~chi[1]^2~"with non centrality parameter"~lambda))
lines(x,dchisq(x, 1, ncp = 7.85, log = FALSE),type="l",col=2,lwd=2)
lines(x,dchisq(x, 1, ncp = 10.5, log = FALSE),type="l",col=3,lwd=2)
legend("topright",legend=c("Central",
                           expression("Non-central with"~lambda==7.85),
                           expression("Non-central with"~lambda==10.50)),lwd=2,col=c(1,2,3))
