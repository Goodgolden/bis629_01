library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(MASS)
library(geepack)
library(lme4)



######solveing optimization equation:
#### min K*c1+K*m*c2
#### constraint: K>0,M>0 and 7.85 = beta^2/(2*sigma^2*(1+(m-1)*rho/(K*m)))

#load nloptr package
library(nloptr)
library(CVXR)

# step 1: define variable minimiza over
#K <- Variable(1, integer=TRUE)
#m <- Variable(1, integer=TRUE)

# step 2: define the objective function:
#objective <- Minimize(K*c1+K*m*c2)

# step 3: define constains
#constraints <- list(K>=1, m>=1, 0.121/(0.496*(1+(m-1)*0.121)/(K*m))==7.85)

# step 4 create the problem to solve
#problem <- Problem(objective, constraints)

# solve the problem
#solution <- solve(problem)

# objective function
eval_f0 <- function( x , c1,c2,beta,sigma,rho){ #, c1,c2,beta,sigma,rho
  #,beta,sigma,rho
  K=x[1]
  m=x[2]
  cost=K*c1+K*m*c2
  return(cost )
}

# step 2: constraints function
eval_g0<-function(x, c1,c2,beta,sigma,rho){ #, c1,c2,beta,sigma,rho
  K=x[1]
  m=x[2]
  g=(beta^2)/(2*sigma*(1+(m-1)*rho)/(K*m))-7.85
  return(g)
}

# step 3 define parameters
#c1=10000; c2=50; beta=-0.3481; sigma = 0.248;rho=0.121
c1=10000; c2=50; beta=-0.0777; sigma = 0.248;rho=0.121

# step 4 lower bound and upper bound
lb<-c(1,1)
ub<-c(1000,1000)

# step 5 set initial values of x
x0 = c(10,3)

# Objective Function
eval_f <-  function( x , c1,c2,beta,sigma,rho){ #, c1,c2,beta,sigma,rho
  #,beta,sigma,rho
  K=x[1]
  m=x[2]
  cost=K*c1+K*m*c2
  return(cost )
}


# Equality constraints
eval_g_eq <- function(x, c1,c2,beta,sigma,rho){ #, c1,c2,beta,sigma,rho
  K=x[1]
  m=x[2]
  
  g=(beta^2)/(2*sigma*(1+(m-1)*rho)/(K*m))-7.85
  return(g)
}
# Lower and upper bounds
lb <- c(1,1)
ub<-c(250,100)
#initial values
x0 <- c(10,15)



# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 1000000,
              "local_opts" = local_opts,
              "print_level" = 0 )


res <- nloptr ( x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                # eval_g_ineq = eval_g_ineq,
                eval_g_eq = eval_g_eq,
                opts = opts,
                c1=c1,c2=c2,beta=beta,sigma=sigma,rho=rho
)
print(res)


#4.54 38.8
(beta^2)/(2*sigma*(1+(94-1)*rho)/(94*39))
94*c1+94*39*c2

93*c1+93*39*c2

94*c1+94*38*c2



# 3.69, 35.5
3*(beta^2)/(2*sigma*(1+(36-1)*rho)/(4*36))
#4*c1+4*36*c2

######################################
# 2022 code for optimizing K and M 
#####################################


C1=10000; C2=50
f=function(m,A,B,r) {
  objective=(15.7*C1*A*(1+(m-1)*r)+15.7*C2*A*(1+(m-1)*r)*m)/(B*m)
  objective
}

m = optimize(f,c(1,100),tol=0.0001,A=0.246,B=0.005929,r=0.121,lower=1)

(0.005929)/(2*0.246*(1+(39-1)*0.121)/(94*39))

# m=94, K=38
(0.005929)/(2*0.246*(1+(38-1)*0.121)/(94*38))
# power
1-pchisq(3.84,df=1,ncp=7.859)
C1*94+C2*38*94

# m=93, K=39
(0.005929)/(2*0.246*(1+(39-1)*0.121)/(93*39))
1-pchisq(3.84,df=1,ncp=7.807842)
C1*93+C2*39*93
