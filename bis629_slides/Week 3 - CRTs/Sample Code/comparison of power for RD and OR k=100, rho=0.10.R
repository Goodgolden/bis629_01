# compare the power of logit and indentity link of binary outcomes

# K=232, m=2, r = 0.121


# first we check m given K
# the power of binary link is
  # var_b = p1(1-p1)+p2(1-p2) beta_star = p2-p1 = -0.0777, p1=0.4885,p2=0.411

  #power  = 1-chi^2[lambda(k,m,sigma,r,b_star)]
  #lam = b_star^2 Km/ 2*sig*(1+(m-1)r)

r = 0.5
K=50
m = seq(2,100,by=1)

p1=0.6
p2=0.4
RD=p1-p2
sig_rd = p1*(1-p1)+p2*(1-p2)
lam_rd = (RD^2)*K*m/(sig_rd*(1+(m-1)*r))
p_rd = 1-pchisq(3.84, 1, ncp =lam_rd, log = FALSE)


#b0=-0.0461
log_OR = log((p1/(1-p1))/(p2/(1-p2)))

sig_or = 1/(p1*(1-p1))+1/(p2*(1-p2))
lam_or = (log_OR^2)*K*m/(sig_or*(1+(m-1)*r))
p_or = 1-pchisq(3.84,1, ncp =lam_or, log = FALSE)
                plot(m,p_rd,type="l",col="red", ylab = "Power",
     main = expression(paste("Power by m, K =50, ", rho," =0.50, p_y=0.4, RR=1.5")))

lines(m,p_or,col="green")
legend(x = 80, y = 0.9,          # Position
       legend = c("RD", "OR"),  # Legend texts
      # lty = c("l","l"),           # Line types
       col = c("red", "green"),           # Line colors
       lwd = 2)  
zor=log_OR^2/sig_or
zrd=RD^2/sig_rd
#now hpgn values

r = 0.121
K=232
m = seq(2,100,by=1)

p1=0.4885
p2=0.411
RD=p1-p2
sig_rd = p1*(1-p1)+p2*(1-p2)
lam_rd = (RD^2)*K*m/(sig_rd*(1+(m-1)*r))
p_rd = 1-pchisq(3.84, 1, ncp =lam_rd, log = FALSE)


#b0=-0.0461
log_OR = log((p1/(1-p1))/(p2/(1-p2)))
p1x=exp(p1)/(1+exp(p1))
p2=exp(p2)/(1+exp(p2))
sig_or = 1/(p1*(1-p1))+1/(p2*(1-p2))
lam_or = (log_OR^2)*K*m/(sig_or*(1+(m-1)*r))
p_or = 1-pchisq(3.84,1, ncp =lam_or, log = FALSE)
plot(m,p_rd,type="l",col="red", ylab = "Power",
     main = expression(paste("Power for HPTN by m, K =232, ", rho," =0.12, p_y=0.41, RR=1.18")))

lines(m,p_or,col="green")
legend(x = 80, y = 0.9,          # Position
       legend = c("RD", "OR"),  # Legend texts
       # lty = c("l","l"),           # Line types
       col = c("red", "green"),           # Line colors
       lwd = 2)  
zor=log_OR^2/sig_or
zrd=RD^2/sig_rd









