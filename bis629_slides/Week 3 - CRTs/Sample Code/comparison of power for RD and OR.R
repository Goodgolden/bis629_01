# compare the power of logit and indentity link of binary outcomes

# K=232, m=2, r = 0.121


# first we check m given K
# the power of binary link is
  # var_b = p1(1-p1)+p2(1-p2) beta_star = p2-p1 = -0.0777, p1=0.4885,p2=0.411

  #power  = 1-chi^2[lambda(k,m,sigma,r,b_star)]
  #lam = b_star^2 Km/ 2*sig*(1+(m-1)r)

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
log_OR = -0.3148
p1=exp(-0.0461)/(1+exp(-0.0461))
p2=exp(-0.0461-0.3148)/(1+exp(-0.0461-0.3148))
sig_or = 1/(p1*(1-p1))+1/(p2*(1-p2))
lam_or = (log_OR^2)*K*m/(sig_or*(1+(m-1)*r))
p_or = 1-pchisq(3.84, 1, ncp =lam_or, log = FALSE)



plot(m,p_rd,type="l",col="red", ylab = "Power",
     main = "Comparision of Power with different m given K =232")
lines(m,p_or,col="green")
legend(x = 80, y = 0.9,          # Position
       legend = c("RD", "OR"),  # Legend texts
      # lty = c("l","l"),           # Line types
       col = c("red", "green"),           # Line colors
       lwd = 2) 



# Then we check K given m
# the power of binary link is
# var_b = p1(1-p1)+p2(1-p2) beta_star = p2-p1 = -0.0777, p1=0.4885,p2=0.411

#power  = 1-chi^2[lambda(k,m,sigma,r,b_star)]
#lam = b_star^2 Km/ 2*sig*(1+(m-1)r)

r = 0.121
m=2
K= seq(200,800,by=1)

p1=0.4885
p2=0.411
RD=p1-p2
sig_rd = p1*(1-p1)+p2*(1-p2)
lam_rd = (RD^2)*K*m/(sig_rd*(1+(m-1)*r))
p_rd = 1-pchisq(3.84, 1, ncp =lam_rd, log = FALSE)

#b0=-0.0461
log_OR = -0.3148
p1=exp(-0.0461)/(1+exp(-0.0461))
p2=exp(-0.0461-0.3148)/(1+exp(-0.0461-0.3148))
sig_or = 1/(p1*(1-p1))+1/(p2*(1-p2))
lam_or = (log_OR^2)*K*m/(sig_or*(1+(m-1)*r))
p_or = 1-pchisq(3.84, 1, ncp =lam_or, log = FALSE)



plot(K,p_rd,type="l",col="red", ylab = "Power",
     main = "Comparision of Power with different K given m=2")
lines(K,p_or,col="green")
legend(x = 700, y = 0.8,          # Position
       legend = c("RD", "OR"),  # Legend texts
       # lty = c("l","l"),           # Line types
       col = c("red", "green"),           # Line colors
       lwd = 2) 





