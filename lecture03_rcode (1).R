# Simulation Example

N = 10000
NSIM = 1000
y = matrix(0,NSIM,2)
for (i in 1:NSIM) {
 x=rnorm(N,0,1)
 ci = mean(x) + c(-1,1)*qnorm(.975)*sd(x)/sqrt(N)  # Normal CI
 cit = mean(x) + c(-1,1)*qt(.975,N-1)*sd(x)/sqrt(N) # student-T CI
 y[i,1] = (ci[1] <= 0 & 0 <= ci[2])
 y[i,2] = (cit[1] <= 0 & 0 <= cit[2])
}


apply(y,2,mean) # Coverage of CIs

# Problem 1.8

#Wald statistic:
 
	y = 854
	n = 1103
 	pi.hat = y/n
 	pi.0 = 0.75
 	zwald = (pi.hat - pi.0)/sqrt(pi.hat*(1-pi.hat)/n)
 	zwald
 	abs(zwald) >  qnorm(0.975)
 
#Wald p-value:

     2*(pnorm(abs(zwald),lower.tail=F))

#Wald 95% CI:

     pi.hat + c(-1,1)*qnorm(0.975)*sqrt(pi.hat*(1-pi.hat)/n)

#Score statistic:

      zscore = (y-n*.75)/sqrt(n*.75*.25)
      zscore
      abs(zscore) > qnorm(0.975)

#score p-value and 95% CI

      prop.test(y,n,p=0.75,correct=F)


#LR statistic:

   LR = function(pi.0,y,N) {
      pi.hat = y/N; L0 = 0; L1 = 0 
      if (pi.0 < 1) L0 = L0 + (N-y)*log(1-pi.0)
      if (pi.0 > 0) L0 = L0 + y*log(pi.0)
      if (pi.hat > 0) L1 = L1 + y*log(pi.hat)
      if (pi.hat < 1) L1 = L1 + (N-y)*log(1-pi.hat)
      LR = 2*(L1-L0)
      return(LR)
   }

   LRstat = LR(pi.0,y,n)
   LRstat

   LRstat > qchisq(0.95,1)

#LR p-value:

   pchisq(LRstat,1,lower.tail=F)

#LR 95% CI:

   LR = function(pi.0,y,N,alpha) {
     pi.hat = y/N; L0 = 0; L1 = 0 
     if (pi.0 < 1) L0 = L0 + (N-y)*log(1-pi.0)
     if (pi.0 > 0) L0 = L0 + y*log(pi.0)	
     if (pi.hat > 0) L1 = L1 + y*log(pi.hat)
     if (pi.hat < 1) L1 = L1 + (N-y)*log(1-pi.hat)
     LR = 2*(L1-L0)
     LR-qchisq(1-alpha,df=1)
   }

   uniroot(f=LR,interval=c(0.000001,pi.hat),N=n,y=y,alpha=0.05)$root # lower bound
   uniroot(f=LR,interval=c(pi.hat,0.999999),N=n,y=y,alpha=0.05)$root # upper bound


#Exact p-value and 95% CI:

      binom.test(y,n,p=0.75)
