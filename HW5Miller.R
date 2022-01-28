# Larri Miller
# PSYC 740 - Prof. Jeff Starns
# HW 5
# Due 10/1/2021

# 1.

inferProbSucc = function(m, mu, sigma, # m = misses
                         lo=0, hi=1, x=0:15, #assuming x=0:15 like last HW
                         p=seq(0.01, 1, by=.01)){ # adding in p Sequence here
  par(mfcol=c(3,1),mar=c(4.5,4.5,1,1))
  
  # will need dtnorm
  dtnorm=function(x=x,mu=mu,sigma=sigma,
                  lo=-0,hi=1){
    dens=rep(NA,length(x))
    
    normCons=pnorm(hi,mu,sigma)-pnorm(lo,mu,sigma)
    
    for(i in 1:length(x)){
      if(x[i]<lo) dens[i]=0
      if(x[i]>hi) dens[i]=0
      if(x[i]>=lo & x[i]<=hi) dens[i]=dnorm(x[i],mu,sigma)/normCons
    }
    
    return(dens)
  }
  
  # probability functions, copied from previous code, replacing x w/ m
  prior=function(p) dtnorm(p,mu,sigma,0,1)
  condPred=function(m,p) dgeom(m,p)
  joint=function(m,p) prior(p)*condPred(m,p)
  marPred=function(m){
    pred=rep(NA,length(m))
    for(i in 1:length(pred)){
      pred[i]=integrate(joint,lower=0,upper=1,
                        m=m[i])$val
    }
    return(pred)
  }
  
  # prior
  probSeq <- seq(0.01, 1, by=.01)
  pri <- dtnorm(probSeq, mu, sigma, 0, 1)
  yMax1 <- max(pri)*1.2
  plot(probSeq, pri, type = 'l',
       bty='n', ylab="Prior",
       ylim=c(0, yMax1), las=1)
  
  # likelihood
  likelihood <- condPred(m, probSeq)
  yMax2 <- max(likelihood)*1.2
  plot(probSeq, likelihood, type='l',
       bty='n', ylab="Likelihood",
       ylim=c(0, yMax2), las=1)
  
  # posterior
  normCons <- marPred(m)
  posterior <- joint(m,p)/normCons
  yMax3 <- max(posterior)*1.2
  plot(probSeq, posterior, type='l',
       bty='n', ylab="Posterior",
       ylim=c(0, yMax3), las=1)
}
  
# 2. 
dtnorm=function(x=0,mu=0,sigma=1,lo=-Inf,hi=Inf){ #want any number of inputs for the x
  dens=rep(NA,length(x))
  
  normCons=pnorm(hi,mu, sigma)-pnorm(lo,mu,sigma)
  
  for(i in 1:length(x)){
    if(x[i]<lo) dens[i]=0
    if(x[i]>hi) dens[i]=0
    if(x[i]>=lo & x[i]<=hi) dens[i]=dnorm(x[i],mu,sigma)/normCons
  }
  return(dens)
}

seeTnorm = function(mu=0,sigma=1,
                    lo=-Inf,hi=Inf){
  xMin=max(lo,mu-5*sigma)
  xMax=min(hi,mu+5*sigma)
  xSeq=seq(xMin, xMax,length.out=250)
  yMin=0
  yMax=dtnorm(mu,mu,sigma,lo,hi)*1.2 #gives yourself a buffer on the top
  plot(xSeq, dtnorm(xSeq,mu,sigma,lo,hi),
       type='l', bty='n', yaxs='i',
       xlab="Value", ylab="Prob. Dens.",
       ylim=c(yMin, yMax))
  abline(v=0.2) # adding vertical lines at the ten percent cutoff points
  abline(v=.56)
  
}
# let's try some guesses! Mu will stay 0.38.
seeTnorm(0.38,0.3) #wayyyyy too much
seeTnorm(0.38, 0.2) # closer but still too much
seeTnorm(0.38, 0.1) # looks better
seeTnorm(0.38, 0.05) #too small!
seeTnorm(0.38, 0.15) # looks to be too much
seeTnorm(0.38, 0.12) # this feels about right
seeTnorm(0.38, 0.13)

# I think that a decent sigma is at or around 0.12.


# 3. We know from the previous question that the mean success rate is 0.38, so 
# mu should be 0.38. Let's keep sigma around 0.12 as well. I want to use seeGeom for
# of misses at this probability.
seeGeom = function(p){
  par(mfcol=c(1,1),mar=c(4.5,4.5,1,1)) # graph parameters from lecture code
  
  # reusing "seeBinom" function from binomLecture code
  xSeq=0:15 # x values from 0 - 15
  predDist=dgeom(xSeq, p) # to get geometric dist
  yMax=max(predDist)*1.2
  plot(xSeq, predDist, bty='o', # I like having the box better so I changed bty
       ylab="Probability",
       ylim=c(0, yMax), las=1,
       cex=2)
}
seeGeom(0.38)

# it looks like they level off around 5, meaning that that looks to be the max
# ish number of shots they need to take to get it in. Let's see what inferProbSucc
# tells us with mu 0.38, sigma 0.12, and number of misses 5.
inferProbSucc(5, 0.38, 0.12)

# Because the coach thinks the student will have a low success rate, obviously
# the number of misses is greater than 5. I'm going to play around with the numbers.
inferProbSucc(6, 0.38, 0.12)
inferProbSucc(7, 0.38, 0.12)
inferProbSucc(10, 0.38, 0.12)
inferProbSucc(15, 0.38, 0.12)

# Okay, the mu has to be 0.38 because that's the prior average. I'm guessing that
# the standard deviation is 0.12 based on my answer for # 2. Playing around with
# seeGeom tells me that mu should be > 5. I'm going to guess that "very low success
# rate" is somewhere around 10-- he didn't say that the student made NO shots, but
# I'm guessing it's at least doubly as bad as the other players to warrant "very
# low success."
