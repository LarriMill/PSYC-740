# HW 10
# Larri Miller
# Due 11/12/2021

# Q1 STARTS AT LINE 131, THE BEGINNING IS JUST PULLING IN THE FUNCTIONS I NEED

# getting in that data
rq1_1 <- read.delim("LariResQuestOneProcVar1.txt", sep=",")
rq1_2 <- read.delim("LariResQuestOneProcVar2.txt", sep=",")
rq1_3 <- read.delim("LariResQuestOneProcVar3.txt", sep=",")
rq1_4 <- read.delim("LariResQuestOneProcVar4.txt", sep=",")
rq2_1 <- read.delim("LariResQuestTwoProcVar1.txt", sep=",")
rq2_2 <- read.delim("LariResQuestTwoProcVar2.txt", sep=",")
rq2_3 <- read.delim("LariResQuestTwoProcVar3.txt", sep=",")
rq2_4 <- read.delim("LariResQuestTwoProcVar4.txt", sep=",")

# defining the priors I set in HW 9
rq1_mu <- 0.75
rq1_sigma <- 0.4
rq2_mu <- 0.2
rq2_sigma <- 0.4

# Part A
# I used a Gaussian prior, so I am pulling code from lines BayesFactorStarns.r 
# to find t val
tval=function(d1, mu=0.5){
  as.numeric(t.test(d1,mu=0.5)$stat)
}

tval2=function(d1, d2){
  as.numeric(t.test(d1, d2)$stat, paired=T)
}
# will need to plug that t value into marPred and nullPred, pulled from 
# effectSizeEstimation.r 

prior=function(es,mu=0,sigma=1) dnorm(es,mu,sigma)

condPred=function(t,es,N){
  df=N-1
  ncp=es*sqrt(N)
  cpred=suppressWarnings(dt(t,df,ncp))
  return(cpred)
}

joint=function(t,es,N,mu=1,sigma=0) prior(es,mu,sigma)*condPred(t,es,N)

marPred=function(t,N,mu=0,sigma=1){
  mpred=rep(NA,length(t))
  for(i in 1:length(t)){
    mpred[i]=integrate(joint,lower=-Inf,upper=Inf,
                       t=t[i],N=N,mu=mu,sigma=sigma)$val
  }
  return(mpred)
}

nullPred=function(t,N) dt(t,N-1)

# Function to calculate posterior probability of an effect
postProb = function(bayesFactor){
  bayesFactor/(bayesFactor+1)
}

# for part c, pulling seeInference() from Jeff code
seeInference=function(tObs,nObs,muPri=0,sigmaPri=1){
  par(mfcol=c(3,1),mar=c(2.5,4.5,1,4.5))
  lineWid=1.5 #just sets line width for plots
  
  normCons=marPred(tObs,nObs,muPri,sigmaPri)
  
  posterior=function(es) joint(tObs,es,nObs,muPri,sigmaPri)/
    normCons
  
  esSeq=seq(-4,4,length.out=100)
  
  #plot prior
  dens=prior(esSeq,muPri,sigmaPri)
  yMax=max(dens)*1.2
  plot(esSeq,dens,type='l',
       bty='n',ylab="Prior",
       ylim=c(0,yMax),las=1,
       xlab="",lwd=lineWid)
  abline(h=0)
  
  #plot likelihood
  lik=condPred(tObs,esSeq,nObs)
  yMax=max(lik)*1.2
  plot(esSeq,lik,type='l',
       bty='n',ylab="Likelihood",
       ylim=c(0,yMax),las=1,
       xlab="",lwd=lineWid)
  abline(h=0)
  
  #get a Gaussian that approximates
  #  the posterior distribution
  distPost=approxPost(tObs,nObs,muPri,sigmaPri)
  muPost=distPost[1]
  sigmaPost=distPost[2]
  
  #plot posterior  
  post=posterior(esSeq)
  yMax=max(post)*1.2
  plot(esSeq,post,type='l',
       bty='n',ylab="Posterior",
       ylim=c(0,yMax),las=1,
       xlab="",lwd=lineWid)
  abline(h=0)
  
  points(esSeq,dnorm(esSeq,muPost,sigmaPost),
         type='l',col='orange',lty=3,lwd=lineWid)
  
  return(normCons)
}

# then to get the exact values, also pulling approxPost() from Jeff code
approxPost=function(tObs,nObs,muPri=0,sigmaPri=1){
  #par(mfcol=c(1,1),mar=c(4.5,4.5,1,1))
  
  normCons=marPred(tObs,nObs,muPri,sigmaPri)
  
  posterior=function(es) joint(tObs,es,nObs,muPri,sigmaPri)/
    normCons
  
  esSeq=seq(-4,4,length.out=1000)
  post=posterior(esSeq)
  
  mode=esSeq[post==max(post)][1]
  
  rats=post/max(post)
  ratMiss=abs(rats-(dnorm(1)/dnorm(0)))
  oneSdAbove=esSeq[esSeq>mode & 
                     ratMiss==min(ratMiss[esSeq>mode])]
  oneSdBelow=esSeq[esSeq<mode & 
                     ratMiss==min(ratMiss[esSeq<mode])]
  
  devs=abs(c(oneSdAbove,oneSdBelow) - mode)
  
  muPost=mode
  sigmaPost=mean(devs)
  
  # plot(esSeq,post,type='l')
  # points(esSeq,dnorm(esSeq,muPost,sigmaPost),
  #        type="l",col="blue",lty=2)
  
  output=c(muPost,sigmaPost)
  return(output)
}

####################################### The actual HW, finally:
# Question 1
# A: Bayes Factor
rq1_1t <- tval(rq1_1$PropCor)
marPred(rq1_1t, 20, rq1_mu, rq1_sigma)/nullPred(rq1_1t, 20)
# Bayes Factor is 496.72

# B: Posterior Probability of an Effect
rq1_1B <- postProb(496.72)
# Posterior prob is 0.998

# C: 
seeInference(rq1_1t, 20, rq1_mu, rq1_sigma)
approxPost(rq1_1t, 20, rq1_mu, rq1_sigma)
# mean: 0.925 sd: 0.228

# Question 2
# A: Bayes Factor
rq1_2t <- tval(rq1_2$PropCor)
marPred(rq1_2t,20,rq1_mu,rq1_sigma)/nullPred(rq1_2t, 20)
# Bayes Factor is 24,338.53 (that's a big boy bayes factor)

# B: Posterior Probability of an Effect
rq1_2B <- postProb(24338.53)
# Posterior prob is 0.999

# C:
seeInference(rq1_2t, 20, rq1_mu, rq1_sigma)
approxPost(rq1_2t, 20, rq1_mu, rq1_sigma)
# mean: 1.197 sd: 0.252

# Question 3
# A: Bayes Factor
rq1_3t <- tval(rq1_3$PropCor)
marPred(rq1_3t,20,rq1_mu,rq1_sigma)/nullPred(rq1_3t, 20)
# Bayes Factor is 0.917 (what a juxtaposition from the last Bayes Factor)

# B: Posterior Probability of an Effect
rq1_3B <- postProb(0.917)
# Posterior prob is 0.478

# C:
seeInference(rq1_3t, 20, rq1_mu, rq1_sigma)
approxPost(rq1_3t, 20, rq1_mu, rq1_sigma)
# mean: 0.436 sd: 0.200

# Question 4
# A: Bayes Factor
rq1_4t <- tval(rq1_4$PropCor)
marPred(rq1_4t,20,rq1_mu,rq1_sigma)/nullPred(rq1_4t, 20)
# Bayes Factor is 0.271 (wow this range in Bayes Factors is wide)

# B: Posterior Probability of an Effect
rq1_4B <- postProb(0.271)
# Posterior prob is 0.213

# C:
seeInference(rq1_4t, 20, rq1_mu, rq1_sigma)
approxPost(rq1_4t, 20, rq1_mu, rq1_sigma)
# mean: 0.300 sd: 0.196

# Question 5
rq2_1t <- tval2(rq2_1$PropCorOo3, rq2_1$PropCorOc3)
rq2_1t
marPred(rq2_1t,25,rq2_mu,rq2_sigma)/nullPred(rq2_1t, 25)
# Bayes Factor is 12.00127

# B: Posterior Probability of an Effect
rq2_1B <- postProb(12.0013)
rq2_1B
# Posterior prob is 0.923

# C:
seeInference(rq2_1t, 25, rq2_mu, rq2_sigma)
approxPost(rq2_1t, 25, rq2_mu, rq2_sigma)
# mean: -0.501 sd: 0.192

# Question 6
rq2_2t <- tval2(rq2_2$PropCorOc3, rq2_2$PropCorOo3)
rq2_2t
marPred(rq2_2t,25,rq2_mu,rq2_sigma)/nullPred(rq2_2t, 25)
# Bayes Factor is 62.902

# B: Posterior Probability of an Effect
rq2_2B <- postProb(62.902)
rq2_2B
# Posterior prob is 0.984

# C:
seeInference(rq2_2t, 25, rq2_mu, rq2_sigma)
approxPost(4.6153, 25, rq2updatedmu, rq2updatedsigma)
# mean: -0.637 sd: 0.204

# Question 7
rq2_3t <- tval2(rq2_3$PropCorOc3, rq2_3$PropCorOo3)
rq2_3t
marPred(rq2_3t,25,rq2_mu,rq2_sigma)/nullPred(rq2_3t, 25)
# Bayes Factor is 2.914

# B: Posterior Probability of an Effect
rq2_3B <- postProb(2.914)
rq2_3B
# Posterior prob is 0.744

# C:
seeInference(2.6463, 25, rq2_mu, rq2_sigma)
approxPost(2.6463, 25, rq2_mu, rq2_sigma)
# mean: -0.372 sd: 0.188

# Question 8
rq2_4t <- tval2(rq2_4$PropCorOc3, rq2_4$PropCorOo3)
rq2_4t
marPred(rq2_4t,25,rq2_mu,rq2_sigma)/nullPred(rq2_4t, 25)
# Bayes Factor is 0.399

# B: Posterior Probability of an Effect
rq2_4B <- postProb(0.399)
rq2_4B
# Posterior prob is 0.285

# C:
seeInference(rq2_4t, 25, rq2_mu, rq2_sigma)
approxPost(0.0914, 25, rq2updatedmu, rq2updatedsigma)
# mean: -0.0280 sd: 0.180

