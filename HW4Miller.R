# Larri Miller
# PSYC 740 - Prof. Jeff Starns
# HW 4
# Due 9/24/2021

# Heads up I've had quite the week so this will absolutely not be my best HW
# ya win some ya lose some

# Question 1
# A
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

# check to make sure it at least does something:
seeGeom(0.5) 
seeGeom(0.25)
seeGeom(0.75) # it did something hooray

# B 

# following betaBinomialDist and truncNormal code for integration stuff -
# getting here definitely took discussion with classmates!
dtnorm=function(x=0,mu=0,sigma=1,lo=-Inf,hi=Inf){ 
  dens=rep(NA,length(x))
  
  normCons=pnorm(hi,mu,sigma)-pnorm(lo,mu,sigma)
  
  for(i in 1:length(x)){
    if(x[i]<lo) dens[i]=0
    if(x[i]>hi) dens[i]=0
    if(x[i]>=lo & x[i]<=hi) dens[i]=dnorm(x[i],mu,sigma)/normCons
  }
  return(dens)
}
seeTnormGeom = function(mu, sigma, x=seq(0:15),
                        lo=-Inf,hi=Inf){
  
  par(mfcol=c(1,1),mar=c(4.5,4.5,1,1)) # graph parameters from lecture code
  
  prior=function(p) dtnorm(p,mu,sigma)
  condPred=function(x,p) dgeom(x,p)
  joint=function(x,p) prior(p)*condPred(x,p)
  marPred=function(x){
    pred=rep(NA,length(x))
    for(i in 1:length(pred)){
      pred[i]=integrate(joint,lower=0,upper=1,
                        x=x[i])$val
    }
    return(pred)
  }
  predDist=marPred(x)
  yMin=0
  yMax=max(predDist)*1.2
  plot(x, predDist,
       bty='o', 
       xlab="Misses", ylab="Pred. Dist.",
       ylim=c(yMin, yMax), las=1)
}


# C
# lets throw some numbers in here shall we
seeTnormGeom(2,1) #let's say it only takes a kid 2 tries
seeTnormGeom(5,2) # but they're kids so maybe they miss more
seeTnormGeom(12,4) # this would be my skill level, on a good day
# tried a few more, but I'm not going to force you to read all of that commentary

# Okay, it looks like my probability generally is increasing when I use smaller
# mus, so I'm going to stick to the lower end. There generally seems to be an elbow
# around 5 with the various numbers that I'm trying, so I'm going to pick a nice
# median value of 3 for my mu. I'm assuming that free throws for kids are like right
# under the basket or something. I'm keeping my sigma a tad wider at 2-- that
# gets me to that 5 cutoff zone and back to 1, which seems reasonable that it 
# would take a kid on the basketball team around 1-5 throws. These probabilities
# are super low though so I'm not convinced that I've done 1B correctly.
seeTnormGeom(3,2)

# Question 2
# A
# let's make a vector with the handy dandy dgeom() function
dgeom(0:5, .7)
#so, the probability that all 5 nights were sold out is 0.7, 4 is 0.21, 3 is 
# 0.063, 2 is 0.019, 1 is 0.0057, and the probability that none of the nights
# were sold out is 0.0017.

# B 
# well, now we need to chop out the possibility that 0-2 nights were sold out
dgeom(0:2,0.7)/pgeom(2,0.7)
# Now that we have more info, the probability that the show sold out all 5 nights
# 0.72, 4 nights is 0.22, and 3 nights is 0.065. 

