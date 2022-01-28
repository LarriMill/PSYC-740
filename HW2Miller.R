# Larri Miller
# PSYCH 740 HW 2
# Due 9/10/2021


# 1. Read in data frame
dat <- read.table("FacebookUsers2021.txt", header = TRUE, sep = ",")
dat <- as.data.frame(dat) #ensuring that dat is in data frame format

# looking at dat, just to make sure it read in okay
head(dat)

# 2. Create a histogram for the vote count (numUsers) variable
library(tidyverse) # I like ggplot

ggplot(dat, aes(NumUsers)) +
  geom_histogram() +
  labs(title = "Histogram of NumUsers")

# another function for creating a histogram:
numUsers <- dat$NumUsers
hist(numUsers)

# 3. Write code that returns the sum, mean, median, and standard deviation of the numUsers variable.
sum <- sum(dat$NumUsers)
mean <- mean(dat$NumUsers)
median <- median(dat$NumUsers)
sd <- sd(dat$NumUsers)

# The sum is 1,553; mean is 77.65; median is 45.5; and standard deviation is 77.087.

# 4. Write code that returns the median number of users for just the first five rows of dat
dat5 <- slice(dat, n = 1:5) # using the slice() function to create a subsetted dataframe with only five rows
head(dat5) # checking to see that it worked (it did)
median5 <- median(dat5$NumUsers)

# The median is 140.

# 5. Using the "which" statement, write code that creates a vector called "inAsia" 
# holding the row number of every country that is coded as being on the Asian continent
inAsia <- c(which(dat$Continent == "Asia"))

# 6. Write code that returns the mean of the vote count (NumUsers) variable just
# for the countries on the Asian continent
Asia <- subset(dat, Continent == "Asia") #creating a dataframe with just Asian countries
meanAsia <- mean(Asia$NumUsers)

# The mean of Asian countries is 102.625. 

# 7. Write an R function called "ProbToOdds" that will take a vector of probability
# values as inputs and return the corresponding odds values as outputs
ProbToOdds = function(x){ 
  y = x / (1 - x)
  return(y)
}

# does it work? Let's try feeding it a vector:
ProbToOdds(c(0.25, 0.5, 0.75))
# looks like it spit me a vector back out, which is good! That's what I wanted to check.

# 8. Write an R function called "OddsToProb" that will take a vector of odds values
# as inputs and return the corresponding probability values as outputs
OddsToProb = function(x){
  y = x / (x + 1)
  return(y)
}

OddsToProb(c(0.33, 1.0, 3.00)) #I checked using the output of the OddsToProb function,
# because this function should (if I'm thinking correctly) give me the input that
# I used in the OddsToProb function. Looks like it worked.

# 9. Write code that creates a vector called "probs" consisting of 20 probabilities
# randomly sampled from a uniform distribution
probs <- c(runif(20, min = 0, max = 1))
probs #looks like it worked

# 10. Write code that creates a scatterplot with your "probs" vector on the x axis and 
# the corresponding odds value returned by your "ProbToOdds" function
plot(probs, ProbToOdds(probs), main = "Probability to Odds",
     xlab ="Probabilities", ylab = "Odds")

# 11. Write code that creates a variable called "userProp" that holds the proportion
# of the total number of Facebook users across all countries that are in each 
# particular country
dat$userProp <- dat$NumUsers/sum #created new column dividing each countries num user
# by the sum calculated earlier

# checking - userProp should have a sum of ~ 1
sum(dat$userProp) # it does :)

