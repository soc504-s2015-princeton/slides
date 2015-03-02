############## Load Data #####

# install.packages("MASS") 
library(MASS)
library(ggplot2)
library(dplyr)
library(broom)
df <- read.csv("data/nature08230-s2.csv")
df <- tbl_df(df)
df <- select(df, country, HDI.2005, TFR.2005)
df <- filter(df, !is.na(HDI.2005), !is.na(TFR.2005))
df

############## Linear Regression ##########

ggplot(df, aes(x = HDI.2005, y = TFR.2005)) +
  geom_point(color = "blue")


lfit <- lm(TFR.2005 ~ HDI.2005, data = df)
tidy(lfit)

ggplot(df, aes(x = HDI.2005, y = TFR.2005)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", se = F)

# 
vcov(lfit) 

# multivariate random normal
mvrnorm(1, mu = lfit$coefficients, Sigma = vcov(lfit)) 
mvrnorm(2, mu = lfit$coefficients, Sigma = vcov(lfit)) 



# draw 100
sample.coefs <- mvrnorm(100, mu = lfit$coefficients, Sigma = vcov(lfit)) 
sample.coefs[1, ]
head(sample.coefs)

############## Introduction to Loops ##########

# Ex. 1
x <- c(5, 12, 13)
for (n in x){
  print( n ^ 2)
}

# Ex. 2
for (i in 1:10){
  print("Hello world!")
  print(i*i)
}

## which is the same as 
x<- c(1:10)
for (i in x){
  print("Hello world!")
  print(i*i)
}

## and 
x <- c(1:10)
for (snow in x){     # the counter can take any name
  print("Hello world!")
  print(snow*snow)
}


# Ex. 3
sqr <-  seq(1, 100, by=2) # generating sequences of 50 numbers
head(sqr)
sqr.squared <- NULL  ## setting up empty vector
for (n in 1:50) {
  sqr.squared[n] <- sqr[n]^2
}

head(sqr.squared)
head(cbind(sqr, sqr.squared))


############## for Loops ##########

plot(x = df$HDI.2005, y = df$TFR.2005)
abline(sample.coefs[1, ], col = "yellow", lwd = 2)


# let row be everything from 1 to 100
for (row in 1:100) {
  abline(sample.coefs[row, ], col = "yellow", lwd = 2)
} 

#### Introduction to Functions ########

# Creating a function, called f1, which adds a pair of numbers.
f1 <- function(x, y) {
  x + y
}

f1(3, 4)
f1(2, 6)

# Ex. 2
my.scatter.plot <- function(color, size = 2, method) { # set default size to 2
  ggplot(df, aes(x = HDI.2005, y =  TFR.2005)) + 
    geom_point(color = color, size = size) +   # color and size vary based on inputs
    geom_smooth(method = method, color = "red") + # line will always be red
    theme_bw()                                    # theme will always be bw
}

my.scatter.plot(color = "green", size = 6,  method = "lm")
my.scatter.plot(color = "blue",  method = "lm")
my.scatter.plot("red", 5, "loess")

############# Functions ###########

## calculating Root mean squared error RMSE


predicted.TFR <- sample.coefs[1, 1] + sample.coefs[1, 2] * df$HDI.2005
residuals <- df$TFR.2005 - predicted.TFR
RMSE <- sqrt(mean(residuals^2))
RMSE

Calc.RMSE <- function(intercept, slope){
  
  predicted.TFR <- intercept + slope * df$HDI.2005
  residuals <- df$TFR.2005 - predicted.TFR
  RMSE = sqrt(mean(residuals ^ 2))
  return(RMSE)
  
} 


Calc.RMSE(9, -7) # made up intercept and slope

# Draw 40
Calc.RMSE(intercept = sample.coefs[40, 1], slope = sample.coefs[40, 2])
Calc.RMSE(sample.coefs[40, 1], sample.coefs[40, 2]) 

# Draw 50
Calc.RMSE(sample.coefs[50, 1], sample.coefs[50, 2]) 




Matt.rmse <- function(prediction.vec, truth.vec) {
  # function to calculate RMSE
  # for vector of predictions and vector of truth
  # CHECK INPUTS
  if (length(prediction.vec) != length(truth.vec)) {
    stop("ERROR: prediction.vec and truth.vec are not the same length")
  }
  if (any(is.na(prediction.vec))) {
    stop("ERROR: prediction.vec has NA")
  }
  
  if (any(is.na(truth.vec))) {
    stop("ERROR: truth.vec has NA")
  }
  # FINISHED CHECKING INPUTS
  result <- sqrt(mean((prediction.vec - truth.vec)^2))
  # CHECK OUTPUT
  if (is.na(result)) stop("ERROR: result in NA")
  return(result)
}

Matt.rmse(lfit$fitted.value, TFR.2005)
