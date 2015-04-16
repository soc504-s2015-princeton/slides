
##### Load data
#install.packages("nnet")
library(nnet)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stargazer)

load("data/Hanmer.RData")
df <- tbl_df(x)
df <- filter(df, !is.na(presvote))


## create 3 level factor of party identification
df2 <- df %>%
mutate(party = ifelse(df$partyid == 0 | df$partyid == 1, "dem",  ## Democrats
               ifelse(df$partyid == 2 | df$partyid == 3 | df$partyid == 4, "indep",  ## Independents
               ifelse(df$partyid == 5 | df$partyid == 6, "rep", NA))))     ## Republicans 
                            
df2

######## multinomial 
fit3 <- multinom(party ~ presvote + retecon  + bushiraq + ideol7b + white + female +
              age + educ1_7 + income, data=df2)
summary(fit3)
stargazer(fit3, type = "text")


### observed value approach
df2.w <- df2
df2.nw <- df2

df2.w$white <- 1
df2.nw$white <- 0 

df2.w <- cbind(predict(fit3, type = "probs", newdata = df2.w), df2.w)
tbl_df(df2.w)
mean(df2.w$dem)
mean(df2.w$indep)
mean(df2.w$rep)


df2.nw <- cbind(predict(fit3, type = "probs", newdata = df2.nw), df2.nw)
tbl_df(df2.nw)
mean(df2.nw$dem)
mean(df2.nw$indep)
mean(df2.nw$rep)



### average case approach
df2.mean <- with(df2, data.frame(ideol7b = 4, retecon = 0,
                                 white = c(0, 1), female = 1, age = mean(age), 
                                 educ1_7 = 5, income = 16,
                                 bushiraq = (1/3),
                                 presvote = 1))


df2.mean
df3 <-cbind(predict(fit3, newdata = df2.mean, type = "probs"), df2.mean)
df3

## data frame for graph
df3 <- gather(df3, key, value, dem:rep) 

## observed value
ova.value <- c(mean(df2.nw$dem), mean(df2.w$dem), mean(df2.nw$indep), mean(df2.w$indep),
               mean(df2.nw$rep), mean(df2.w$rep))

df3$ova.value <- ova.value
df3

## graph for whites - observed value approach is in blue; average case approach in red
ggplot(filter(df3, white == 1), aes(x = key, y = value )) + geom_point(aes(color = as.factor(white))) +
  geom_point(color = "red") +
  geom_point( aes(y = ova.value), color = "blue") +
  ggtitle("predicted probabilities, whites")

## graph for nonwhites
ggplot(filter(df3, white == 0), aes(x = key, y = value )) +
  geom_point(color = "red") +
  geom_point( aes(y = ova.value), color = "blue") +
  ggtitle("predicted probabilities, nonwhites")
