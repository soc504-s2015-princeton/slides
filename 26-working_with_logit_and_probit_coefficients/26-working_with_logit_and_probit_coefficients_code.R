library(dplyr)
library(broom)
library(ggplot2)

load("data/Hanmer.RData")
df <- tbl_df(x)
df <- filter(df, !is.na(presvote))

#### Odds and log-odds #####

### proportion
df %>%
  filter(white == 1) %>%
  summarise(prob.white = mean(presvote))

df %>%
  filter(white == 0) %>%
  summarise(prob.nonwhite = mean(presvote))

### Odds
odds.w <- df %>%
  filter(white == 1) %>%
  summarise(odds = (mean(presvote) / (1 - mean(presvote))))
odds.w             

odds.nw <- df %>%
  filter(white == 0) %>%
  summarise(odds = (mean(presvote) / (1 - mean(presvote))))
odds.nw



### Log - Odds
log.odds.w <- df %>%
  filter(white == 1) %>%
  summarise(logodds = log((mean(presvote) / (1 - mean(presvote)))))
log.odds.w

log.odds.nw <- df %>%
  filter(white == 0) %>%
  summarise(logodds =  log((mean(presvote) / (1 - mean(presvote)))))
log.odds.nw

### back to probability
exp(log.odds.w$logodds) / (1 + exp(log.odds.w$logodds))
exp(log.odds.nw$logodds) / (1 + exp(log.odds.nw$logodds))

#### function
Inv.logit <- function(logitvals) {
  pred.odds <- exp(logitvals)
  pred.prob <- pred.odds / (1 + pred.odds)
  return(pred.prob)
}

Inv.logit(log.odds.w$logodds)
Inv.logit(log.odds.nw$logodds)


####### One varible logisitics regression

fit1 <- glm(presvote ~  white, data=df, family = binomial)
fit1 <- tidy(fit1)

#### calculate odds 
fit1$estimate
fit1$estimate[1] + fit1$estimate[2] ## 0.3807725
log.odds.w$logodds ## 0.3807725

## odds ratio
exp(fit1$estimate[2])

## odds.w$odds /odds.nw$odds  ## odds ratio

### predicted probabilities 

(exp(fit1$estimate[1] + fit1$estimate[2])) / (1 + exp(fit1$estimate[1] + fit1$estimate[2]))

exp(fit1$estimate[1]) / (1 + exp(fit1$estimate[1]))

##### replication from Hanmer and Kalkan
fit2 <- glm(presvote ~ retecon + partyid + bushiraq + ideol7b + white + female +
             age + educ1_7 + income, data=df, family = binomial)
tidy(fit2)


## observed value approach
df.white <- df
df.nonwhite <- df

df.white$white <- 1
df.nonwhite$white <- 0 

pp.white <- predict(fit2, type = "response", newdata = df.white)
mean(pp.white)

pp.nonwhite <- predict(fit2, type = "response", newdata = df.nonwhite)
mean(pp.nonwhite)

###  marginal effect - observed value
mean(pp.white) - mean(pp.nonwhite) ###  0.007900108


## average case approach
df.mean <- with(df, data.frame(ideol7b = 4, retecon = 0,
                               white = c(0, 1), female = 1, age = mean(age), 
                               educ1_7 = 5, income = 16, partyid = 3,
                               bushiraq = (1/3)))
df.mean
df.mean$pp <- predict(fit2, newdata = df.mean, type = "response")
df.mean

##  marginal effect - average case
df.mean$pp[2] - df.mean$pp[1] ## 0.03552846

### create data frame for graph
graph <- df.mean
graph$pp.white.ova <- c(mean(pp.nonwhite), mean(pp.white))
graph 

ggplot(graph, aes(x = as.factor(white), y = pp)) + geom_point(color = "red") +
  geom_point(aes(y = pp.white.ova), color = "blue") 

