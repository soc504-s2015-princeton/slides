## Slide 3 ####

library(dplyr)
library(ggplot2)
data <- read.csv("data/nature08230-s2.csv")
data <- select(data, country, HDI.1975, TFR.1975, HDI.2005, TFR.2005)

## Slide 3 ####

ggplot(data, aes(x = country, y = HDI.1975)) + geom_point()
ggplot(data, aes(x = country, y = TFR.1975)) + geom_point()
ggplot(data, aes(x = country, y = HDI.2005)) + geom_point()
ggplot(data, aes(x = country, y = TFR.2005)) + geom_point()

## scatter plots 
ggplot(data, aes(x = HDI.1975, y = TFR.1975)) + geom_point()
ggplot(data, aes(x = HDI.2005, y = TFR.2005)) + geom_point()

# Bar graphs
ggplot(data, aes(x = TFR.1975)) + geom_bar()
ggplot(data, aes(x = HDI.1975)) + geom_bar()

ggplot(data, aes(x = TFR.2005)) + geom_bar()
ggplot(data, aes(x = HDI.2005)) + geom_bar()



## Slide 5 - Using lm()

lfit <- lm(TFR.1975 ~ HDI.1975, data = data)
summary(lfit)

## Slide 6 - An lm() Object

str(lfit)
lfit$coefficients
ggplot(data, aes(x = HDI.1975, y = TFR.1975)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

no.miss <- filter(data, !is.na(HDI.1975))
head(cbind(no.miss$HDI.1975, lfit$fitted.values, lfit$residuals))

library(broom)
lfit.broom <- lm(TFR.1975 ~ HDI.1975, data = data)
tidy(lfit.broom)

## Slide  7 - Residuals

lfit.fort <- fortify(lfit)
head(lfit.fort)

ggplot(lfit.fort, aes(x = .fitted, y = .resid)) + geom_point()

ggplot(lfit.fort, aes(x = .fitted, y = .resid)) + geom_point() + geom_smooth()



## Slide  8 - Comparing 1975 to 2005


ggplot(data, aes(x = HDI.1975, y = TFR.1975)) + geom_point() 
ggplot(data, aes(x = HDI.1975, y = TFR.1975)) + geom_point() + geom_point(aes(x = HDI.2005, y = TFR.2005), color = "red")


lfit2005 <- lm(TFR.2005 ~ HDI.2005, data = data)
summary(lfit2005)
summary(lfit)


ggplot(data, aes(x = HDI.2005, y = TFR.2005)) + geom_point(color = "red") +
  geom_smooth(method = "lm", se = F) 
ggplot(data, aes(x = HDI.2005, y = TFR.2005)) + geom_point(color = "red") +
  geom_smooth(method = "lm", se = F, color = "red") + geom_point(aes(x = HDI.1975, y = TFR.1975)) 
ggplot(data, aes(x = HDI.2005, y = TFR.2005)) + geom_point(color = "red") +
  geom_smooth(method = "lm", se = F, color = "red") + geom_point(aes(x = HDI.1975, y = TFR.1975)) + 
  geom_abline(intercept = 10.9075, slope = -9.6221)

# bonus residuals

lfit.fort2005 <- fortify(lfit2005)
head(lfit.fort2005)

ggplot(lfit.fort2005, aes(x = .fitted, y = .resid)) + geom_point()
ggplot(lfit.fort2005, aes(x = HDI.2005, y = .resid)) + geom_point()

ggplot(lfit.fort2005, aes(x = .fitted, y = .resid)) + geom_point() + geom_smooth()
ggplot(lfit.fort2005, aes(x = HDI.2005, y = .resid)) + geom_point() + geom_smooth()

