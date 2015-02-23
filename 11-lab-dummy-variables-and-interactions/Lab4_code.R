##### “UN Data on Life Expectancy.”

library(ggplot2)
library(dplyr)
df <-read.table("http://socserv.socsci.mcmaster.ca/jfox/Books/Applied-Regression-2E/datasets/UnitedNations.txt")
df$country <- rownames(df)
rownames(df) <- NULL
df <- tbl_df(df)
df

###
# data <- read.table("data/Prestige.txt", header=TRUE)
# data$occupation <- rownames(data)
# rownames(data) <- NULL
# data <- tbl_df(data)
# data

ggplot(df, aes(x = educationFemale, y = lifeFemale)) + 
  geom_point(color = "blue") #+ 
  geom_point(aes(x = educationMale, y = lifeMale), 
             color = "red")

ggplot(df, aes(x = educationMale, y = lifeMale)) + 
  geom_point(color = "red") 

ggplot(df, aes(x = lifeMale, y = tfr)) + 
  geom_point(color = "red") 

ggplot(df, aes(x = lifeFemale, y = tfr)) + 
  geom_point(color = "blue") 

ggplot(df, aes(x = educationFemale, y = lifeFemale)) + 
  geom_point(color = "blue") + 
geom_point(aes(x= educationMale, y = lifeMale),
           color = "red")

ggplot(df, aes(x = lifeMale, y = lifeFemale)) + 
  geom_point() 


### Male and Female Life Expectancy



ggplot(df, aes(x = lifeFemale, y = lifeMale)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0) 
  # female LE is higher

# By region
ggplot(df, aes(x = lifeFemale, y = lifeMale, color = region)) + 
  geom_point()  

ggplot(df, aes(x = lifeFemale, y = lifeMale)) + 
  geom_point() + facet_grid(. ~ region) 

ggplot(df, aes(x = lifeFemale, y = lifeMale, color = region)) + 
  geom_point() + facet_grid(. ~ region)

ggplot(df, aes(x = lifeFemale, y = lifeMale, label=rownames(df))) + 
    geom_text(size = 3)

## Comparing Regions

# dummy variables
# mutate(df, europe = region == "Europe"))
# fit.df <- tidy(fit)
# print(fit.df)

# 
# data <- mutate(data, prof = as.numeric(type=="prof"))
# data <- mutate(data, white.collar = as.numeric(type=="wc"))
# fit <- lm(prestige ~ education + prof + white.collar + education:type, data = data)
# tidy(fit)


df$europe <- df$region == "Europe" # will read as 1/0 even though it shows up T/F
df$noteurope <- df$region != "Europe" # only 1 equal sign

df$euro <- ifelse(df$region == "Europe", 1, 0)

# regression
fit <- lm(lifeFemale ~ europe + noteurope - 1, data = df) # what does -1 mean
summary(fit)
fit <- lm(lifeFemale ~ europe + noteurope, data = df) # what does -1 mean
summary(fit)

fit <- lm(lifeFemale ~ europe, data = df)
summary(fit)

## regression with region
str(df$region)

fit.2 <- lm(lifeFemale ~ region, data = df) # if you give R a factor variable it will 
#create the dummies for you
summary(fit.2)

##  U.S. and europe?
levels(df$region)
df$region <- relevel(df$region, ref = "Europe")
levels(df$region)

fit.3 <- lm(lifeFemale ~ region, data = df) 
summary(fit.3)


####  Life Expectancy and GDP

# plot
ggplot(df, aes(x = educationFemale, y = lifeFemale, color = region)) + 
  geom_point()  


fit.ed <- lm(lifeFemale ~ educationFemale, data = df)
summary(fit.ed)

ggplot(df, aes(x = educationFemale, y = lifeFemale)) + 
  geom_point()  + geom_smooth(method = "lm", se = F)

ggplot(df, aes(x = educationFemale, y = lifeFemale, color = region)) + 
  geom_point()  + geom_smooth(method = "lm", se = F)
