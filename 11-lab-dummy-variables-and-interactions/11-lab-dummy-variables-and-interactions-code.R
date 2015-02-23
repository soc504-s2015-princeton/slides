##### “UN Data on Life Expectancy.”

library(ggplot2)
library(dplyr)
library(broom)
df <-read.table("http://socserv.socsci.mcmaster.ca/jfox/Books/Applied-Regression-2E/datasets/UnitedNations.txt")
df$country <- rownames(df)
rownames(df) <- NULL
df <- tbl_df(df)
df

######## Explore ########

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


### Male and Female Life Expectancy ###########

### 1. statistiscs

df %>%
  summarise(mean.life.female = mean(lifeFemale, na.rm = T)) 

df %>%
  summarise(mean.life.male = mean(lifeMale, na.rm = T)) 

df %>%
  summarise(diff.life = mean(lifeFemale, na.rm = T) - 
              mean(lifeMale, na.rm = T))
 
### 2. Visually 

ggplot(df, aes(x = lifeMale, y = lifeFemale)) + 
  geom_point() + geom_abline(slope = 1, intercept = 0) 
 
### 3. By region statistics 

df %>%
  group_by(region) %>%
  mutate(mean.life.female = mean(lifeFemale, na.rm = T)) %>%
  mutate(mean.life.male = mean(lifeMale, na.rm = T))  %>%
  mutate(diff.life = mean.life.female - mean.life.male) %>%
  select(region, mean.life.female, mean.life.male, diff.life)

class(df$region)

### 4. By region visually 
ggplot(df, aes(x = lifeFemale, y = lifeMale, color = region)) + 
  geom_point()  

ggplot(df, aes(x = lifeFemale, y = lifeMale)) + 
  geom_point() + facet_grid(. ~ region) 

ggplot(df, aes(x = lifeFemale, y = lifeMale, color = region)) + 
  geom_point() + facet_grid(. ~ region)


# 5. Challenge
ggplot(df, aes(x = lifeFemale, y = lifeMale, label=country)) + 
    geom_text(size = 3)

######### Comparing Regions ############

## 1. and 2. Create dummies
df <- df  %>% 
  mutate(europe = as.numeric(region == "Europe")) %>%
  mutate(not.europe = as.numeric(region != "Europe"))

## 3. regression

fit <- lm(lifeFemale ~ europe, data = df)
tidy(fit)

fit <- lm(lifeFemale ~ not.europe, data = df)
tidy(fit)


# df %>%
#   filter(region != "Europe") %>%
#   summarise(mean = mean(lifeFemale, na.rm = T))
# 
# df %>%
#   filter(region == "Europe") %>%
#   summarise(mean = mean(lifeFemale, na.rm = T))
#  65.91558 + 11.55584


## 4. regression with region

fit.2 <- lm(lifeFemale ~ region, data = df) # if you give R a factor variable it will 
#create the dummies for you
tidy(fit.2)

##  5. Challenge U.S. and europe?
levels(df$region)
df$region <- relevel(df$region, ref = "Europe")
levels(df$region)

fit.3 <- lm(lifeFemale ~ region, data = df) 
tidy(fit.3)
summary(fit.3)

####  Life Expectancy and GDP ############

# 1. Plot
ggplot(df, aes(x = educationFemale, y = lifeFemale, color = region)) + 
  geom_point()  


## 2. One dummy
fit.ed <- lm(lifeFemale ~ educationFemale, data = df)
fit.ed <- tidy(fit.ed)

ggplot(df, aes(x = educationFemale, y = lifeFemale)) + 
  geom_point()  + geom_smooth(method = "lm", se = F)

ed.10 <- fit.ed$estimate[1] + fit.ed$estimate[2]*10
ed.10
ed.11 <- fit.ed$estimate[1] + fit.ed$estimate[2]*11
ed.11
ed.11 - ed.10
fit.ed

## 3. Two dummies
fit.ed.euro <- lm(lifeFemale ~ educationFemale + europe, data = df)
fit.ed.euro <- tidy(fit.ed.euro)
fit.ed.euro

# 12 years, european country 
fit.ed.euro$estimate[1] + fit.ed.euro$estimate[2]*12 +  fit.ed.euro$estimate[3]

# 12 years, non-european country
fit.ed.euro$estimate[1] + fit.ed.euro$estimate[2]*12 


inter.non.euro <- as.numeric(filter(fit.ed.euro, term =="(Intercept)") %>%
                           select(estimate))
inter.euro <- inter.non.euro + as.numeric(filter(fit.ed.euro, term == "europe") %>%
                                          select(estimate))
slope <- as.numeric(filter(fit.ed.euro, term =="educationFemale") %>%
                      select(estimate))


ggplot(df, aes(x=educationFemale, y=lifeFemale, colour = as.factor(europe))) +
  geom_point() + scale_color_manual(values = c("red", "blue")) +
    geom_abline(intercept = inter.non.euro, slope = slope, color = "red") +
   geom_abline(intercept = inter.euro, slope = slope, color = "blue")


## 4. Only Europe
fit.euro <- lm(lifeFemale ~ educationFemale, 
               data = filter(df, europe == 1))
fit.euro <- tidy(fit.euro)
fit.euro

fit.euro$estimate[1] + fit.euro$estimate[2]*12 

ggplot(filter(df, europe == 1), 
       aes(x = educationFemale, y = lifeFemale)) + 
  geom_point() + geom_smooth(method = "lm", se = F)

# country labels
ggplot(filter(df, europe == 1), 
       aes(x = educationFemale, y = lifeFemale, label = country)) + 
  geom_smooth(method = "lm", se = F) + geom_text(size = 3)

## 5. - Only America

fit.am <- lm(lifeFemale ~ educationFemale +
               europe, data = filter(df, region == "America"))
tidy(fit.am)

ggplot(filter(df, region == "America"),
       aes(x = educationFemale, y = lifeFemale)) + 
  geom_point() + geom_smooth(method = "lm", se = F) 

# country labels
ggplot(filter(df, region == "America"),
       aes(x = educationFemale, y = lifeFemale, label = country)) + 
    geom_smooth(method = "lm", se = F) + geom_text(size = 3)

## 6. Interaction

fit.f.euro <- lm(lifeFemale ~ educationFemale + europe + 
                   educationFemale:europe, data = df)
tidy(fit.f.euro)

ggplot(df,
       aes(x = educationFemale, y = lifeFemale, color = as.factor(europe))) +
  geom_point() + scale_color_manual(values = c("red", "blue")) +
  geom_abline(slope = 2.455864, intercept = 44.134927, color = "red") +   
   geom_abline(slope = (2.455864 -1.036607), intercept = (44.134927 + 14.563841), 
               color = "blue") +  xlim(0, 17)  + ylim(40, 90)


########## Ambitious #########

fit <- lm(lifeFemale ~ educationFemale  + region, data = df)
tidy(fit)

fit <- lm(lifeFemale ~ educationFemale  + region + 
            educationFemale:region, data = df)
tidy(fit)


