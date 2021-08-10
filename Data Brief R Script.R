#set working directory
setwd("/file_path/")
#load package
library(readxl)
library(ggplot2)

#set the cars dataset as a variable
cars <- read_excel("cars.xlsx")

View(cars)

regression_viz_hp <- ggplot(cars, aes(x = hp, y = mpg)) + geom_point() +
  stat_smooth(method = lm) + 
  labs(title = 'Suggested Model Scatterplot', subtitle = 'mpg~hp', x = 'Horsepower', y = 'Miles per Gallon')

regression_viz_hp

regression_viz_wt<-ggplot(cars, aes(x = wt, y = mpg)) + geom_point() +
  stat_smooth(method = lm) +
  labs(title = 'Alternative IV for Suggested Model', subtitle = 'mpg~wt', x = 'Weight (1000lbs)', y = 'Miles per Gallon')

regression_viz_wt

#weight looks like a more linear relationship


#setting the model suggested as an environment object
suggested_model <- lm(mpg~hp, data = cars)

summary(suggested_model)

confint(suggested_model)

anova(suggested_model)


#this model isn't very good

#create log variables for mpg, hp, and wt.
cars$logmpg <- log(cars$mpg)

cars$loghp <- log(cars$hp)

cars$logwt <- log(cars$wt)



#I think the regression will fit more smoothly with a log log transformation
#Using ggplot to check this hypothesis before i build a new model
#mpg is the dependent variable in both cases
#two plots of weight and horsepower are below

regression_logwt <-ggplot(cars, aes(x = logwt, y = logmpg)) + geom_point() +
  stat_smooth(method = lm)

regression_logwt

regression_loghp <- ggplot(cars, aes(x = loghp, y = logmpg)) + geom_point() +
  stat_smooth(method = lm)

regression_loghp

#the log log transformation ostensibly is tighter

#create a new log log model with DV = mpg and IV = hp + wt
#lets see if the error term goes down
log_log_model <- lm(logmpg~loghp+logwt, data = cars)

summary(new_model)

confint(new_model)

anova(new_model)

#the errors are much lower than the original model and a simple multi-variable model
#without the log log transformation

#maybe transforming hp into 1/hp would better describe the relationship between mpg~hp

cars$invhp <- 1/(cars$hp)

inv_model <- lm(mpg~invhp+wt, data = cars)

summary(inv_model)

regression_invhp <-ggplot(cars, aes(x = invhp, y = mpg)) + geom_point() +
  stat_smooth(method = lm) 

regression_invhp  





