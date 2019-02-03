#FINAL PROJECT R - Berkeley Poulsen - INST 314

require(openintro)
require(ggplot2)
require(dplyr)

#Read the data, movies.csv
data <- read.csv("/Users/berke/Documents/1_Documentation/INST314/movies.csv")
nrow(data) # 200 movies
ncol(data) # 29 variables asscociated with the movies

#We removed c(143) from the data set, reasons in paper
max(data$budget)
which(data$budget == 1000000000)
data <- data[ -c(143), ]
nrow(data) #We now have 199 rows, Godzilla 1999 is removed

#Regression between budget and box-office
# 1
#Get the gross income
gross <- data$gross
#Get the budget
budget <- data$budget
options("scipen"=100) #changing log form to numerical
head(budget)

# Plot the data
plot(gross, budget, main = "Scatterplot between gross and budget")
abline(0,1)
cor(gross, budget) # Correlation between gross and budget is 0.6227

#This is the main correlation scatterplot between gross income and budget spent
plot(gross, budget)
abline(0,1)
cor(gross, budget)
# The linear correlation R, is 0.6227414

#Plotting with duration
plot(data$gross,  data$duration, main = "Duration and gross income", xlab = "Gross income", ylab = "Duration")
abline(0,1)

cor(data$gross, data$duration)
#The correlation between duration gross income is minimal at best.
# There is not much of a correlation here. it is weak positive and linear.

cor(data$gross, data$imdb_score) #0.2406419
plot(data$gross,  data$imdb_score, main = "Gross income and IMDB score", xlab = "gross", ylab = "IMDB Score")
abline(0,1)

#once again weak positive and linear, nearly vertical in the graph.

#===================================================================================================================

#Confidence Interval

table(data$content_rating) #there are 88 R rated movies
rprop = 88/nrow(data) # about 44.22% of the data is r rated movies
rprop

#For 95% confidence , we'll be using 1.96
# standard Error is
SE = sqrt((rprop)*(1-rprop)/200)
SE

zstar = qnorm(.025, lower.tail = FALSE)
zstar

rprop = 88/nrow(data)

rprop + (SE * zstar) #upper bound = 0.5110419
rprop - (SE * zstar) #Lower bound = 0.3733803

#We are 95% confident the true population proportion of R movies lies
#between (0.3733803, 5110419)

#===================================================================================================================

#Hypothesis Test
# How about lets see if US movies make more than UK movies
# Should be a
# H0 : MuUS - MuUK = 0
# HA : MuUS - MuUK !> 0
# Alpha level is 0.05
# Conditions met

UKtable <- subset(data, country == "UK")
UKmean <- mean(UKtable$gross)
# 22521274

UStable <- subset(data, country == "USA")
USmean = mean(UStable$gross)
# 55343891

t.test(UStable$gross, UKtable$gross, conf.level = 0.95, alternative = "greater")
# T = -3.9805, p-value is less than 0.05
# We reject the null hypothesis, there is a statistical difference between
# The mean of Us movies compared to UK movies.

boxplot(UKtable$gross, UStable$gross, main = "boxplot of Gross income for Uk and USA Movies", xlab = c("UK     US"), ylab = "gross income")

#Hypothesis test
#I want to do a z test for proportions to see

#===================================================================================================================

#Basic descriptions
boxplot(data$gross, main = "Boxplot of gross income", ylab = "Dollars ($)", xlab = "Gross Income")
boxplot(data$budget, main = "Boxplot of sample's budget", ylab = "Dollars ($)", xlab = "budget")

summary(data$gross)
summary(data$duration)
summary(data$budget)
summary(data$imdb_score)

#===================================================================================================================

#Model Process

model_sum = lm(formula = gross ~ budget * duration * imdb_score, data = data)
summary(model_sum)
#remove duration highest pr value, also the interactions it holds

model_sum = lm(formula = gross ~ budget * imdb_score, data = data)
summary(model_sum)
#Budget seems to be quite related with imdb score here, lets conver it into its own interaction

model_sum = lm(formula = gross ~ budget  + imdb_score, data = data)
summary(model_sum)

#End

