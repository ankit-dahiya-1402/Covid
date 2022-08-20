#Remove all variables stored previously
rm(list = ls())


#  We have Covid dataset and we want to answer some of the cliams that people are making about it.


# First we will import data set.
raw_data <- read.csv("C:/Users/91880/OneDrive/Desktop/R/Covid/COVID19_line_list_data.csv")

# Let's view the data set
View(raw_data)

# Install all the required packages
install.packages('Hmisc')
install.packages('tidyverse')

# Call library of packages
library(Hmisc)
library(tidyverse)

# We will look out Describing data
describe(raw_data)

# As we show there where 7 columns which are empty so removing 
# those columns.

data <- raw_data %>% discard(~all(is.na(.) | .==""))

# As death column also has dates so we have to clean the data

unique(data$death)
data$death_dummy <- as.integer(data$death !=0)
unique(data$death_dummy)

# Let's check the Death rate of covid death
death_rate <- sum(data$death_dummy)/ nrow(data)
death_rate # 5.8%

mean(data$death_dummy)
 
# AGE Claim: People who died were older

dead = subset(data,death_dummy==1)
alive= subset(data,death_dummy==0)

mean(dead$age , na.rm = TRUE) # 68.5 yr
mean(alive$age, na.rm = TRUE) # 48.0 yr

# Is this claim is statistically significant ?

t.test(dead$age,alive$age,alternative = "two.sided",conf.level = 0.95 )
#95% confidence: people who are died were 17 to 24 yr older than alive

t.test(dead$age,alive$age,alternative = "two.sided",conf.level = 0.99 )
#99% confidence: people who are died were 15.5 to 25.5 yr older than alive


# Normally, if p-value < 0.05, we reject null hypothesis. Here p-value=2.2e-16 ~ 0, so we reject null hypothesis and conclude that data is statistically significant.



# GENDER Claim: gender has no effect on covid death

female = subset(data,gender=="female")
male   = subset(data,gender=="male")

mean(female$death_dummy , na.rm = TRUE) # 3.7% death rate
mean(male$death_dummy, na.rm = TRUE) # 8.5% death rate

# Is this claim is statistically significant ?

t.test(male$death_dummy,female$death_dummy,alternative = "two.sided",conf.level = 0.95 )
#95% confidence: men have from 1.7% to 7.8% higher chances of dying

t.test(male$death_dummy,female$death_dummy,alternative = "two.sided",conf.level = 0.99 )
#99% confidence: men have from 0.8% to 8.8% higher chances of dying

# p-value = 0.002 < 0.05, so this is statistically significant


# Conclusion : Old are dying more then younger one's and MALE are more vulnerable the FEAMLE. Old MALE requires more attention and care.








