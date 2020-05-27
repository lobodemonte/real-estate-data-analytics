# In-Class Assignment 1:

# Submit to NYU Classes as: [NetID]_1.R.

# 1. Re-run all code above to ensure it works.
# 2. Consider the following list of numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
# Use R to calculate the mean, variance and standard deviation.
# 3. Read in data on the London Interbank Offered Rate from FRED betweeen 2007 and 2016.
# 4. Graph the data.  Describe why the data look the way they do.
# 5. Read in the New York state 2010 Census data.  Create two additional variables.
# a. Population density (number of people per square mile).
# b. Housing density (number of houses per square mile).
# c. Calculate the correlation between these two variables.
# d. Plot housing density against population density.
# e. Interpret your results.

install.packages(c("PASWR2", "MASS", "repmis", "latex2exp", "dplyr", 
                   "ggplot2", "tidyverse", "stargazer", "fredr", "quantmod"))

#If fredr install doesn't work, else comment out
install.packages("C:/Users/erikl/Downloads/fredr_1.0.0.tar.gz", repos = NULL)

# Load the libraries.
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stargazer)
library(fredr)
library(quantmod)

getwd() 
setwd("C:/Users/erikl/OneDrive/Documents/NYU_CUSP/Summer2020/REDA")

#Part 2 
nums = 1:10
mean(nums)
var(nums)
sd(nums)

#Part 3
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.
libor = fredr('USD3MTD156N', observation_start = as.Date("2007-01-01"), observation_end = as.Date("2016-12-31")) # Grab GDP per capita

#Part 4
attach(libor) # Attach and plot.
plot(date, value, type='l', main='LIBOR') # Basic graphs in R.
# The Financial Crash of 2008 seems to have crushed the LIBOR rate for around 4 years 
# This could also be related to the crackdown on LIBOR rate rigging

detach(libor)

#Part 5 
url = "http://www2.census.gov/geo/docs/maps-data/data/gazetteer/census_tracts_list_36.txt"
ny_census = read.csv(url, header=TRUE, sep='\t')
names = c('usps', 'geo', 'pop', 'hu', 'land', 'water', 'landSqmi', 'waterSqmi', 'lat', 'long')
colnames(ny_census) = names

#Part A
popDensity = na.omit( ny_census$pop / ny_census$landSqmi)

#Part B
housingDensity = na.omit(ny_census$hu / ny_census$landSqmi)

#Part C
cor(popDensity, housingDensity)

#Part D
plot(popDensity, housingDensity, pch=16, col="blue", 
     main="Population vs Housing Density", xlab="Population", ylab="Housing")
grid(lw=2)

#Part E
"We can reasonably expect Population and Housing Density to be positively correlated,"
"The bigger the population the more housing you'll need."
"If that weren't the case, we would see major overcrowding."
"Or a waste in resources building housing where it's not needed"
