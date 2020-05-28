# In-Class Assignment 2:

# Submit to NYU Classes as: [NetID]_2.R.

library(quantmod)
library(stargazer)
library(fredr)
library(tidyr)
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(fredr) # Another library to import data from FRED
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3')

# 1. Re-run all code above to ensure that it works.
# 2. Do CAPM for IBM stock for a period of your choice and test the standard hypotheses from CAPM.

getSymbols(c("IBM","^NYA"), from="2000-01-01", to="2019-12-31")
IBM = IBM$IBM.Adjusted
NYX = NYA$NYA.Adjusted

data = merge(as.zoo(IBM), as.zoo(NYX))
names = c("IBM", "NYX")
colnames(data) = names

data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values

plot.ts(data.returns$NYX, data.returns$IBM, pch=16, col="darkgreen", 
        main="CAPM Data 2000-2019", xlab="Returns of NYX", ylab="Returns of IBM",
        xlim=c(-.1,.1),  ylim=c(-.1,.1) )  
abline(lm(data.returns$IBM ~ data.returns$NYX), col="red")
grid(lw = 2)

capm.ols = lm(data.returns$IBM ~ data.returns$NYX)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.99)

# Our NULL HYPOTHESIS: A == 0 && B == 1, ALT HYPOTHESIS: A != 0 || B != 1 
# At 99% CI, Alpha==0 (the constant) is within CI, but Beta==1 (the slope) is not within CI
# We REJECT the NULL HYPOTHESIS
# THUS, IBM seems to have no excess returns, and is less sensitive than the NYSE 


# 3. Import data on short-term and long-term U.S. Treasurys for a period of your choice.  
# Examine the null hypothesis of no relationship between short-term and long-term U.S. Treasurys.

threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01") )) 
tenyear    = drop_na(fredr(series_id = "DGS10",  observation_start = as.Date("2000-01-01"), observation_end = as.Date("2020-01-01") )) 

tenyear$change = Delt(tenyear$value, type=c("arithmetic"))
threemonth$change = Delt(threemonth$value, type=c("arithmetic"))
tenyear$change[is.na(tenyear$change)] = 0
threemonth$change[is.na(threemonth$change)] = 0
tenyear$change[is.infinite(tenyear$change)] = 0
threemonth$change[is.infinite(threemonth$change)] = 0

plot(threemonth$change, tenyear$change, 
     xlab=TeX("3 Month Yield Changes"), ylab=TeX("10 Year Yields"), 
     main="Daily Interest Rate Changes 2000-2019", pch=16, col='darkblue')
grid(lw = 2)
abline(lm(tenyear$change ~ threemonth$change), col='red')

rates.ols = lm(tenyear$change ~ threemonth$change)
stargazer(rates.ols, type="text", title="Interest Rate Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.99)

# Our NULL HYPOTHESIS: 100 BPS CHANGE IN 3MO NOTE RATES LEADS TO >= 50 BPS CHANGE IN 10YR BOND RATES 
# At 99% CI, HOWEVER, WE DO NOT FIND OUR DESIRED OUTCOME IN 10 YR BOND RATES
# We REJECT the NULL HYPOTHESIS
# THUS, there seems to be no relation between the Treasury 3 month note rates and 10 yr bond rates BETWEEN 2000-2019


# 4. Replicate the rent and vacancy results.  
# Write a paragraph about whether rents drive vacancy or vacancy drives rents.  
# Consider the DiPasquale-Wheaton 4-Quadrant Model when doing so.
# Are there ways that we could develop an experiment to examine the relationship?

# In my opinion, it is vacancies that determine rents, not the other way around (at least in this limited scenario)
# If you assume a fixed amount of homogeneous Real Estate, and a number of potential tenants that varies depending on economic performance
# In an economic downturn, a property owner seeking max asset utilization (and profit), they would have to fight for a limited number of tenants
#, an there's a limited number of times you can redo the Lobby before you start lowereing your rents to steal tenants from others.
# In an economic upturn, there's more people looking to start businesses, more Tenants but the same fixed available Square Footage,
#, increasing the demand (and the rent) for a limited resource. 

# For an experiment it would be helpful to model the relationship between Tenants and Property Owners on a computer simulation. 
# In this simulation you can control for things like location premiums, asset depreciation. 
# You can then model each tenants as having a rent budget (of quantity normally distributed), and the goal to seek shelter for the lowest price
# Owners would be models with the goals to maximize their income from rents  
# On each run of the simulation you can change the amount of sq footage and/or the number of tenants (and their purchasing power)
#, and look at the mean rents at the end of the simulation(s) 

data = read.csv('nyc.csv')
names = c("geo", "date", "rent", "vacancy", "cap_rate", "gross_rent")
colnames(data) = names

plot(vacancy, rent, col="blue", main="NYC Office Rent Index v. Vacancy", pch=16, 
     xlab="Vacancy", ylab="Rent Index (2008 $)", xlim=c(0, 20), ylim=c(20, 80))
abline(lm('rent ~ vacancy'), col="red")
grid(lw=2)

model = lm('rent ~ vacancy')
stargazer(model, type="text", title="What Causes What?", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)


