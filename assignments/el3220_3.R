# In-class assignment 3:

# Submit to NYU Classes as: [NetID]_3.R.

# Load the libraries.
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
library(RCurl)
library(haven)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.


getwd() 
setwd("C:/Users/erikl/OneDrive/Documents/NYU_CUSP/Summer2020/REDA")


# 1. Re-run all code above to ensure it works.
# 2. Do FF1F, FF3F and FF5F for IBM and Microsoft stock.

runFF <- function(a, b, stock) {
  data = merge(a, b, join='right')  ## Merge into single time-series dataset
  names = c("date", "mktrf", "smb", "hml", "rmw", "cma", "rf", stock) # factors (per Ken French) and AAPL returns
  colnames(data) = names
  data = data[, colnames(data) != "date"]  # Narrow dataframe 
  
  ff1f.ols = lm(paste(stock,'- rf ~ mktrf'), data = data)
  stargazer(ff1f.ols, type="text", title=paste("Baseline Results:", stock), single.row=TRUE, 
            ci=TRUE, ci.level=0.95)
  
  ff3f.ols = lm(paste(stock,'- rf ~ mktrf + smb + hml'), data = data)
  stargazer(ff3f.ols, type="text", title=paste("FF3F Results:", stock), single.row=TRUE, 
            ci=TRUE, ci.level=0.95)
  
  ff5f.ols = lm(paste(stock,' - rf ~ mktrf + smb + hml + rmw + cma'), data = data)
  stargazer(ff5f.ols, type="text", title=paste("FF5F Results:", stock), single.row=TRUE, 
            ci=TRUE, ci.level=0.95)
}

ff5f = read_dta("FF5F.dta") # Data are stored in multiple formats.  
ff5f$mktrf = ff5f$mktrf / 100
ff5f$smb = ff5f$smb / 100
ff5f$hml = ff5f$hml / 100
ff5f$rmw = ff5f$rmw / 100
ff5f$cma = ff5f$cma / 100
ff5f$rf = ff5f$rf / 100

a = xts(x=ff5f, order.by = ff5f$date)


#IBM
getSymbols(c('IBM'), from="2000-01-01", to="2019-12-31") # Read in AAPL data using quantmod.
b = IBM$IBM.Adjusted
b = diff(log(b), lag=1)  ## Log returns
b = na.omit(b)

runFF(a, b, 'ibm')


#MSFT
getSymbols(c('MSFT'), from="2000-01-01", to="2019-12-31") # Read in AAPL data using quantmod.
b = IBM$IBM.Adjusted
b = diff(log(b), lag=1)  ## Log returns
b = na.omit(b)

runFF(a, b, 'msft')

# 3. Replicate the omitted variable bias example inducing negative correlation.  
# (Pay attention to Z1.) <-- X1??

rm(list=ls())
set.seed(1993)
z = rnorm(10000, mean=0, sd=1)  
v = rnorm(10000, mean=0, sd=1)  
w = rnorm(10000, mean=0, sd=1) 
e = rnorm(10000, mean=0, sd=1) 
x1 = z + v
x2 = -1*(z + w)
y = 1 + 1* x1 + 1* x2 + e
cor(x1, x2)
lm = lm(y~x1)
stargazer(lm, title="Regression Results When We Omit Correlated Relevant Variable", 
          single.row=TRUE, type="text", ci.level=0.95, ci=TRUE)

#lm = lm(y~x1+x2)
#stargazer(lm, title="Regression Results When We Include (Negatively) Correlated Relevant Variable", 
#          single.row=TRUE, type="text", ci.level=0.95, ci=TRUE)

# Now that they're negatively correlated, X1 is moved to the left to compensate for the missing X2
# In our specific case, If both are included X1 and X2 are 1
# If X2 is missing and positively Correlated --> X1 is 1.5
# If X2 is missing and negatively correlated --> X1 is .5
# ^^^Assuming X1, X2 were generated our same way


# 4. In most circumstances, we seek to draw inferences about the effects of some type of policy or action.
# Considering the ideas discussed in class regarding causality:
# Do opportunity zones actually result in benefitial development, 
# and how could we evaluate potential benefits using data? 

# If an OZ actually benefits the community depends on what you choose to measure as an indicator of success
# Lets suppose an investor brings a tech office to an OZ (and that it qualifies as an OZ investment)
# If you use real estate prices as an indicator of success
#, you may think a rise in housing prices shows success when it could actually be indicating a displacement
#  of the original population as white-collar workers buy-out OZ real estate.
# However if you instead decide to track employment outcomes for "original" OZ residents (which sounds harder to do)
# , you could have a better idea of the impact of OZ investments. 
# Determining OZ benefits depends on who is receiving the benefits we are measuring.   



