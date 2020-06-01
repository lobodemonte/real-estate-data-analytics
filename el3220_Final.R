# 3135 May Intensive: Final Examination.

# You may use all materials presented in class, as well as online resources.  
# You may not, however, assist other students in this course with their answers.
# You are bound by NYUSPS Policy on Academic Integrity and Plagiarism.
# Submit to NYU Classes as: [NetID]_Final.R.

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
library(forecast)
library(depmixS4)
library(mFilter)
fredr_set_key('30e6ecb242a73869e11cb35f6aa3afc3') # My key, please don't abuse.

getwd() 
setwd("C:/Users/erikl/OneDrive/Documents/NYU_CUSP/Summer2020/REDA")

# Question 1.
# Consider the following sequence of numbers, called x1: 6 2 9 7 7 1 9 9 6 5 6 1 4 7 9 1 0 5 5 3
# Using R, calculate the mean, variance and standard deviation of x1.
# Consider another sequence of numbers, called x2: 9 5 8 1 9 2 2 6 9 8 5 1 0 2 7 4 9 7 0 9
# Using R, calculate the covariance and correlation between x1 and x2.
# Using R, run the bivariate linear model that regresses x2 on x1.  
# Based on a null hypothesis of no relationship, what is the 95% confidence interval on the slope coefficient?
# Would you reject or fail to reject the null hypothesis at this level of confidence?

x1 <-  c(6, 2, 9, 7, 7, 1, 9, 9, 6, 5, 6, 1, 4, 7, 9, 1, 0, 5, 5, 3)

mean(x1)
var(x1)
sd(x1)

x2 <- c( 9, 5, 8, 1, 9, 2, 2, 6, 9, 8, 5, 1, 0, 2, 7, 4, 9, 7, 0, 9)

cov(x1, x2)
cor(x1, x2)

ols = lm(x1 ~ x2)
stargazer(ols, type="text", title="OLS Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

## Slope CI at 95%: 0.105 (-0.296, 0.506) 
## We would FAIL to reject the null hypothesis because the zero slope is included in the CI, showing no relation between x1 and x2.


# Question 2.
# For a publicly-traded stock of your choice, download data on the adjusted closing
  # stock price, as well as the market (or exchange) on which it trades between 2006 and 2017.
# Calculate the daily returns for both the stock and the market on which it trades.
# Generate graphs of closing adjusted prices for both.
# Generate graphs of returns over time for both.
# Generate histograms of returns for both.
# Using a bivariate linear regression, estimate the CAPM presented in class.
# Assuming the EMH, test the basic CAPM hypotheses using your data.
# Integrate the Fama-French 5 Factor data presented in class.  
# Estimate the Fame-French 3-facotr and 5-factor models.
# Describe how your results change as you move from the 3- to the 5-factor model, addressing changes in the adjusted beta,
# as well as changes in conclusions regarding hypothesis testing of the adjusted beta.

getSymbols(c('GS','^NYA'), from="2006-01-01", to="2017-12-31") 
data = merge(as.zoo(GS$GS.Adjusted), as.zoo(NYA$NYA.Adjusted))
names = c("GS", "NYA")
colnames(data) = names

data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values

plot(GS$GS.Adjusted, main="Goldman Sachs Closing Prices", col = "darkgreen")
plot(NYA$NYA.Adjusted, main="NYSE Closing Prices", col = "darkgreen")

plot(data.returns$GS, main="Goldman Sachs Daily Returns", col = "darkgreen")
plot(data.returns$NYA, main="NYSE Daily Returns", col = "darkgreen")

hist(data.returns$GS, breaks=100, col="darkblue", freq=F, 
     main="Histogram of GS Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")

hist(data.returns$NYA, breaks=100, col="darkblue", freq=F, 
     main="Histogram of GS Daily Returns (2006-2017)", xlab="Daily Returns", 
     xlim=c(-.2, .2))  
abline(v=0, col="red")

plot.ts(data.returns$NYA, data.returns$GS, pch=16, 
        col="darkblue", main="CAPM Data", xlab="Returns of NYSE", 
        ylab="Returns of GS")  ## time series plot in R  
grid(lw = 2)
abline(lm(data.returns$GS ~ data.returns$NYA), col="red")  ## I added the best fit line so the graph looks similar to that presented in class for Apple.library(ggplot2)

capm.ols = lm(data.returns$GS ~ data.returns$NYA)
stargazer(capm.ols, type="text", title="CAPM Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.99)
## Results
##NYA                   1.350*** (1.291, 1.409)  
##Constant              0.0001 (-0.001, 0.001) 
## Assuming EMH, our NULL HYPOTHESIS: A == 0 && B == 1, ALT HYPOTHESIS: A != 0 || B != 1, where A is the constant and B is the slope 
## Using a 99% confidence interval we REJECT the null hypothesis
## Although Zero is within the CI for Alpha, Beta's CI doesn't include a slope of 1, indicating that the GS stock is aggressive with no excess returns.

ff5f = read_dta("FF5F.dta") # Data are stored in multiple formats.  
ff5f$mktrf = ff5f$mktrf / 100
ff5f$smb = ff5f$smb / 100
ff5f$hml = ff5f$hml / 100
ff5f$rmw = ff5f$rmw / 100
ff5f$cma = ff5f$cma / 100
ff5f$rf = ff5f$rf / 100

a = xts(x=ff5f, order.by = ff5f$date)
b = GS$GS.Adjusted
b = diff(log(b), lag=1) 
b = na.omit(b)

merged_data = merge(a, b, join='right')  ## Merge into single time-series dataset
names = c("date", "mktrf", "smb", "hml", "rmw", "cma", "rf", "gs")
colnames(merged_data) = names
merged_data = merged_data[, colnames(merged_data) != "date"]  # Narrow dataframe 

ff3f.ols = lm('gs - rf ~ mktrf + smb + hml', data = merged_data)
stargazer(ff3f.ols, type="text", title="FF3F Results: GS", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

ff5f.ols = lm('gs - rf ~ mktrf + smb + hml + rmw + cma', data = merged_data)
stargazer(ff5f.ols, type="text", title="FF5F Results: GS", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

## Moving from 3-factor to 5-factor we see that the adjusted beta is still above (not even within CI), so again we see that 
## GS stock is aggresive with no excess returns, so we REJECT the null hypothesis. 
## Should be noted that GS stock isn't as aggresive once we move from 3-factor to 5-factor
## No chances in our conclusion

# Question 3.
# Read in the Staten Island sales data presented in class.
# Suppose your focus was not interpretation but prediction, and you wanted to introduce non-linearities in the model.
# Create square and cubed variables for age.  (A cube is x^3.)
# Implement your approach using R and characterize your results as you think appropriate.

SI_Sales = read_dta("SI Sales.dta")
SI_Sales$age2 = SI_Sales$age ^2
SI_Sales$age3 = SI_Sales$age ^3

cor(SI_Sales)

model = lm('price ~ unit_size + land_size + age + age2 + age3', data = SI_Sales)
stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 2)

## From our results we see that as time progresses, initially age will be part of the appreciation of the house. 
## But eventually the age of the house becomes a liability as Age2 and Age3 start to overtake Age and negatively affect the value of a house
## But we should also note that Age2 Age3 have nonnegative values in their CI, 
## perhaps showing a chance that some homes will keep appreciating regardless of the house (e.g. value of land outstrips decay of the house) 
## We introduced unit-size and land_size since they're correlated to age

# Question 4.
# Use the stock data from Question 2.
# Using the Autocorrelation Function (ACF) in the library "forecast", do you find that returns are autocorrelated?
# Calculate daily volatility of returns.
# Is volatility autocorrelated?
# Are these results consistent with the strong form of the EMH?

getSymbols(c('GS'), from="2006-01-01", to="2017-12-31") 
data = merge(as.zoo(GS$GS.Adjusted))
names = c("GS")
colnames(data) = names
data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values
data.returns$GSvol = sqrt(data.returns$GS ** 2)

acf(data.returns$GS, lag.max=5, type=c("correlation"), 
    main="Autocorrelation of GS Returns")
## For GS we don't see previous days affecting today's stock daily returns, so no autocorrelation for daily returns

acf(data.returns$GSvol, lag.max=10, 
    type=c("correlation"), main="Autocorrelation of GS Volatility")

## GS stock Volatility shows that previous days do have an impact on today's stock volatility, so yes it's autocorrelated for volatility  
## But there's no autocorrelation when it comes to GS stock daily returns
##These results are in line with the strong form of EMH


# Question 5.
# Download quarterly US GDP data from FRED as far back as you are able to go.
# Graph the data.
# Using the HP Filter, decompose these data into cyclical and trend components.
# Graph your results.
# How would you interpret the results given economic history?

gdp = fredr('GDP', observation_start = as.Date("1947-01-01"))
plot(gdp$date, gdp$value, pch=16, col="darkblue", main="GDP", 
     xlab="Date", ylab="GDP ($Billions")  ## time series plot in R
lines(gdp$date, gdp$value, col="darkblue")
grid(lw=2)

hp = hpfilter(gdp$value, freq=129600, type="lambda", drift=FALSE)

plot(gdp$date, hp$trend, pch=16, col="blue", 
     xlab = "Date", ylab = "Trend GDP",
     main="De-Seasonalized Trend")
lines(gdp$date, hp$trend, col="blue")
grid(lw=2)

plot(gdp$date, hp$cycle, pch=16, col="blue", 
     xlab = "Date", ylab = "Seasonal GDP Changes", 
     main="Seasonal Cyclicality")
lines(gdp$date, hp$cycle, col="blue")
grid(lw=2)

## From my results the GDP trends up.
## But its seasonality has recently been more wild, with huge swings in growth and steep declines
## The results point to the fact that in this new millenium, steep economic collapse has been followed by strong economic growth


# Extra Credit.
# Ingest the 2000 Census Data for the state of New Jersey.
# Using the naming conventions in class, rename the variables.  
# Plot the longitude and latitude of the centroids in New Jersey.
# Are they consistent with the population clusters of New Jersey?

url = "http://www2.census.gov/geo/docs/maps-data/data/gazetteer/census_tracts_list_34.txt"
data = read.csv(url, header=TRUE, sep='\t')
names = c('usps', 'geo', 'pop', 'hu', 'land', 'water', 
          'landSqmi', 'waterSqmi', 'lat', 'long')
colnames(data) = names
plot(data$long, data$lat, pch=16, col="blue", 
     main="The Garden Stte by Census Centroid", xlab="Longitude", ylab="Latitude")
grid(lw=2)
## Yes these pop clusters are consistent to what we would expect
## namely population clusters near NYC and Philadelphia and between that corridor
## plus the clusters along the beach like Atlantic City