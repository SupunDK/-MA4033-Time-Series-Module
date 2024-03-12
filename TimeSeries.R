# ================= Initial Setup =================
# installing libraries
install.packages("tseries")
install.packages("TSA")
install.packages("lmtest")

# loading libraries
library(TSA) 
library(tseries)
library(lmtest)

# setting the working directory
setwd("/home/supun/R/Time_series_codes")

# ================= Setting up the dataset =================
# reading the dataset
ts <- read.csv("examples.csv")

ts1 <- ts["yt1"]

# you can calculate the ACF, PACF in the current form of the dataset 
# But you cannot plot it as a time series dataset in the current form 
plot(ts1)
acfRes <- acf(ts1)
pacfRes <- pacf(ts1)

# converting the data into a time series dataset
t1 <- ts(ts1, start=c(2010), end=c(2020), frequency=20)

# in time series form you can plot and calculate the ACF, PACF etc
plot(t1)
acf(t1)
pacf(t1)

# ================= Analysing the dataset to find suitable models =================
# checking if the data resemble non-stationarity
tseries::adf.test(t1) # p-value = 0.01 (< 0.05) => Reject the null hypothesis of alpha < 1 => This is a non-stationary model

acf(t1) # only first value seems significant => MA(1) is a good model to try
pacf(t1) # first five terms seems significant => AR(5) is a good model to try

# since there are significant terms in both ACF and PACF, let's try the EACF
eacf(t1) # the most simple suitable models are MA(1), AR(6), ARMA(2,1)

# ================= Trying out the MA(1) model estimated using Maximum Likelikhood method=================
ma_1_model <- arima(t1, order=c(0,0,1), method="ML") #estimating the parameters with the Maximum Likelihood method
ma_1_model
coeftest(ma_1_model)

# residual analysis
res <- residuals(ma_1_model)
hist(res) # resembles a normal distribution
plot(rstandard(ma_1_model)) # 1) does not have values going over 3 & -3, 2) the variance seems to be the same across time, 3) value depicts independance across time
qqnorm(rstandard(ma_1_model)) # points lie on the 45 degree line => the residuals have the same distribution as the standard normal distribution
abline(a=0, b=1)
tsdiag(ma_1_model, gof=15, omit.initial=F) # all the p-value are greater the the critical value => the null hypothesis of residuals being normally distributed is correct
shapiro.test(rstandard(ma_1_model))

# ================= Trying out the AR(6) model estimated using Maximum Likelikhood method=================
ar_6_model <- arima(t1, order=c(6,0,0), method="ML")
ar_6_model
coeftest(ar_6_model)

# residual analysis
res_ar_6 <- residuals(ar_6_model)
hist(res_ar_6)
plot(rstandard(ar_6_model))
qqnorm(res_ar_6)
abline(a=0, b=1)
tsdiag(ar_6_model, gof=15, omit.initial=F)
shapiro.test(rstandard(ar_6_model))
