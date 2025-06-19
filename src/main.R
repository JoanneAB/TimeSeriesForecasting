# --------------------------------------------------------------------------------------------------
#install.packages("readxl") # CRAN version

library(xts)
library(zoo)
library(forecast)
library(ggplot2)
library(readxl)
library(testcorr)

source("modelling_NN.R")
source("modelling_arima.R")
source("modelling_linear.R")
source("functions.R")

options(warn = -1) # Remove warnings

# --------------------------------------------------------------------------------------------------
pdf(file="output.pdf")

# n_lines = 32061 - 2 headers -> 32 059 observations

#data = read_excel("../Elec_30_11_train.xlsx", skip=2, col_names=c("timestamp", "oat", "power", "temp"), col_types=c("text","numeric","numeric","numeric")) 

data = read_excel("../Elec_30_11_train.xlsx", skip=1, col_types=c("text","numeric","numeric","numeric")) 
data$Timestamp <- as.POSIXct(data$Timestamp, format="%m/%d/%Y %H:%M", tz="UTC")

oat   <- "OAT (F)"
power <- "Power (kW)"
nrow  <- nrow(data)

summary(data)
head(data)

# Three seasons : 9 hours, 15 hours and 1 day 
#data$Timestamp[1+36] - data$Timestamp[1]
#data$Timestamp[1+60] - data$Timestamp[1]
#data$Timestamp[1+96] - data$Timestamp[1]

# --------------------------------------------------------------------------------------------------
# Check how many days in the serie :
minT = min(data$Timestamp)
maxT = max(data$Timestamp)
#print(maxT - minT)

# --------------------------------------------------------------------------------------------------
# Check that the dates have same step between every two consecutive entries:
#steps = rep(1,length(data$Timestamp)-1)
#for (i in 1:length(data$Timestamp)-1)
#  steps[i] = data$Timestamp[i+1]-data$Timestamp[i]
#hist(steps, freq=FALSE)

# --------------------------------------------------------------------------------------------------
# Check if "Temp (C)" is the same as "OAT (F)"
myCelsiusDiff = (data[,oat] - 32)/1.8 - data[,4]
#summary(myCelsiusDiff) # difference is null -> It is the same data. No need to take temperature in degrees in working dataset

# --------------------------------------------------------------------------------------------------
#v <- xts(x=data[,2:3], order.by=data$Timestamp, frequency=96)
#attr(v, 'frequency') <- 96 # Set the frequency of the xts object to day
z <- zoo(data[,2:3], data$Timestamp)

#summary(z)

autoplot(z) + ggtitle("Elec_30_11_train") 
autoplot(z, facet=NULL) + ggtitle("Elec_30_11_train") 

# Search for a season, plot two days :
t1 <- as.numeric(as.POSIXct("2010-02-01 00:00:00 UTC"))
t2 <- as.numeric(as.POSIXct("2010-02-02 23:59:59 UTC"))
p <- plot(z, xlim=c(t1, t2), main="Search for periodicity : plot two days")  
# periodicity of one day 

# --------------------------------------------------------------------------------------------------
# Missing data
# 96 missing data only for the last day that needs to be forcasted ?
#sum(is.na(z[nrow-96, power])) # = 0 -> No missing data elsewhere...

# --------------------------------------------------------------------------------------------------
# Back from zoo to ts because many options are not available in zoo:
# Time in time serie is nothing but true time can be converted from column "Timestamp" using:
# as.POSIXct(1262308500, format="%m/%d/%Y %H:%M", tz="UTC")

# Split in train/test
#v <- ts(data[,1:3], start=c(2010,1), frequency=96)
v <- ts(data[25000:nrow,1:3], start=c(2010,1), frequency=96); xmin = 2079 ; xmax = 2083
v_start <- start(v)
v_end   <- end(v)

# remove 2 last periods for training set:
v_train <- window(v, start=v_start                     , end=c(v_end[1],v_end[2]-2*96)) 
# start after 2 to last period for testing set:
v_test  <- window(v, start=c(v_end[1], v_end[2]-2*96+1), end=v_end)

autoplot(v_train[,oat], series="OAT, train") + autolayer(v_test[,oat], series="OAT, test") + autolayer(v_train[,power], series="Power, train")  + autolayer(v_test[,power], series="Power, test") #+ xlim(c(2340,2344)) + ylim(c(20,350))

# --------------------------------------------------------------------------------------------------
# check correlation
cc.test(v_train[,oat], v_train[,power], max.lag=10, plot=FALSE) 
# p-values always 0.000<0.05 -> reject H0 -> no zero correlation -> high correlation

# --------------------------------------------------------------------------------------------------
# There is a seasonal pattern:
#acf(v_train[,power], type="cor", lag=200, plot=TRUE)

# --------------------------------------------------------------------------------------------------
# --- MODELLING ------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
# Remove a linear trend by substraction of shifted curve:
check_linearity(v_train)

# --------------------------------------------------------------------------------------------------
# - Holt-Winters
# Frequency too high for this method.
#out_hw <- holt_winters(v_train)

# --------------------------------------------------------------------------------------------------
# - ARIMA, no seasonality
plot_text("ARIMA\n\n1. Without covariates\n2. With covariates\n")
fc_arima_nocov <- do_arima(v_train, v_test, covariates=FALSE, auto=FALSE)
plot(fc_arima_nocov, xlim=c(xmin,xmax))

fc_arima_cov   <- do_arima(v_train, v_test, covariates=TRUE , auto=FALSE)
plot(fc_arima_nocov, xlim=c(xmin,xmax))

autoplot(v_train[,power]) +
  autolayer(v_test[,power]             , series="test") +
  autolayer(fc_arima_nocov$mean, series="no cov", PI=FALSE) +
  autolayer(fc_arima_cov$mean  , series="cov", PI=FALSE) +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# --------------------------------------------------------------------------------------------------
# - SARIMA
plot_text("SARIMA\n\n1. Without covariates\n2. With covariates\n")
fc_sarima_nocov <- do_sarima(v_train, v_test, covariates=FALSE, auto=FALSE)
plot(fc_sarima_nocov, xlim=c(xmin,xmax))

fc_sarima_cov   <- do_sarima(v_train, v_test, covariates=TRUE , auto=FALSE)
plot(fc_sarima_cov, xlim=c(xmin,xmax))

# Unique season fourier:
plot_text("Fourier\n\n1. Single season\n2. Multi seasons\n")
fc_sarima_fourier <- do_arima_fourier(v_train, v_test)
plot(fc_sarima_fourier, xlim=c(xmin,xmax))

# multi seasons fourier:
fc_sarima_multi_fourier <- do_arima_multi_fourier(v_train, v_test)
plot(fc_sarima_multi_fourier, xlim=c(xmin,xmax))

autoplot(v_train[,power]) +
  autolayer(v_test[,power]              , series="test") +
  autolayer(fc_sarima_nocov$mean, series='no cov', PI=FALSE) +
  autolayer(fc_sarima_cov$mean  , series='cov'   , PI=FALSE) +
  autolayer(fc_sarima_fourier$mean      , series='fourier', PI=FALSE) +
  autolayer(fc_sarima_multi_fourier$mean, series="multi"  , PI=FALSE) +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# --------------------------------------------------------------------------------------------------
# - Random Forest
#plot_text("Random Forest")
#fc_rf <- do_randomForest(v_train, v_test)

# - Neural Network
plot_text("NEURAL NETWORK\n\n1. Without covariates\n2. With covariates\n")
#plot_text("Coucou", y=0.5, newpage=FALSE)

# without covariates:
fc_nn_nocov <- do_NNet(v_train, v_test, covariates=FALSE)
plot(fc_nn_nocov, xlim=c(xmin,xmax))

# with covariates:
fc_nn_cov <- do_NNet(v_train, v_test, covariates=TRUE)
plot(fc_nn_cov, xlim=c(xmin,xmax))

# - XGBoost
plot_text("XGBoost")
fc_xgboost <- do_xgboost(v_train, v_test, covariates=FALSE)
ts_xgboost = ts(fc_xgboost, start=start(v_test), end=end(v_test), frequency=96)
plot(fc_xgboost, xlim=c(xmin,xmax))

autoplot(v_train[,power]) +
  autolayer(v_test[,power]              , series="test") +
  autolayer(fc_nn_nocov, series="NN no cov") +
  autolayer(fc_nn_cov, series="NN cov") +
#  autolayer(fc_rf, series="RandomForest") +
  autolayer(ts_xgboost, series="XGBoost no cov") +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
