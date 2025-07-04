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
index_start=1 ; xmin=2340 ; xmax=2343 ; filename="output_final"
pdf(file=paste(filename,".pdf", sep="")) 

# -------------------------------------------------------------------------------------------------- 
# -------------------------------------------------------------------------------------------------- 
# n_lines = 32061 - 2 headers -> 32 059 observations
data = read_excel("../Elec_30_11_train.xlsx", skip=1, col_types=c("text","numeric","numeric","numeric")) 
data$Timestamp <- as.POSIXct(data$Timestamp, format="%m/%d/%Y %H:%M", tz="UTC")

oat   <- "OAT (F)"
power <- "Power (kW)"
nrow  <- nrow(data)

summary(data)
head(data)

cat("std oat  :", sd(as.numeric(unlist(data[1:nrow,oat])), na.rm=TRUE), "\n")
cat("std power:", sd(as.numeric(unlist(data[1:nrow,power])), na.rm=TRUE), "\n")

# Three seasons : 9 hours, 15 hours and 1 day 
#data$Timestamp[1+36] - data$Timestamp[1]
#data$Timestamp[1+60] - data$Timestamp[1]
#data$Timestamp[1+96] - data$Timestamp[1]

# --------------------------------------------------------------------------------------------------
# Check how many days in the serie :
minT = min(data$Timestamp)
maxT = max(data$Timestamp)

# --------------------------------------------------------------------------------------------------
# Check that the dates have same step between every two consecutive entries:
plot_text("Check that the dates have same step\nbetween every two consecutive entries")
steps = rep(1,length(data$Timestamp)-1)
for (i in 1:length(data$Timestamp)-1)
  steps[i] = data$Timestamp[i+1]-data$Timestamp[i]
hist(steps, freq=FALSE)

# --------------------------------------------------------------------------------------------------
# Check if "Temp (C)" is the same as "OAT (F)"
myCelsiusDiff = (data[,oat] - 32)/1.8 - data[,4]
summary(myCelsiusDiff) # difference is null -> It is the same data. No need to take temperature in degrees in working dataset

# --------------------------------------------------------------------------------------------------
#v <- xts(x=data[,2:3], order.by=data$Timestamp, frequency=96)
#attr(v, 'frequency') <- 96 # Set the frequency of the xts object to day
z <- zoo(data[,2:3], data$Timestamp)

#summary(z)

plot_text("Dataset")
autoplot(z) + ggtitle("Elec_30_11_train") 
autoplot(z, facet=NULL) + ggtitle("Elec_30_11_train") 

# --------------------------------------------------------------------------------------------------
# Remove two ouliers where power lower than 130:
meani = mean(as.numeric(unlist(data[1:nrow,power])), na.rm=TRUE)
for (index in which(z[,power]<120))
  {
  z[index,power] = meani
  data[index,power] = meani
  }
autoplot(z, facet=NULL) + ggtitle("Elec_30_11_train - No outliers") 

# --------------------------------------------------------------------------------------------------
# Search for a season, plot two days :
t1 <- as.numeric(as.POSIXct("2010-02-01 00:00:00 UTC"))
t2 <- as.numeric(as.POSIXct("2010-02-02 23:59:59 UTC"))
p <- plot(z, xlim=c(t1, t2), main="Search for periodicity : plot two days")  
# periodicity of one day 

# --------------------------------------------------------------------------------------------------
# Missing data
# 96 missing data only for the last day that needs to be forcasted ?
sum(is.na(z[nrow-96, power])) # = 0 -> No missing data elsewhere...

# --------------------------------------------------------------------------------------------------
# Back from zoo to ts because many options are not available in zoo:
# Time in time serie is nothing but true time can be converted from column "Timestamp" using:
# as.POSIXct(1262308500, format="%m/%d/%Y %H:%M", tz="UTC")

# Split in train/test
#v <- ts(data[,1:3], start=c(2010,1), frequency=96)
v <- ts(data[index_start:nrow,1:3], start=c(2010,1), frequency=96)
v_start <- start(v)
v_end   <- end(v)

#FIXME : take only one perid to compare to test
#FIXME : make second prediction using all v and forecast on unknown power

# remove 2 last periods for training set:
v_train <- window(v, start=v_start                     , end=c(v_end[1],v_end[2]-2*96)) 
# start after 2 to last period for testing set:
v_test  <- window(v, start=c(v_end[1], v_end[2]-2*96+1), end=v_end)

autoplot(v_train[,oat], series="OAT, train") + autolayer(v_test[,oat], series="OAT, test") + autolayer(v_train[,power], series="Power, train")  + autolayer(v_test[,power], series="Power, test") + xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(20,350))

# --------------------------------------------------------------------------------------------------
# check correlation
cc.test(v_train[,oat], v_train[,power], max.lag=10, plot=FALSE) 
# p-values always 0.000<0.05 -> reject H0 -> no zero correlation -> high correlation

# --------------------------------------------------------------------------------------------------
# There is a seasonal pattern:
acf(v_train[,power], type="cor", lag=200, plot=TRUE)

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
# --- MODELLING ------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
# - SARIMA
plot_text("SARIMA\n\n1. Without covariates\n2. With covariates\n")

# without covariates:
fc_sarima_nocov <- do_sarima(v_train, v_test, covariates=FALSE, auto=FALSE)
plot(fc_sarima_nocov, xlim=c(xmin,xmax))

# with covariates:
fc_sarima_cov   <- do_sarima(v_train, v_test, covariates=TRUE , auto=FALSE)
plot(fc_sarima_cov, xlim=c(xmin,xmax))

autoplot(v_train[,power]) + autolayer(v_test[,power]      , series="Test set") +
  autolayer(fc_sarima_nocov$mean, series='Without covariates', PI=FALSE) +
  autolayer(fc_sarima_cov$mean  , series='With covariates'   , PI=FALSE) +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# --------------------------------------------------------------------------------------------------
# - Neural Network
plot_text("NEURAL NETWORK\n\n1. Without covariates\n2. With covariates\n")

# without covariates:
fc_nn_nocov <- do_NNet(v_train, v_test, covariates=FALSE)
plot(fc_nn_nocov, xlim=c(xmin,xmax))

# with covariates:
fc_nn_cov <- do_NNet(v_train, v_test, covariates=TRUE)
plot(fc_nn_cov, xlim=c(xmin,xmax))

autoplot(v_train[,power]) + autolayer(v_test[,power], series="Test set") +
  autolayer(fc_nn_nocov   , series="Neural Network without covariates") +
  autolayer(fc_nn_cov     , series="Neural Network with covariates") +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# - XGBoost
plot_text("XGBoost")
fc_xgboost <- do_xgboost(v_train, v_test, covariates=FALSE)
# FIXME prediction too long. why ??
ts_xgboost = ts(fc_xgboost, start=start(v_test), end=end(v_test), frequency=96)
plot(fc_xgboost, xlim=c(xmin,xmax))

autoplot(v_train[,power]) + autolayer(v_test[,power], series="Test set") +
  autolayer(fc_nn_nocov   , series="Neural Network without covariates") +
  autolayer(fc_nn_cov     , series="Neural Network with covariates") +
  autolayer(ts_xgboost    , series="XGBoost") +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# --------------------------------------------------------------------------------------------------
autoplot(v_train[,power]) + autolayer(v_test[,power]        , series="Test set") +
  autolayer(fc_sarima_cov$mean    , series='SARIMA'         , PI=FALSE) +
  autolayer(fc_nn_cov             , series='Neural Network' , PI=FALSE) +
  autolayer(ts_xgboost            , series="XGBoost") +
  xlim(c(xmin+0.5, xmax-0.5)) + ylim(c(100,350))

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
save.image(file=paste(filename,".RData", sep=""))

