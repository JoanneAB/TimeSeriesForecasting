# --------------------------------------------------------------------------------------------------
#install.packages("readxl") # CRAN version

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

pdf(file="outputs/output_NN_1_final.pdf")

data = read_excel("../Elec_30_11_train.xlsx", skip=1, col_types=c("text","numeric","numeric","numeric")) 
data$Timestamp <- as.POSIXct(data$Timestamp, format="%m/%d/%Y %H:%M", tz="UTC")

# --------------------------------------------------------------------------------------------------
oat   <- "OAT (F)"
power <- "Power (kW)"
nrow  <- nrow(data)

# --------------------------------------------------------------------------------------------------
# Split in train/test
v <- ts(data[1:nrow,1:3], start=c(2010,1), frequency=96)
v_start <- start(v)
v_end   <- end(v)

# --------------------------------------------------------------------------------------------------
# remove 2 last periods for training set:
v_train <- window(v, start=v_start                     , end=c(v_end[1],v_end[2]-2*96)) 
# start after 2 to last period for testing set:
v_test  <- window(v, start=c(v_end[1], v_end[2]-2*96+1), end=v_end)

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
xmin=2340
xmax=2344

# - Neural Network
plot_text("NEURAL NETWORK\n\n1. Without covariates\n2. With covariates\n")

# without covariates:
fc_nn_nocov_new <- do_NNet(v_train, v_test, covariates=FALSE)
plot(fc_nn_nocov_new, xlim=c(xmin,xmax))

# with covariates:
fc_nn_cov_new <- do_NNet(v_train, v_test, covariates=TRUE)
plot(fc_nn_cov_new, xlim=c(xmin,xmax))

autoplot(v_train[,power]) +
  autolayer(v_test[,power], series="Test set") +
  autolayer(fc_nn_nocov_new   , series="Neural Network without covariates") +
  autolayer(fc_nn_cov_new     , series="Neural Network with covariates") +
  xlim(c(xmin, xmax)) + ylim(c(100,350))

save.image(file="NN.RData")


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

load("outputs/output_all_1_final_192.RData")


# --------------------------------------------------------------------------------------------------
autoplot(v_train[,power]) +
  autolayer(v_test[,power]        , series="Test set") +
  autolayer(fc_sarima_cov$mean    , series='SARIMA'         , PI=FALSE) +
  autolayer(fc_nn_cov_new         , series='Neural Network' , PI=FALSE) +
  autolayer(ts_xgboost            , series="XGBoost") +
  xlim(c(xmin, xmax)) + ylim(c(100,350))



