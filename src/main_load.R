# --------------------------------------------------------------------------------------------------
#install.packages("readxl") # CRAN version

library(zoo)
library(forecast)
library(ggplot2)
library(readxl)
library(testcorr)

source("functions.R")

options(warn = -1) # Remove warnings

load("outputs/output_all_1_final_192.RData")

autoplot(v_train[,power]  , series="Train set") +
  autolayer(v_test[,power], series="Test set") + 
  autolayer(ts_xgboost    , series="XGBoost")  + xlim(c(xmin,xmax))

