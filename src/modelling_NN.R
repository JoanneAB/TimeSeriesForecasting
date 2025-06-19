# --------------------------------------------------------------------------------------------------
#install.packages("xgboost")

library(vars)
library(randomForest)
library(MTS)
library(xgboost)

# --------------------------------------------------------------------------------------------------
do_xgboost <- function(v_train, v_test, covariates=FALSE)
  {
  if (covariates)
    {
    # TODO : add covariates
    }
  else
    {
    start.time <- Sys.time()
    data = as.vector(v_train[,power])[1:97]
    for (i in 1:(length(as.vector(v_train[,power]))-97) )
      {
      data = rbind(data, as.vector(v_train[,power])[(i+1):(i+97)])
      }

    model <- xgboost(data=data[,1:96], label=data[,97],
      max_depth=10, eta=0.1, nrounds=100, nthread=4, objective="reg:squarederror",
      verbose=FALSE)

    # rmse=19.72 ; AIC=12 659

    pred    = rep(NULL, 192)
    newdata = tail(v_train[,power],96)
    for (t in 1:192)
      {
      pred[t] = predict(model, matrix(newdata,1,96))
      newdata = c(newdata[-1],pred[t])
      }

    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n") # 1min 47
    ts_xgboost = ts(pred, start=start(v_test), end=end(v_test), frequency=96)

    rmse = calc_rmse(ts_xgboost, v_test[,power])
    cat("RMSE XGBoost (without covariates) :", rmse, "\n")

    #aic_xgboost  = calc_aic(v_train[,power], v_test[,power], ts_xgboost)
    #cat("AIC  XGBoost no cov:", aic_xgboost, "\n")
    }
  return(pred)
  }

# --------------------------------------------------------------------------------------------------
do_randomForest <- function(v_train, v_test)
  {
  # Forecast based on the 96 previous observations.
  start.time <- Sys.time()
  nrow = nrow(v_train)
  data = as.vector(v_train[,power])[1:(nrow-96)]
  for (i in 1:(nrow-96))
    {
    data = rbind(data, as.vector(v_train[,power])[(i+1):(i+96)])
    }

#  fit = randomForest(v_train[,oat], v_train[,power])
  fit = randomForest(x=data[,-96], y=data[,96])
  checkresiduals(fit)
  tsdisplay(fit)

  end.time <- Sys.time()
  cat("timing :", (end.time-start.time), "\n") # 1min 47

  prev = predict(fit, newdata=v_test[,power])

  rmse = calc_rmse(prev$mean, v_test[,power])
  cat("RMSE Random Forest (without covariates) :", rmse, "\n")

  return(prev)
  }

# --------------------------------------------------------------------------------------------------
do_NNet <- function(v_train, v_test, covariates=FALSE)
  {
  if ( covariates )
    {
    start.time <- Sys.time()
    fit  = nnetar(v_train[,power], xreg=v_train[,oat], p=4, P=2)

    # From grid-search, with no covariate, best model for p=4, P=2 -> rmse =  
    # timing = 23

    checkresiduals(fit)
    tsdisplay(fit$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n") # 1min 47

    prev = forecast(fit, xreg=v_train[,oat], h=96) 

    rmse = calc_rmse(prev$mean, v_test[,power])
    cat("RMSE NN (with covariates) :", rmse, "\n")
    }
  else
    {
    start.time <- Sys.time()
    fit  = nnetar(v_train[,power], p=7, P=3)

    # NNAR(28,1,16)[96]
    # p-value < 2.2e-16 -> KO
    # sigma^2 estimated at 101.6

    # From grid-search, with no covariate, best model for p=7, P=3 -> rmse = 10.45 
    # timing : 12 sec

    checkresiduals(fit)
    tsdisplay(fit$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n") # 1min 47

    prev = forecast(fit, h=96)  

    rmse = calc_rmse(prev$mean, v_test[,power])
    cat("RMSE NN (without covariates) :", rmse, "\n")
    }
  return(prev)
  }

# --------------------------------------------------------------------------------------------------
# --- Do grid-search on hyper-parameters of the neural network to search for best set of parameters
# NNAR(p,P,k)[m] <-> ARIMA(p,0,0)(P,0,0)[m]

do_NNet_gridSearch <- function(v_train, v_test)
  {
  for ( p in 1:10 ) # number of non-seasonal lag
    {
    for ( P in 1:5 ) # number of seasonal lag
      {
      for ( k in (floor((p+P+1)/2)-3):(floor((p+P+1)/2)+3) ) # number of hidden node
        {
        #fit  = nnetar(v_train[,power], p=p, P=P, k=k)
        fit  = nnetar(v_train[,power], xreg=v_train[,oat], p=p, P=P, k=k)

        #prev = forecast(fit, h=96)
        prev = forecast(fit, xreg=v_train[,oat], h=96)

        #cat('p:',p,', P:',P,', k:',k,', RMSE:', calc_rmse(prev$mean, v_test[,power]),'\n')
        cat('p:',p,', P:',P, ', k:',k, ' RMSE:', calc_rmse(prev$mean[1:96], v_test[1:96,power]),'\n')
        # no cov      : p=7, P=3, k=8 - rmse =  9.27 
        # no cov no k : p=7, P=3      - rmse = 10.14

        #    cov      : p=4, P=2, k=0 - rmse = 10.90
        #    cov no k : p=4, P=2      - rmse = 11.12
        }
      }
    }
  }

