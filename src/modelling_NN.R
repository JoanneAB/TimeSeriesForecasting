# --------------------------------------------------------------------------------------------------
#install.packages("MTS")

library(vars)
library(randomForest)
library(MTS)

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
do_randomForest <- function(v_train, v_test)
  {
  # We based our forecast on the 96 previous observations, but that can be optimized (by CV)
  nrow = nrow(v_train)
  data = as.vector(v_train[,power])[1:(nrow-96)]
  for (i in 1:(nrow-96))
    {
    data = rbind(data, as.vector(v_train[,power])[(i+1):(i+96)])
    }

  fit = randomForest(x=data[,-96], y=data[,96])
  prev = predict(fit, newdata=v_test[,power])

#  fit = randomForest(v_train[,oat], v_train[,power])
#  checkresiduals(fit)
#  tsdisplay(fit)
#
#  prev = predict(fit, newdata=v_test[,power])
  return(prev)
  }

# --------------------------------------------------------------------------------------------------
do_NNet <- function(v_train, covariates)
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
    return(prev)
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
    return(prev)
    }
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

