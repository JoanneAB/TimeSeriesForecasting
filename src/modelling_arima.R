# --------------------------------------------------------------------------------------------------
#install.packages("MTS")

library(vars)
library(MTS)

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
do_arima <- function(v_train, v_test, covariates, auto)
  {
  # AUTO-ARIMA
  if (auto)
    {
    start.time <- Sys.time()
    fit_auto <- auto.arima(v_train[,power], xreg=v_train[,oat])
    # (4,0,0)(0,1,0)[96] -> p-value < 2.2e-16 :(
    checkresiduals(fit_auto)
    tsdisplay(fit_auto$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n")

    fc <- forecast(fit_auto, xreg=v_train[,oat], h=96)
    return(fc)
    }

  # --------------------------------------------------------------
  if ( covariates )
    {
    # ARIMA with covariates
    start.time <- Sys.time()
    fit <- Arima(v_train[,power], xreg=v_train[,oat], order=c(2,1,3))
    # when adding covariates, not improved results. 

    checkresiduals(fit) # -> p-value << 0.05 ! BAD 
    tsdisplay(fit$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n") # -> 6 sec

    fc <- forecast(fit, xreg=v_train[,oat], h=96)

    rmse = calc_rmse(v_test[,power], fc$mean)
    cat("RMSE arima (with covariates) :", rmse, "\n")
    }
  else
    {
    # ARIMA without covariates
    start.time <- Sys.time()
    fit <- Arima(v_train[,power], order=c(2,1,3))
    checkresiduals(fit) # -> p-value << 0.5
    tsdisplay(fit$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n") # -> 4 sec
    # FIXME : should I take into account the two additional seasons ?
    #acf(fit$residuals, lag=100, xlim=c(0.00,1.00), ylim=c(-0.2,0.2))
    # check the periods using: data$Timestamp[1+x] - data$Timestamp[x]: 
    # negative : 32.5   ( 8h 15min), 35.5 ( 9h  0min), ++ 37.5 ( 9h 30min) 
    #            62.5   (15h 45min), 63.5 (16h  0min),    64.5 (16h 15min)
    # positive : 45.5 ++(11h 30min), 50.0 (12h 30min),    54.1 (13h 30min)
    # Only in 1st period of PACF but 3 periods on ACF !!

    fc <- forecast(fit, h=96)

    rmse = calc_rmse(v_test[,power], fc$mean)
    cat("RMSE arima (without covariates) :", rmse, "\n")
    }
  return(fc)
  }


# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
# SARIMA (with seasonality)
do_sarima <- function(v_train, v_test, covariates, auto)
  {
  if ( auto )
    {
    start.time <- Sys.time()
    fit_auto <- auto.arima(v_train[,power], xreg=v_train[,oat])
    checkresiduals(fit_auto)
    tsdisplay(fit_auto$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n")

    fc <- forecast(fit_auto, xreg=v_train[,oat], h=192)

    rmse = calc_rmse(v_test[,power], fc$mean)
    cat("RMSE sarima (with covariates, auto) :", rmse, "\n")
    }

  if ( covariates )
    {
    start.time <- Sys.time()
    fit = Arima(v_train[,power], xreg=v_train[,oat], order=c(1,0,3), seasonal=c(1,1,1))
    #fit = Arima(v_train[,power], xreg=v_train[,oat], order=c(1,0,0), seasonal=c(0,0,2))
    checkresiduals(fit)
    tsdisplay(fit$residuals)
    # still high PACF correlations at periods 288 -> seasonal ( , ,3)
    # still ACF linear decrease -> order (1, , ), seasonal (1, , )
    # PACF seasonal pattern of period 96 (expo decrease) -> seasonal ( ,1, )

    # Q* = 13308, df = 190, p-value < 2.2e-16
    # Model df: 2.   Total lags used: 192  

    # ARIMA(1,0,3)(1,1,1)[96]
    # p-value = 2.3e-7 < 0.05 --> KO for data[25000:nrow] - timing : 19.80 min
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n")

    fc <- forecast(fit, xreg=v_train[,oat], h=192)

    rmse = calc_rmse(v_test[,power], fc$mean)
    cat("RMSE SARIMA (with covariates) :", rmse, "\n")
    }
  else
    {
    start.time <- Sys.time()
    fit <- Arima(v_train[,power], order=c(1,0,3), seasonal=c(1,1,1))
    #fit = Arima(v_train[,power], order=c(1,0,3), seasonal=c(0,1,2))
    # ARIMA(2,0,0)(1,1,1)[96] :
    # p-value = 0.472   > 0.05 --> OK for data[30000:nrow] - timing :  5.34 min
    # p-value = 2.2e-13 < 0.05 --> KO for data[25000:nrow] - timing : 10.59 min

    # ARIMA(1,0,3)(1,1,1)[96]
    # p-value = 2.2e-7 < 0.05 --> KO for data[25000:nrow] - timing : 12.48 min

    checkresiduals(fit)
    tsdisplay(fit$residuals)
    end.time <- Sys.time()
    cat("timing :", (end.time-start.time), "\n")

    fc <- forecast(fit, h=192)

    rmse = calc_rmse(v_test[,power], fc$mean)
    cat("RMSE SARIMA (without covariates) :", rmse, "\n")
    }

  return(fc)
  }  
  
  # --------------------------------------------------------------
  # --------------------------------------------------------------
  # --- FOURIER ---
# --------------------------------------------------------------------------------------------------
do_arima_fourier <- function(v_train, v_test)
  {
  for (K in c(4)) #,10))
    {
    # By increasing K in fourier : add high frequencies.
    start.time <- Sys.time()
    #fit_fourier <- auto.arima(v_train[,power], xreg=fourier(v_train[,power], 4)) #, order=c(2,1,3))
    fit_fourier <- Arima(v_train[,power], order=c(2,1,3), xreg=fourier(v_train[,power], K))

#    checkresiduals(fit_fourier)
    tsdisplay(fit_fourier$residuals)
    end.time <- Sys.time()
    cat("timing fourier:", (end.time-start.time), "\n")

    fc_fourier <- forecast(fit_fourier, xreg=fourier(v_train[,power], K, 96), h=96)

    rmse = calc_rmse(v_test[,power], fc_fourier$mean)
    cat("RMSE fourier (single season) :", rmse, "\n")
    }
  return(fc_fourier)
  }

# --------------------------------------------------------------------------------------------------
do_arima_multi_fourier <- function(v_train, v_test)
  {
  # Plot to search for the seasons:
#  fit <- Arima(v_train[,power], order=c(2,1,3))
#  my_lag = 100
#  acf_plot = acf(fit$residuals, lag=my_lag,  plot=FALSE) #xlim=c(0.80,1.00), ylim=c(-0.2,0.2))
#  plot(c(0:my_lag), acf_plot$acf, type='l', xlim=c(35,40), ylim=c(-0.2,0.2))

  # --------------------------------------------------------------
  # Three seasons of 36 (9 hours), 60 (15 hours) and 96 (1 day):
  K = c(4,4,4) 
  # No need to increase to much K. Same RMSE when K=c(15,15,15) than for c(4,4,4)
  # Adds high frequencies.
  # FIXME : adding more frequencies does not help ? -> try much more to see

  v_train_msts <- msts(v_train, start=start(v_train), seasonal.periods=c(36,60,96)) #36,60,96))
#0.5
  start.time <- Sys.time()
  #fit_msts = auto.arima(v_train[,power], xreg=fourier(v_train_msts, K)) #, seasonal=F) 
  # seasonal=F to avoid max lag of 350... 
  # found (5,1,1) or (3,1,1)(0,0,1), same RMSE

  fit_msts <- Arima(v_train_msts[,power], order=c(2,1,3), xreg=fourier(v_train_msts, K)) 
  end.time <- Sys.time()
  cat("timing multi:", (end.time-start.time), "\n")

  #checkresiduals(fit_msts)
  tsdisplay(fit_msts$residuals)
  fc_msts <- forecast(fit_msts, xreg=fourier(v_train_msts, K, 96), h=96)

  rmse = calc_rmse(v_test[,power], fc_msts$mean)
  cat("RMSE fourier (multi seasons) :", rmse, "\n")
  return(fc_msts)
  }

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
