# --------------------------------------------------------------------------------------------------
library(vars)

# --------------------------------------------------------------------------------------------------
check_linearity <- function(v_train)
  {
  # Check if linear trend and remove it

  autoplot(decompose(v_train[,power]))
  #d1 <- diff(v_train[,power], difference=1, lag=96) # remove degree 1 trend
  d1 <- diff(v_train[,power], lag=96) # remove degree 1 trend
  ggtsdisplay(d1) # still trend. Remove it:
  d2 <- diff(d1, lag=1) 
  ggtsdisplay(d2)

  # White noise test
  bt <- Box.test(d2, lag=10, type='Ljung-Box')
  print(bt)
  # p-value < 2.2e-16 -> < 0.05: not white noise
  # We can clearly see the seasonal pattern of season 96 with very high ACF/PACF
  }

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
holt_winters <- function(v_train)
  {
  fit=hw(v_train[,power], seasonal='additive') #, lambda="auto")
  checkresiduals(fit)

#  prevHW=forecast(fit,h=18)
#  autoplot(prevHW) + autolayer(serie_test, series="true data")+
#  autolayer(prevHW$mean, series="HW forecasts")
  }

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
