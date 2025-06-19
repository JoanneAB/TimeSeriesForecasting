# --------------------------------------------------------------------------------------------------
calc_rmse <- function(x, y)
  {
  # Compute RMSE from prediction
  return(sqrt(mean((x-y)^2, na.rm=TRUE)))
  }

# --------------------------------------------------------------------------------------------------
calc_aic <- function(v_train, v_test, v_pred)
  {
  # Calculate RMSE:
  rmse = calc_rmse(v_test, v_pred)
  
  # Get number of features:
  n_features = length(v_train)
    
  n = length(v_test) 
  aic = 2*n_features - 2*n*log(rmse) 
  return(aic)
  }

# --------------------------------------------------------------------------------------------------
plot_text <- function(text, x=0.05, y=1, font=1, cex=1, col="#000000", newpage=TRUE)
  {
  # Write text into a new page of the pdf file:
  if ( newpage )
    {
    plot.new()
    }

  text(x=x, y=y, adj=c(0,1), font=font, cex=cex, col=col, labels=text)
  }
# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------
