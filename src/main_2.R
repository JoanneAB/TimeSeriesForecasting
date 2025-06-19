# --------------------------------------------------------------------------------------------------
#install.packages("readxl") # CRAN version

library(zoo)
library(forecast)
library(ggplot2)
library(readxl)
library(testcorr)

options(warn = -1) # Remove warnings

# --------------------------------------------------------------------------------------------------
pdf(file="output.pdf")

# n_lines = 32061 - 2 headers -> 32 059 observations

#data = read_excel("../Elec_30_11_train.xlsx", skip=2, col_names=c("timestamp", "oat", "power", "temp"), col_types=c("text","numeric","numeric","numeric")) 

data = read_excel("../Elec_30_11_train.xlsx", skip=1, col_types=c("text","numeric","numeric","numeric")) 
data$Timestamp <- as.POSIXct(data$Timestamp, format="%m/%d/%Y %H:%M", tz="UTC")

#library(tsibble)
#library(feasts)
#
#tsbl <- as_tsibble(data[,1:3]) #, key=origin, index=time_hour)
#
#autoplot(tsbl) + ggtitle("Elec_30_11_train") 
#
#decompose(tsbl)
#head(tsbl)
