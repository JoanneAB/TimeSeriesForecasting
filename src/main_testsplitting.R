# --------------------------------------------------------------------------------------------------
#install.packages("readxl") # CRAN version

library(xts)
library(zoo, quietly=TRUE)
library(forecast)
library(ggplot2)
library(readxl)
library(testcorr)

options(warn = -1) # Remove warnings

# --------------------------------------------------------------------------------------------------
pdf(file="output.pdf")

data = read_excel("../Elec_30_11_train.xlsx", skip=1, col_types=c("text","numeric","numeric","numeric")) 
data$Timestamp <- as.POSIXct(data$Timestamp, format="%m/%d/%Y %H:%M", tz="UTC")

# --------------------------------------------------------------------------------------------------
# Check how many days in the serie :
minT = min(data$Timestamp)
maxT = max(data$Timestamp)

# --------------------------------------------------------------------------------------------------
#v <- xts(x=data[,2:3], order.by=data$Timestamp, frequency=96)
#attr(v, 'frequency') <- 96 # Set the frequency of the xts object to day
z <- zoo(data[,2:3], data$Timestamp)
#v <- ts(data[,2:3], start=as.POSIXct(min(data$Timestamp)))
#v <- ts(data[,2:3], start=as.Date(min(data$Timestamp)), end=as.Date(max(data$Timestamp)), freq=1)
#v <- ts(data[,2:3], start=as.POSIXct(min(data$Timestamp)), end=as.POSIXct(max(data$Timestamp)))

#as.POSIXct(1262309426, format="%m/%d/%Y %H:%M", tz="UTC")
#as.POSIXct(1262309512, format="%m/%d/%Y %H:%M", tz="UTC")


# --------------------------------------------------------------------------------------------------
# - split in train/test
z_train = z[1:(nrow(z)-2*96),]
z_test  = z[(nrow(z)-96-95):nrow(z),]

t1 <- as.numeric(as.POSIXct("2010-11-28 00:00:00 UTC"))
t2 <- as.numeric(as.POSIXct("2010-11-30 23:45:00 UTC"))
par(mfrow=c(2,1))
plot(z_train[,1], ylab="OAT (F)", xlim=c(t1, t2), main="Train/Test sets") 
lines(z_test[,1], col=2)

plot(z_train[,2], ylab="Power (kW)", xlim=c(t1, t2)) 
lines(z_test[,2], col=2)
par(mfrow=c(1,1))

# --------------------------------------------------------------------------------------------------
#v_train <- ts(data[1:(nrow(z)-2*96),2:3]       , start=c(2010,1),  frequency=96)
#v_test  <- ts(data[(nrow(z)-96-95):nrow(z),2:3], start=2341.948 , frequency=96) 

v <- ts(data, start=c(2010,1), frequency=96)
endv   <- end(v)
startv <- start(v)
v_train <- window(v, start=startv               , end=c(endv[1],endv[2]-2*96))
v_test  <- window(v, start=c(endv[1], endv[2]-2*96+1), end=endv)

par(mfrow=c(2,1))
autoplot(v_train[,"OAT (F)"], series="OAT, train") + autolayer(v_test[,"OAT (F)"], series="OAT, test") + autolayer(v_train[,"Power (kW)"], series="Power, train")  + autolayer(v_test[,"Power (kW)"], series="Power, test") + xlim(c(2340,2344)) + ylim(c(20,350)) 






